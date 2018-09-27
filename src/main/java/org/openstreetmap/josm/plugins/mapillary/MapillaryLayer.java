// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary;

import java.awt.AlphaComposite;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Composite;
import java.awt.Graphics2D;
import java.awt.GraphicsEnvironment;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.TexturePaint;
import java.awt.event.ActionEvent;
import java.awt.geom.Line2D;
import java.awt.geom.Path2D;
import java.awt.image.BufferedImage;
import java.util.Comparator;
import java.util.IntSummaryStatistics;
import java.util.Optional;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.KeyStroke;

import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.data.osm.DataSet;
import org.openstreetmap.josm.data.osm.event.DataChangedEvent;
import org.openstreetmap.josm.data.osm.event.DataSetListenerAdapter;
import org.openstreetmap.josm.data.osm.visitor.BoundingXYVisitor;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.MapView;
import org.openstreetmap.josm.gui.NavigatableComponent;
import org.openstreetmap.josm.gui.dialogs.LayerListDialog;
import org.openstreetmap.josm.gui.dialogs.LayerListPopup;
import org.openstreetmap.josm.gui.layer.AbstractModifiableLayer;
import org.openstreetmap.josm.gui.layer.Layer;
import org.openstreetmap.josm.gui.layer.MainLayerManager.ActiveLayerChangeEvent;
import org.openstreetmap.josm.gui.layer.MainLayerManager.ActiveLayerChangeListener;
import org.openstreetmap.josm.plugins.mapillary.cache.CacheUtils;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryChangesetDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryFilterDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryMainDialog;
import org.openstreetmap.josm.plugins.mapillary.history.MapillaryRecord;
import org.openstreetmap.josm.plugins.mapillary.history.commands.CommandDelete;
import org.openstreetmap.josm.plugins.mapillary.io.download.MapillaryDownloader;
import org.openstreetmap.josm.plugins.mapillary.io.download.MapillaryDownloader.DOWNLOAD_MODE;
import org.openstreetmap.josm.plugins.mapillary.mode.AbstractMode;
import org.openstreetmap.josm.plugins.mapillary.mode.JoinMode;
import org.openstreetmap.josm.plugins.mapillary.mode.SelectMode;
import org.openstreetmap.josm.plugins.mapillary.utils.MapViewGeometryUtil;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryColorScheme;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;
import org.openstreetmap.josm.tools.I18n;
import org.openstreetmap.josm.tools.ImageProvider.ImageSizes;
import org.openstreetmap.josm.tools.Logging;

/**
 * This class represents the layer shown in JOSM. There can only exist one
 * instance of this object.
 *
 * @author nokutu
 */
public final class MapillaryLayer extends AbstractModifiableLayer implements
  ActiveLayerChangeListener, MapillaryDataListener {

  /** The radius of the image marker */
  private static final int IMG_MARKER_RADIUS = 7;
  /** The radius of the circular sector that indicates the camera angle */
  private static final int CA_INDICATOR_RADIUS = 15;
  /** The angle of the circular sector that indicates the camera angle */
  private static final int CA_INDICATOR_ANGLE = 40;
  /** Length of the edge of the small sign, which indicates that traffic signs have been found in an image. */
  private static final int TRAFFIC_SIGN_SIZE = 6;
  /** A third of the height of the sign, for easier calculations */
  private static final double TRAFFIC_SIGN_HEIGHT_3RD = Math.sqrt(
    Math.pow(TRAFFIC_SIGN_SIZE, 2) - Math.pow(TRAFFIC_SIGN_SIZE / 2d, 2)
  ) / 3;

  private static final DataSetListenerAdapter DATASET_LISTENER =
    new DataSetListenerAdapter(e -> {
      if (e instanceof DataChangedEvent && MapillaryDownloader.getMode() == DOWNLOAD_MODE.OSM_AREA) {
        // When more data is downloaded, a delayed update is thrown, in order to
        // wait for the data bounds to be set.
        MainApplication.worker.execute(MapillaryDownloader::downloadOSMArea);
      }
    });

  /** Unique instance of the class. */
  private static MapillaryLayer instance;
  /** The nearest images to the selected image from different sequences sorted by distance from selection. */
  private MapillaryImage[] nearestImages = {};
  /** {@link MapillaryData} object that stores the database. */
  private final MapillaryData data;

  /** Mode of the layer. */
  public AbstractMode mode;

  private volatile TexturePaint hatched;
  private final MapillaryLocationChangeset locationChangeset = new MapillaryLocationChangeset();

  private MapillaryLayer() {
    super(I18n.tr("Mapillary Images"));
    this.data = new MapillaryData();
    data.addListener(this);
  }

  /**
   * Initializes the Layer.
   */
  private void init() {
    final DataSet ds = MainApplication.getLayerManager().getEditDataSet();
    if (ds != null) {
      ds.addDataSetListener(DATASET_LISTENER);
    }
    MainApplication.getLayerManager().addActiveLayerChangeListener(this);
    if (!GraphicsEnvironment.isHeadless()) {
      setMode(new SelectMode());
      if (MapillaryDownloader.getMode() == DOWNLOAD_MODE.OSM_AREA) {
        MainApplication.worker.execute(MapillaryDownloader::downloadOSMArea);
      }
      if (MapillaryDownloader.getMode() == DOWNLOAD_MODE.VISIBLE_AREA) {
        this.mode.zoomChanged();
      }
    }
    // Does not execute when in headless mode
    if (MainApplication.getMainFrame() != null && !MapillaryMainDialog.getInstance().isShowing()) {
      MapillaryMainDialog.getInstance().showDialog();
    }
    if (MapillaryPlugin.getMapView() != null) {
      MapillaryMainDialog.getInstance().mapillaryImageDisplay.repaint();
      MapillaryMainDialog.getInstance()
        .getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW)
        .put(KeyStroke.getKeyStroke("DELETE"), "MapillaryDel");
      MapillaryMainDialog.getInstance().getActionMap()
        .put("MapillaryDel", new DeleteImageAction());

      getLocationChangeset().addChangesetListener(MapillaryChangesetDialog.getInstance());
    }
    createHatchTexture();
    invalidate();
  }

  public static void invalidateInstance() {
    if (hasInstance()) {
      getInstance().invalidate();
    }
  }

  /**
   * Changes the mode the the given one.
   *
   * @param mode The mode that is going to be activated.
   */
  public void setMode(AbstractMode mode) {
    final MapView mv = MapillaryPlugin.getMapView();
    if (this.mode != null && mv != null) {
      mv.removeMouseListener(this.mode);
      mv.removeMouseMotionListener(this.mode);
      NavigatableComponent.removeZoomChangeListener(this.mode);
    }
    this.mode = mode;
    if (mode != null && mv != null) {
      mv.setNewCursor(mode.cursor, this);
      mv.addMouseListener(mode);
      mv.addMouseMotionListener(mode);
      NavigatableComponent.addZoomChangeListener(mode);
      MapillaryUtils.updateHelpText();
    }
  }

  private static synchronized void clearInstance() {
    instance = null;
  }

  /**
   * Returns the unique instance of this class.
   *
   * @return The unique instance of this class.
   */
  public static synchronized MapillaryLayer getInstance() {
    if (instance != null) {
      return instance;
    }
    final MapillaryLayer layer = new MapillaryLayer();
    layer.init();
    instance = layer; // Only set instance field after initialization is complete
    return instance;
  }

  /**
   * @return if the unique instance of this layer is currently instantiated
   */
  public static boolean hasInstance() {
    return instance != null;
  }

  /**
   * Returns the {@link MapillaryData} object, which acts as the database of the
   * Layer.
   *
   * @return The {@link MapillaryData} object that stores the database.
   */
  public MapillaryData getData() {
    return this.data;
  }

  /**
   * Returns the {@link MapillaryLocationChangeset} object, which acts as the database of the
   * Layer.
   *
   * @return The {@link MapillaryData} object that stores the database.
   */
  public MapillaryLocationChangeset getLocationChangeset() {
    return locationChangeset;
  }

  /**
   * Returns the n-nearest image, for n=1 the nearest one is returned, for n=2 the second nearest one and so on.
   * The "n-nearest image" is picked from the list of one image from every sequence that is nearest to the currently
   * selected image, excluding the sequence to which the selected image belongs.
   * @param n the index for picking from the list of "nearest images", beginning from 1
   * @return the n-nearest image to the currently selected image, or null if no such image can be found
   */
  public synchronized MapillaryImage getNNearestImage(final int n) {
    return n >= 1 && n <= nearestImages.length ? nearestImages[n - 1] : null;
  }

  @Override
  public synchronized void destroy() {
    clearInstance();
    setMode(null);
    MapillaryRecord.getInstance().reset();
    AbstractMode.resetThread();
    MapillaryDownloader.stopAll();
    if (MapillaryMainDialog.hasInstance()) {
      MapillaryMainDialog.getInstance().setImage(null);
      MapillaryMainDialog.getInstance().updateImage();
    }
    final MapView mv = MapillaryPlugin.getMapView();
    if (mv != null) {
      mv.removeMouseListener(this.mode);
      mv.removeMouseMotionListener(this.mode);
    }
    try {
      MainApplication.getLayerManager().removeActiveLayerChangeListener(this);
      if (MainApplication.getLayerManager().getEditDataSet() != null) {
        MainApplication.getLayerManager().getEditDataSet().removeDataSetListener(DATASET_LISTENER);
      }
    } catch (IllegalArgumentException e) {
      // TODO: It would be ideal, to fix this properly. But for the moment let's catch this, for when a listener has already been removed.
    }
    super.destroy();
  }

  @Override
  public boolean isModified() {
    return this.data.getImages().parallelStream().anyMatch(MapillaryAbstractImage::isModified);
  }

  @Override
  public void setVisible(boolean visible) {
    super.setVisible(visible);
    getData().getImages().parallelStream().forEach(img -> img.setVisible(visible));
    if (MainApplication.getMap() != null) {
      MapillaryFilterDialog.getInstance().refresh();
    }
  }

  /**
   * Initialize the hatch pattern used to paint the non-downloaded area.
   */
  private void createHatchTexture() {
    BufferedImage bi = new BufferedImage(15, 15, BufferedImage.TYPE_INT_ARGB);
    Graphics2D big = bi.createGraphics();
    big.setColor(MapillaryProperties.BACKGROUND.get());
    Composite comp = AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.3f);
    big.setComposite(comp);
    big.fillRect(0, 0, 15, 15);
    big.setColor(MapillaryProperties.OUTSIDE_DOWNLOADED_AREA.get());
    big.drawLine(0, 15, 15, 0);
    Rectangle r = new Rectangle(0, 0, 15, 15);
    this.hatched = new TexturePaint(bi, r);
  }

  @Override
  public synchronized void paint(final Graphics2D g, final MapView mv, final Bounds box) {
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
    if (MainApplication.getLayerManager().getActiveLayer() == this) {
      // paint remainder
      g.setPaint(this.hatched);
      g.fill(MapViewGeometryUtil.getNonDownloadedArea(mv, this.data.getBounds()));
    }

    // Draw the blue and red line
    synchronized (MapillaryLayer.class) {
      final MapillaryAbstractImage selectedImg = data.getSelectedImage();
      for (int i = 0; i < nearestImages.length && selectedImg != null; i++) {
        if (i == 0) {
          g.setColor(Color.RED);
        } else {
          g.setColor(Color.BLUE);
        }
        final Point selected = mv.getPoint(selectedImg.getMovingLatLon());
        final Point p = mv.getPoint(nearestImages[i].getMovingLatLon());
        g.draw(new Line2D.Double(p.getX(), p.getY(), selected.getX(), selected.getY()));
      }
    }

    // Draw sequence line
    g.setStroke(new BasicStroke(2));
    final MapillaryAbstractImage selectedImage = getData().getSelectedImage();
    for (MapillarySequence seq : getData().getSequences()) {
      if (seq.getImages().contains(selectedImage)) {
        g.setColor(
          seq.getKey() == null ? MapillaryColorScheme.SEQ_IMPORTED_SELECTED : MapillaryColorScheme.SEQ_SELECTED
        );
      } else {
        g.setColor(
          seq.getKey() == null ? MapillaryColorScheme.SEQ_IMPORTED_UNSELECTED : MapillaryColorScheme.SEQ_UNSELECTED
        );
      }
      g.draw(MapViewGeometryUtil.getSequencePath(mv, seq));
    }
    for (MapillaryAbstractImage imageAbs : this.data.getImages()) {
      if (imageAbs.isVisible() && mv != null && mv.contains(mv.getPoint(imageAbs.getMovingLatLon()))) {
        drawImageMarker(g, imageAbs);
      }
    }
    if (this.mode instanceof JoinMode) {
      this.mode.paint(g, mv, box);
    }
  }

  /**
   * Draws an image marker onto the given Graphics context.
   * @param g the Graphics context
   * @param img the image to be drawn onto the Graphics context
   */
  private void drawImageMarker(final Graphics2D g, final MapillaryAbstractImage img) {
    if (img == null || img.getLatLon() == null) {
      Logging.warn("An image is not painted, because it is null or has no LatLon!");
      return;
    }
    final MapillaryAbstractImage selectedImg = getData().getSelectedImage();
    final Point p = MainApplication.getMap().mapView.getPoint(img.getMovingLatLon());

    // Determine colors
    final Color markerC;
    final Color directionC;
    if (selectedImg != null && getData().getMultiSelectedImages().contains(img)) {
      markerC = img instanceof MapillaryImportedImage
        ? MapillaryColorScheme.SEQ_IMPORTED_HIGHLIGHTED
        : MapillaryColorScheme.SEQ_HIGHLIGHTED;
      directionC = img instanceof MapillaryImportedImage
        ? MapillaryColorScheme.SEQ_IMPORTED_HIGHLIGHTED_CA
        : MapillaryColorScheme.SEQ_HIGHLIGHTED_CA;
    } else if (selectedImg != null && selectedImg.getSequence() != null && selectedImg.getSequence().equals(img.getSequence())) {
      markerC = img instanceof MapillaryImportedImage
        ? MapillaryColorScheme.SEQ_IMPORTED_SELECTED
        : MapillaryColorScheme.SEQ_SELECTED;
      directionC = img instanceof MapillaryImportedImage
        ? MapillaryColorScheme.SEQ_IMPORTED_SELECTED_CA
        : MapillaryColorScheme.SEQ_SELECTED_CA;
    } else {
      markerC = img instanceof MapillaryImportedImage
        ? MapillaryColorScheme.SEQ_IMPORTED_UNSELECTED
        : MapillaryColorScheme.SEQ_UNSELECTED;
      directionC = img instanceof MapillaryImportedImage
        ? MapillaryColorScheme.SEQ_IMPORTED_UNSELECTED_CA
        : MapillaryColorScheme.SEQ_UNSELECTED_CA;
    }

    // Paint direction indicator
    g.setColor(directionC);
    if (img.isPanorama()) {
      g.fillOval(p.x - CA_INDICATOR_RADIUS, p.y - CA_INDICATOR_RADIUS, 2 * CA_INDICATOR_RADIUS, 2 * CA_INDICATOR_RADIUS);
    } else {
      g.fillArc(p.x - CA_INDICATOR_RADIUS, p.y - CA_INDICATOR_RADIUS, 2 * CA_INDICATOR_RADIUS, 2 * CA_INDICATOR_RADIUS, (int) (90 - img.getMovingCa() - CA_INDICATOR_ANGLE / 2d), CA_INDICATOR_ANGLE);
    }
    // Paint image marker
    g.setColor(markerC);
    g.fillOval(p.x - IMG_MARKER_RADIUS, p.y - IMG_MARKER_RADIUS, 2 * IMG_MARKER_RADIUS, 2 * IMG_MARKER_RADIUS);

    // Paint highlight for selected or highlighted images
    if (img.equals(getData().getHighlightedImage()) || getData().getMultiSelectedImages().contains(img)) {
      g.setColor(Color.WHITE);
      g.setStroke(new BasicStroke(2));
      g.drawOval(p.x - IMG_MARKER_RADIUS, p.y - IMG_MARKER_RADIUS, 2 * IMG_MARKER_RADIUS, 2 * IMG_MARKER_RADIUS);
    }


    if (img instanceof MapillaryImage && !((MapillaryImage) img).getDetections().isEmpty()) {
      Path2D trafficSign = new Path2D.Double();
      trafficSign.moveTo(p.getX() - TRAFFIC_SIGN_SIZE / 2d, p.getY() - TRAFFIC_SIGN_HEIGHT_3RD);
      trafficSign.lineTo(p.getX() + TRAFFIC_SIGN_SIZE / 2d, p.getY() - TRAFFIC_SIGN_HEIGHT_3RD);
      trafficSign.lineTo(p.getX(), p.getY() + 2 * TRAFFIC_SIGN_HEIGHT_3RD);
      trafficSign.closePath();
      g.setColor(Color.WHITE);
      g.fill(trafficSign);
      g.setStroke(new BasicStroke(1));
      g.setColor(Color.RED);
      g.draw(trafficSign);
    }
  }

  @Override
  public Icon getIcon() {
    return MapillaryPlugin.LOGO.setSize(ImageSizes.LAYER).get();
  }

  @Override
  public boolean isMergable(Layer other) {
    return false;
  }

  @Override
  public void mergeFrom(Layer from) {
    throw new UnsupportedOperationException(
      "This layer does not support merging yet");
  }

  @Override
  public Action[] getMenuEntries() {
    return new Action[]{
      LayerListDialog.getInstance().createShowHideLayerAction(),
      LayerListDialog.getInstance().createDeleteLayerAction(),
      new LayerListPopup.InfoAction(this)
    };
  }

  @Override
  public Object getInfoComponent() {
    IntSummaryStatistics seqSizeStats = getData().getSequences().stream().mapToInt(seq -> seq.getImages().size()).summaryStatistics();
    final long numImported = getData().getImages().stream().filter(i -> i instanceof MapillaryImportedImage).count();
    final long numDownloaded = getData().getImages().stream().filter(i -> i instanceof MapillaryImage).count();
    final int numTotal = getData().getImages().size();
    return new StringBuilder(I18n.tr("Mapillary layer"))
      .append('\n')
      .append(I18n.trn(
        "{0} sequence, containing between {1} and {2} images (ø {3})",
        "{0} sequences, each containing between {1} and {2} images (ø {3})",
        getData().getSequences().size(),
        getData().getSequences().size(),
        seqSizeStats.getCount() <= 0 ? 0 : seqSizeStats.getMin(),
        seqSizeStats.getCount() <= 0 ? 0 : seqSizeStats.getMax(),
        seqSizeStats.getAverage()
      ))
      .append("\n\n")
      .append(I18n.trn("{0} imported image", "{0} imported images", numImported, numImported))
      .append("\n+ ")
      .append(I18n.trn("{0} downloaded image", "{0} downloaded images", numDownloaded, numDownloaded))
      .append("\n= ")
      .append(I18n.trn("{0} image in total", "{0} images in total", numTotal, numTotal))
      .toString();
  }

  @Override
  public String getToolTipText() {
    return I18n.tr("{0} images in {1} sequences", getData().getImages().size(), getData().getSequences().size());
  }

  @Override
  public void activeOrEditLayerChanged(ActiveLayerChangeEvent e) {
    if (MainApplication.getLayerManager().getActiveLayer() == this) {
      MapillaryUtils.updateHelpText();
    }

    if (MainApplication.getLayerManager().getEditLayer() != e.getPreviousDataLayer()) {
      if (MainApplication.getLayerManager().getEditLayer() != null) {
        MainApplication.getLayerManager().getEditLayer().getDataSet().addDataSetListener(DATASET_LISTENER);
      }
      if (e.getPreviousDataLayer() != null) {
        e.getPreviousDataLayer().getDataSet().removeDataSetListener(DATASET_LISTENER);
      }
    }
  }

  @Override
  public void visitBoundingBox(BoundingXYVisitor v) {
  }

  /* (non-Javadoc)
   * @see org.openstreetmap.josm.plugins.mapillary.MapillaryDataListener#imagesAdded()
   */
  @Override
  public void imagesAdded() {
    updateNearestImages();
  }

  /* (non-Javadoc)
   * @see org.openstreetmap.josm.plugins.mapillary.MapillaryDataListener#selectedImageChanged(org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage, org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage)
   */
  @Override
  public void selectedImageChanged(MapillaryAbstractImage oldImage, MapillaryAbstractImage newImage) {
    updateNearestImages();
  }

  /**
   * Returns the closest images belonging to a different sequence and
   * different from the specified target image.
   *
   * @param target the image for which you want to find the nearest other images
   * @param limit the maximum length of the returned array
   * @return An array containing the closest images belonging to different sequences sorted by distance from target.
   */
  private MapillaryImage[] getNearestImagesFromDifferentSequences(MapillaryAbstractImage target, int limit) {
    return data.getSequences().parallelStream()
      .filter(seq -> seq.getKey() != null && !seq.getKey().equals(target.getSequence().getKey()))
      .map(seq -> { // Maps sequence to image from sequence that is nearest to target
        Optional<MapillaryAbstractImage> resImg = seq.getImages().parallelStream()
          .filter(img -> img instanceof MapillaryImage && img.isVisible())
          .min(new NearestImgToTargetComparator(target));
        return resImg.orElse(null);
      })
      .filter(img -> // Filters out images too far away from target
        img != null &&
        img.getMovingLatLon().greatCircleDistance(target.getMovingLatLon())
          < MapillaryProperties.SEQUENCE_MAX_JUMP_DISTANCE.get()
       )
      .sorted(new NearestImgToTargetComparator(target))
      .limit(limit)
      .toArray(MapillaryImage[]::new);
  }

  private synchronized void updateNearestImages() {
    final MapillaryAbstractImage selected = data.getSelectedImage();
    if (selected != null) {
      nearestImages = getNearestImagesFromDifferentSequences(selected, 2);
    } else {
      nearestImages = new MapillaryImage[0];
    }
    if (MainApplication.isDisplayingMapView()) {
      MapillaryMainDialog.getInstance().redButton.setEnabled(nearestImages.length >= 1);
      MapillaryMainDialog.getInstance().blueButton.setEnabled(nearestImages.length >= 2);
    }
    if (nearestImages.length >= 1) {
      CacheUtils.downloadPicture(nearestImages[0]);
      if (nearestImages.length >= 2) {
        CacheUtils.downloadPicture(nearestImages[1]);
      }
    }
  }

  /**
   * Action used to delete images.
   *
   * @author nokutu
   */
  private class DeleteImageAction extends AbstractAction {

    private static final long serialVersionUID = -982809854631863962L;

    @Override
    public void actionPerformed(ActionEvent e) {
      if (instance != null)
        MapillaryRecord.getInstance().addCommand(
          new CommandDelete(getData().getMultiSelectedImages()));
    }
  }

  private static class NearestImgToTargetComparator implements Comparator<MapillaryAbstractImage> {
    private final MapillaryAbstractImage target;

    public NearestImgToTargetComparator(MapillaryAbstractImage target) {
      this.target = target;
    }
    /* (non-Javadoc)
     * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
     */
    @Override
    public int compare(MapillaryAbstractImage img1, MapillaryAbstractImage img2) {
      return (int) Math.signum(
        img1.getMovingLatLon().greatCircleDistance(target.getMovingLatLon()) -
        img2.getMovingLatLon().greatCircleDistance(target.getMovingLatLon())
      );
    }
  }
}
