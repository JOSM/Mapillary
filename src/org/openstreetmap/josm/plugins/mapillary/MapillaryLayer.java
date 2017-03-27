// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.AlphaComposite;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Composite;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.TexturePaint;
import java.awt.event.ActionEvent;
import java.awt.geom.Line2D;
import java.awt.geom.Path2D;
import java.awt.image.BufferedImage;
import java.util.Comparator;
import java.util.Optional;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.KeyStroke;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.data.osm.event.DataChangedEvent;
import org.openstreetmap.josm.data.osm.event.DataSetListenerAdapter;
import org.openstreetmap.josm.data.osm.visitor.BoundingXYVisitor;
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
import org.openstreetmap.josm.tools.ImageProvider.ImageSizes;

/**
 * This class represents the layer shown in JOSM. There can only exist one
 * instance of this object.
 *
 * @author nokutu
 */
public final class MapillaryLayer extends AbstractModifiableLayer implements
  ActiveLayerChangeListener, MapillaryDataListener {

  /** Maximum distance for the red/blue lines. */
  public static final int SEQUENCE_MAX_JUMP_DISTANCE = MapillaryProperties.SEQUENCE_MAX_JUMP_DISTANCE.get();

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
    new DataSetListenerAdapter((e) -> {
      if (e instanceof DataChangedEvent) {
        // When more data is downloaded, a delayed update is thrown, in order to
        // wait for the data bounds to be set.
        Main.worker.submit(new DelayedDownload());
      }
    });

  /** If the download is in semiautomatic during this object lifetime. */
  public boolean tempSemiautomatic;

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
    super(tr("Mapillary Images"));
    this.data = new MapillaryData();
    data.addListener(this);
  }

  /**
   * Initializes the Layer.
   */
  private void init() {
    if (Main.main != null && Main.isDisplayingMapView()) {
      setMode(new SelectMode());
      Main.getLayerManager().addLayer(this);
      Main.getLayerManager().addActiveLayerChangeListener(this);
      if (Main.getLayerManager().getEditLayer() != null) {
        Main.getLayerManager().getEditLayer().data.addDataSetListener(DATASET_LISTENER);
      }
      if (MapillaryDownloader.getMode() == DOWNLOAD_MODE.OSM_AREA) {
        MapillaryDownloader.downloadOSMArea();
      }
      if (MapillaryDownloader.getMode() == DOWNLOAD_MODE.VISIBLE_AREA) {
        this.mode.zoomChanged();
      }
    }
    // Does not execute when in headless mode
    if (MapillaryPlugin.getExportMenu() != null) {
      MapillaryPlugin.setMenuEnabled(MapillaryPlugin.getExportMenu(), true);
      if (!MapillaryMainDialog.getInstance().isShowing()) {
        MapillaryMainDialog.getInstance().getButton().doClick();
      }
    }
    createHatchTexture();
    if (Main.main != null) {
      MapillaryMainDialog.getInstance().mapillaryImageDisplay.repaint();
      MapillaryMainDialog.getInstance()
        .getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW)
        .put(KeyStroke.getKeyStroke("DELETE"), "MapillaryDel");
      MapillaryMainDialog.getInstance().getActionMap()
        .put("MapillaryDel", new DeleteImageAction());

      MapillaryData.dataUpdated();
      getLocationChangeset().addChangesetListener(MapillaryChangesetDialog.getInstance());
    }
  }

  /**
   * Changes the mode the the given one.
   *
   * @param mode The mode that is going to be activated.
   */
  public void setMode(AbstractMode mode) {
    if (this.mode != null) {
      Main.map.mapView.removeMouseListener(this.mode);
      Main.map.mapView.removeMouseMotionListener(this.mode);
      NavigatableComponent.removeZoomChangeListener(this.mode);
    }
    this.mode = mode;
    if (mode != null) {
      Main.map.mapView.setNewCursor(mode.cursor, this);
      Main.map.mapView.addMouseListener(mode);
      Main.map.mapView.addMouseMotionListener(mode);
      NavigatableComponent.addZoomChangeListener(mode);
      MapillaryUtils.updateHelpText();
    }
  }

  /**
   * Clears the unique instance of this class.
   */
  public static void clearInstance() {
    instance = null;
  }

  /**
   * Returns the unique instance of this class.
   *
   * @return The unique instance of this class.
   */
  public static synchronized MapillaryLayer getInstance() {
    if (instance == null) {
      instance = new MapillaryLayer();
      instance.init();
    }
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
   * @return the n-nearest image to the currently selected image
   */
  public MapillaryImage getNNearestImage(final int n) {
    return n >= 1 && n <= nearestImages.length ? nearestImages[n - 1] : null;
  }

  @Override
  public void destroy() {
    setMode(null);
    MapillaryRecord.getInstance().reset();
    AbstractMode.resetThread();
    MapillaryDownloader.stopAll();
    MapillaryMainDialog.getInstance().setImage(null);
    MapillaryMainDialog.getInstance().updateImage();
    MapillaryPlugin.setMenuEnabled(MapillaryPlugin.getExportMenu(), false);
    MapillaryPlugin.setMenuEnabled(MapillaryPlugin.getZoomMenu(), false);
    Main.map.mapView.removeMouseListener(this.mode);
    Main.map.mapView.removeMouseMotionListener(this.mode);
    Main.getLayerManager().removeActiveLayerChangeListener(this);
    if (Main.getLayerManager().getEditLayer() != null)
      Main.getLayerManager().getEditLayer().data.removeDataSetListener(DATASET_LISTENER);
    clearInstance();
    super.destroy();
  }

  @Override
  public boolean isModified() {
    return this.data.getImages().parallelStream().anyMatch(MapillaryAbstractImage::isModified);
  }

  @Override
  public void setVisible(boolean visible) {
    super.setVisible(visible);
    this.data.getImages().parallelStream().forEach(img -> img.setVisible(visible));
    if (Main.map != null) {
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
    if (Main.getLayerManager().getActiveLayer() == this) {
      // paint remainder
      g.setPaint(this.hatched);
      g.fill(MapViewGeometryUtil.getNonDownloadedArea(mv, this.data.getBounds()));
    }

    // Draw the blue and red line
    synchronized (nearestImages) {
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
      if (imageAbs.isVisible() && Main.map.mapView.contains(Main.map.mapView.getPoint(imageAbs.getMovingLatLon()))) {
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
      Main.warn("An image is not painted, because it is null or has no LatLon!");
      return;
    }
    final MapillaryAbstractImage selectedImg = getData().getSelectedImage();
    final Point p = Main.map.mapView.getPoint(img.getMovingLatLon());

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
    g.fillArc(p.x - CA_INDICATOR_RADIUS, p.y - CA_INDICATOR_RADIUS, 2 * CA_INDICATOR_RADIUS, 2 * CA_INDICATOR_RADIUS, (int) (90 - img.getMovingCa() - CA_INDICATOR_ANGLE / 2), CA_INDICATOR_ANGLE);
    // Paint image marker
    g.setColor(markerC);
    g.fillOval(p.x - IMG_MARKER_RADIUS, p.y - IMG_MARKER_RADIUS, 2 * IMG_MARKER_RADIUS, 2 * IMG_MARKER_RADIUS);

    // Paint highlight for selected or highlighted images
    if (img.equals(getData().getHighlightedImage()) || getData().getMultiSelectedImages().contains(img)) {
      g.setColor(Color.WHITE);
      g.setStroke(new BasicStroke(2));
      g.drawOval(p.x - IMG_MARKER_RADIUS, p.y - IMG_MARKER_RADIUS, 2 * IMG_MARKER_RADIUS, 2 * IMG_MARKER_RADIUS);
    }


    if (img instanceof MapillaryImage && !((MapillaryImage) img).getSigns().isEmpty()) {
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
    return tr("Mapillary layer") +
      '\n' +
      tr("Total images:") +
      ' ' +
      this.data.size() +
      '\n';
  }

  @Override
  public String getToolTipText() {
    return this.data.size() + (' ' + tr("images"));
  }

  @Override
  public void activeOrEditLayerChanged(ActiveLayerChangeEvent e) {
    if (Main.getLayerManager().getActiveLayer() == this) {
      MapillaryUtils.updateHelpText();
    }
    MapillaryPlugin.setMenuEnabled(MapillaryPlugin.getJoinMenu(), Main.getLayerManager().getActiveLayer() == this);

    if (Main.getLayerManager().getEditLayer() != e.getPreviousEditLayer()) {
      if (Main.getLayerManager().getEditLayer() != null) {
        Main.getLayerManager().getEditLayer().data.addDataSetListener(DATASET_LISTENER);
      }
      if (e.getPreviousEditLayer() != null) {
        e.getPreviousEditLayer().data.removeDataSetListener(DATASET_LISTENER);
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
      .filter(seq -> target.getSequence() == null || (seq.getKey() != null && !seq.getKey().equals(target.getSequence().getKey())))
      .map(seq -> { // Maps sequence to image from sequence that is nearest to target
        Optional<MapillaryAbstractImage> resImg = seq.getImages().parallelStream()
          .filter((img) -> img instanceof MapillaryImage && img.isVisible())
          .sorted(new NearestImgToTargetComparator(target))
          .findFirst();
        return resImg.isPresent() ? resImg.get() : null;
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

  private void updateNearestImages() {
    final MapillaryAbstractImage selected = data.getSelectedImage();
    if (selected != null) {
      nearestImages = getNearestImagesFromDifferentSequences(selected, 2);
    } else {
      nearestImages = new MapillaryImage[0];
    }
    if (Main.main != null) {
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
   * Threads that runs a delayed Mapillary download.
   *
   * @author nokutu
   */
  private static class DelayedDownload extends Thread {

    @Override
    public void run() {
      try {
        sleep(1500);
      } catch (InterruptedException e) {
        Main.error(e);
      }
      MapillaryDownloader.downloadOSMArea();
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
