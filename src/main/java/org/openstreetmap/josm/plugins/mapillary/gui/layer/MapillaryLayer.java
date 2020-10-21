// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.layer;

import java.awt.AlphaComposite;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Composite;
import java.awt.Graphics2D;
import java.awt.GraphicsEnvironment;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.TexturePaint;
import java.awt.event.ActionEvent;
import java.awt.geom.AffineTransform;
import java.awt.geom.Line2D;
import java.awt.geom.Point2D;
import java.awt.image.BufferedImage;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.IntSummaryStatistics;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;

import org.openstreetmap.josm.actions.UploadAction;
import org.openstreetmap.josm.actions.upload.UploadHook;
import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.data.osm.BBox;
import org.openstreetmap.josm.data.osm.DataSet;
import org.openstreetmap.josm.data.osm.DataSourceChangeEvent;
import org.openstreetmap.josm.data.osm.DataSourceListener;
import org.openstreetmap.josm.data.osm.Node;
import org.openstreetmap.josm.data.osm.OsmPrimitive;
import org.openstreetmap.josm.data.osm.visitor.BoundingXYVisitor;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.MapView;
import org.openstreetmap.josm.gui.NavigatableComponent;
import org.openstreetmap.josm.gui.dialogs.LayerListDialog;
import org.openstreetmap.josm.gui.dialogs.LayerListPopup;
import org.openstreetmap.josm.gui.layer.AbstractModifiableLayer;
import org.openstreetmap.josm.gui.layer.Layer;
import org.openstreetmap.josm.gui.layer.LayerManager.LayerAddEvent;
import org.openstreetmap.josm.gui.layer.LayerManager.LayerChangeListener;
import org.openstreetmap.josm.gui.layer.LayerManager.LayerOrderChangeEvent;
import org.openstreetmap.josm.gui.layer.LayerManager.LayerRemoveEvent;
import org.openstreetmap.josm.gui.layer.MainLayerManager.ActiveLayerChangeEvent;
import org.openstreetmap.josm.gui.layer.MainLayerManager.ActiveLayerChangeListener;
import org.openstreetmap.josm.gui.layer.OsmDataLayer;
import org.openstreetmap.josm.gui.mappaint.Range;
import org.openstreetmap.josm.gui.mappaint.mapcss.Selector;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryData;
import org.openstreetmap.josm.plugins.mapillary.MapillaryDataListener;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImportedImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.MapillarySequence;
import org.openstreetmap.josm.plugins.mapillary.cache.CacheUtils;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryMainDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.changeset.MapillaryChangeset;
import org.openstreetmap.josm.plugins.mapillary.gui.dialog.MapillaryChangesetDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.dialog.MapillaryFilterDialog;
import org.openstreetmap.josm.plugins.mapillary.history.MapillaryRecord;
import org.openstreetmap.josm.plugins.mapillary.history.commands.CommandDelete;
import org.openstreetmap.josm.plugins.mapillary.io.download.MapillaryDownloader;
import org.openstreetmap.josm.plugins.mapillary.io.download.MapillaryDownloader.DOWNLOAD_MODE;
import org.openstreetmap.josm.plugins.mapillary.mode.AbstractMode;
import org.openstreetmap.josm.plugins.mapillary.mode.EditMode;
import org.openstreetmap.josm.plugins.mapillary.mode.JoinMode;
import org.openstreetmap.josm.plugins.mapillary.mode.SelectMode;
import org.openstreetmap.josm.plugins.mapillary.utils.MapViewGeometryUtil;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryColorScheme;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;
import org.openstreetmap.josm.spi.preferences.Config;
import org.openstreetmap.josm.tools.Geometry;
import org.openstreetmap.josm.tools.I18n;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.ImageProvider.ImageSizes;
import org.openstreetmap.josm.tools.Logging;

/**
 * This class represents the layer shown in JOSM. There can only exist one
 * instance of this object.
 *
 * @author nokutu
 */
public final class MapillaryLayer extends AbstractModifiableLayer implements
  ActiveLayerChangeListener, LayerChangeListener, MapillaryDataListener, UploadHook {

  /** The radius of the image marker */
  private static final int IMG_MARKER_RADIUS = 7;
  /** The radius of the circular sector that indicates the camera angle */
  private static final int CA_INDICATOR_RADIUS = 15;
  /** Length of the edge of the small sign, which indicates that traffic signs have been found in an image. */
  private static final int TRAFFIC_SIGN_SIZE = (int) (ImageProvider.ImageSizes.MAP.getAdjustedWidth() / 1.5);
  /** The range to paint the full detection image at */
  private static final Range IMAGE_CA_PAINT_RANGE = Selector.GeneralSelector.fromLevel(18, Integer.MAX_VALUE);

  /** The sprite to use to indicate that there are sign detections in the image */
  private static final Image YIELD_SIGN = new ImageProvider("josm-ca", "sign-detection")
    .setMaxSize(TRAFFIC_SIGN_SIZE).get()
    .getImage();

  private static class DataSetSourceListener implements DataSourceListener {
    @Override
    public void dataSourceChange(DataSourceChangeEvent event) {
      SwingUtilities.invokeLater(MapillaryDownloader::downloadOSMArea);
    }
  }

  private static final DataSourceListener DATASET_LISTENER = new DataSetSourceListener();

  /** Unique instance of the class. */
  private static MapillaryLayer instance;
  /** The nearest images to the selected image from different sequences sorted by distance from selection. */
  private MapillaryImage[] nearestImages = {};
  /** {@link MapillaryData} object that stores the database. */
  private final MapillaryData data;

  /** The images that have been viewed since the last upload */
  private final ConcurrentHashMap<DataSet, Set<MapillaryAbstractImage>> imageViewedMap = new ConcurrentHashMap<>();

  /** Mode of the layer. */
  public AbstractMode mode;

  private volatile TexturePaint hatched;
  private final MapillaryChangeset locationChangeset = new MapillaryChangeset();
  private final MapillaryChangeset deletionChangeset = new MapillaryChangeset();
  private boolean destroyed;
  private static AlphaComposite fadeComposite = AlphaComposite
    .getInstance(AlphaComposite.SRC_OVER, MapillaryProperties.UNSELECTED_OPACITY.get().floatValue());
  private static Point2D standardImageCentroid = null;

  private MapillaryLayer() {
    super(I18n.tr("Mapillary Images"));
    this.data = new MapillaryData();
    data.addListener(this);
    UploadAction.registerUploadHook(this, true);
  }

  /**
   * Initializes the Layer.
   */
  private void init() {
    final DataSet ds = MainApplication.getLayerManager().getEditDataSet();
    if (ds != null) {
      ds.addDataSourceListener(DATASET_LISTENER);
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
      MapillaryMainDialog.getInstance().imageViewer.repaint();
      MapillaryMainDialog.getInstance()
        .getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW)
        .put(KeyStroke.getKeyStroke("DELETE"), "MapillaryDel");
      MapillaryMainDialog.getInstance().getActionMap()
        .put("MapillaryDel", new DeleteImageAction());

      getLocationChangeset().addChangesetListener(MapillaryChangesetDialog.getInstance());
      getDeletionChangeset().addChangesetListener(MapillaryChangesetDialog.getInstance());
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
   * Changes the mode to the given one.
   *
   * @param mode The mode that is going to be activated.
   */
  public void setMode(AbstractMode mode) {
    final MapView mv = MapillaryPlugin.getMapView();
    if (this.mode != null && mv != null) {
      mv.removeMouseListener(this.mode);
      mv.removeMouseMotionListener(this.mode);
      try {
        this.mode.exitMode();
      } catch (IllegalArgumentException e) {
        if (!e.getMessage().contains("was not registered before or already removed")) {
          throw e;
        }
      }
      NavigatableComponent.removeZoomChangeListener(this.mode);
    }
    this.mode = mode;
    if (mode != null && mv != null) {
      mode.enterMode();
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
  // @Override -- depends upon JOSM 16548+
  @Override
  public MapillaryData getData() {
    return this.data;
  }

  /**
   * Returns the {@link MapillaryChangeset} object, which acts as the list of changed images.
   *
   * @return The {@link MapillaryChangeset} object that stores the changed images.
   */
  public MapillaryChangeset getLocationChangeset() {
    return locationChangeset;
  }

  /**
   * Returns the {@link MapillaryChangeset} object, which acts as the list of deleted images.
   *
   * @return The {@link MapillaryChangeset} object that stores the deleted images.
   */
  public MapillaryChangeset getDeletionChangeset() {
    return deletionChangeset;
  }

  /**
   * Returns the n-nearest image, for n=1 the nearest one is returned, for n=2 the second nearest one and so on.
   * The "n-nearest image" is picked from the list of one image from every sequence that is nearest to the currently
   * selected image, excluding the sequence to which the selected image belongs.
   *
   * @param n the index for picking from the list of "nearest images", beginning from 1
   * @return the n-nearest image to the currently selected image, or null if no such image can be found
   */
  public synchronized MapillaryImage getNNearestImage(final int n) {
    return n >= 1 && n <= nearestImages.length ? nearestImages[n - 1] : null;
  }

  /**
   * Set an image as viewed
   *
   * @param image The image that has been viewed
   * @return {@code true} if the image wasn't viewed before in the current editing session.
   */
  public boolean setImageViewed(MapillaryAbstractImage image) {
    DataSet ds = MainApplication.getLayerManager().getActiveDataSet();
    if (image != null && ds != null) {
      Set<MapillaryAbstractImage> imageViewedList = imageViewedMap
        .getOrDefault(ds, Collections.synchronizedSet(new HashSet<MapillaryAbstractImage>()));
      imageViewedMap.putIfAbsent(ds, imageViewedList);
      return imageViewedList.add(image);
    }
    return false;
  }

  @Override
  public synchronized void destroy() {
    if (!destroyed) {
      data.setSelectedImage(null);
      clearInstance();
      setMode(null);
      MapillaryRecord.getInstance().reset();
      AbstractMode.resetThread();
      /**
       * Stop downloads in a separate thread to avoid a 30s hang. Check if the worker
       * is shutdown first though (so we don't throw an error).
       */
      if (MainApplication.worker.isShutdown()) {
        MapillaryDownloader.stopAll();
      } else {
        MainApplication.worker.submit(MapillaryDownloader::stopAll);
      }
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
          MainApplication.getLayerManager().getEditDataSet().removeDataSourceListener(DATASET_LISTENER);
        }
      } catch (IllegalArgumentException e) {
        // TODO: It would be ideal, to fix this properly. But for the moment let's catch
        // this, for when a listener has
        // already been removed.
      }
      UploadAction.unregisterUploadHook(this);
    }
    super.destroy();
    destroyed = true;
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
    fadeComposite = AlphaComposite
      .getInstance(AlphaComposite.SRC_OVER, MapillaryProperties.UNSELECTED_OPACITY.get().floatValue());
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
          seq.getKey() == null ? MapillaryColorScheme.SEQ_IMPORTED_SELECTED : MapillaryColorScheme.SEQ_SELECTED);
      } else if (selectedImage == null) {
        g.setColor(
          seq.getKey() == null ? MapillaryColorScheme.SEQ_IMPORTED_UNSELECTED : MapillaryColorScheme.SEQ_UNSELECTED);
      } else {
        g.setColor(
          seq.getKey() == null ? MapillaryColorScheme.SEQ_IMPORTED_UNSELECTED : MapillaryColorScheme.SEQ_UNSELECTED);
        g.setComposite(fadeComposite);
      }
      g.draw(MapViewGeometryUtil.getSequencePath(mv, seq));
      if (this.mode instanceof EditMode) {
        Composite backup = g.getComposite();
        g.setComposite(fadeComposite.derive(0.25f));
        g.draw(MapViewGeometryUtil.getImageChangesPath(mv, seq));
        g.draw(MapViewGeometryUtil.getOriginalSequencePath(mv, seq));
        g.setComposite(backup);
      }
      g.setComposite(AlphaComposite.SrcOver);
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
   *
   * @param g   the Graphics context
   * @param img the image to be drawn onto the Graphics context
   */
  private void drawImageMarker(final Graphics2D g, final MapillaryAbstractImage img) {
    if (img == null || img.getLatLon() == null) {
      Logging.warn("An image is not painted, because it is null or has no LatLon!");
      return;
    }
    final MapillaryAbstractImage selectedImg = getData().getSelectedImage();
    if (!IMAGE_CA_PAINT_RANGE.contains(MainApplication.getMap().mapView.getDist100Pixel()) && !img.equals(selectedImg)
      && !getData().getMultiSelectedImages().contains(img)
      && (selectedImg == null || !img.getSequence().equals(selectedImg.getSequence()))) {
      Logging.trace("An image was not painted due to a high zoom level, and not being the selected image/sequence");
      return;
    }
    final Point p = MainApplication.getMap().mapView.getPoint(
      img instanceof MapillaryImage && ((MapillaryImage) img).isDeleted() ? img.getLatLon() : img.getMovingLatLon());
    final Point originalP = MainApplication.getMap().mapView.getPoint(img.getLatLon());
    Composite composite = g.getComposite();
    if (selectedImg != null && !selectedImg.getSequence().equals(img.getSequence())) {
      g.setComposite(fadeComposite);
    }
    // Determine colors
    final Color directionC;
    final Image i;
    if (selectedImg != null && getData().getMultiSelectedImages().contains(img)) {
      i = img.getSelectedImage();
      directionC = img.paintHighlightedAngleColour();
    } else if (selectedImg != null && selectedImg.getSequence() != null
      && selectedImg.getSequence().equals(img.getSequence())) {
      directionC = img.paintSelectedAngleColour();
      i = img.getActiveSequenceImage();
    } else {
      i = img.getDefaultImage();
      directionC = img.paintUnselectedAngleColour();
    }
    // Paint direction indicator
    g.setColor(directionC);
    if (img.isPanorama()) {
      Composite currentComposit = g.getComposite();
      g.setComposite(fadeComposite);
      g.fillOval(p.x - CA_INDICATOR_RADIUS, p.y - CA_INDICATOR_RADIUS, 2 * CA_INDICATOR_RADIUS,
        2 * CA_INDICATOR_RADIUS);
      g.setComposite(currentComposit);
    }

    // This _must_ be set after operations complete (see JOSM 19516 for more information)
    AffineTransform backup = g.getTransform();
    if (img instanceof MapillaryImage && mode instanceof EditMode) {
      if (!((MapillaryImage) img).isDeleted()) {
        Composite currentComposit = g.getComposite();
        g.setComposite(fadeComposite.derive(0.25f));
        g.setTransform(getTransform(Math.toRadians(img.getCa()), originalP, getOriginalCentroid(i), backup));
        g.drawImage(img.getActiveSequenceImage(), originalP.x, originalP.y, null);
        g.setTransform(getTransform(Math.toRadians(img.getMovingCa()), p, getOriginalCentroid(i), backup));
        g.setComposite(currentComposit);
        g.drawImage(i, p.x, p.y, null);
        g.setTransform(backup);
      } else {
        g.setTransform(getTransform(Math.toRadians(img.getCa()), p, getOriginalCentroid(i), backup));
        g.drawImage(img.getDeletedImage(), p.x, p.y, null);
        g.setComposite(composite);
        g.setTransform(backup);
      }
    } else {
      g.setTransform(getTransform(Math.toRadians(img.getMovingCa()), p, getOriginalCentroid(i), backup));
      g.drawImage(i, p.x, p.y, null);
      g.setTransform(backup);
    }

    // Paint highlight for selected or highlighted images
    if (getData().getHighlightedImages().contains(img) || img.equals(getData().getHighlightedImage())
      || getData().getMultiSelectedImages().contains(img)) {
      g.setColor(Color.WHITE);
      g.setStroke(new BasicStroke(2));
      g.drawOval(p.x - IMG_MARKER_RADIUS, p.y - IMG_MARKER_RADIUS, 2 * IMG_MARKER_RADIUS, 2 * IMG_MARKER_RADIUS);

    }

    if (img instanceof MapillaryImage) {
      if (!((MapillaryImage) img).getDetections().isEmpty()) {
        g.drawImage(YIELD_SIGN, (int) (p.getX() - TRAFFIC_SIGN_SIZE / 3d), (int) (p.getY() - TRAFFIC_SIGN_SIZE / 3d),
            null);
      }
    }
    g.setComposite(composite);
  }

  public static AffineTransform getTransform(double angle, Point p, Point2D origin, AffineTransform original) {
    AffineTransform move = AffineTransform.getRotateInstance(angle, p.getX(), p.getY());
    move.preConcatenate(original);
    move.translate(-origin.getX(), -origin.getY());
    Point2D.Double d2 = new Point2D.Double(p.x + origin.getX(), p.y + origin.getY());
    move.transform(d2, d2);
    return move;
  }

  private static Point2D getOriginalCentroid(Image i) {
    if (standardImageCentroid == null) {
      int width = i.getWidth(null);
      int height = i.getHeight(null);
      double originalCentroidX = width / 2d;
      double originalCentroidY = 2 * height / 3d;
      standardImageCentroid = new Point2D.Double(originalCentroidX, originalCentroidY);
    }
    return standardImageCentroid;
  }

  @Override
  public Icon getIcon() {
    return MapillaryPlugin.LOGO.setSize(ImageSizes.LAYER).get();
  }

  /**
   * Check if the Mapillary layer has been used for editing.
   *
   * @return {@code true} if the Mapillary source tag should be added.
   */
  private boolean isApplicable() {
    boolean isApplicable = false;
    DataSet ds = MainApplication.getLayerManager().getActiveDataSet();
    if (ds != null) {
      Collection<OsmPrimitive> primitives = ds.allModifiedPrimitives();
      final double maxDistance = Config.getPref().getDouble("mapillary.source.maxdistance", 30.0);
      Set<MapillaryAbstractImage> imageViewedList = imageViewedMap.getOrDefault(ds, Collections.emptySet());
      synchronized (imageViewedList) {
        for (MapillaryAbstractImage image : imageViewedList) {
          BBox bbox = new BBox();
          bbox.addLatLon(image.getLatLon(), 0.005); // 96m-556m, depending upon N/S location (low at 80 degrees, high at
                                                    // 0)
          List<OsmPrimitive> searchPrimitives = ds.searchPrimitives(bbox);
          if (primitives.parallelStream().filter(searchPrimitives::contains)
            .mapToDouble(prim -> Geometry.getDistance(prim, new Node(image.getLatLon())))
            .anyMatch(d -> d < maxDistance)) {
            isApplicable = true;
            break;
          }
        }
      }
    }
    return isApplicable;
  }

  @Override
  public String getChangesetSourceTag() {
    return isApplicable() ? "Mapillary" : null;
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
    return new Action[] {
      LayerListDialog.getInstance().createShowHideLayerAction(),
      LayerListDialog.getInstance().createDeleteLayerAction(),
      new LayerListPopup.InfoAction(this)
    };
  }

  @Override
  public Object getInfoComponent() {
    IntSummaryStatistics seqSizeStats = getData().getSequences().stream().mapToInt(seq -> seq.getImages().size())
      .summaryStatistics();
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
        seqSizeStats.getAverage()))
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
        MainApplication.getLayerManager().getEditLayer().getDataSet().addDataSourceListener(DATASET_LISTENER);
      }
      if (e.getPreviousDataLayer() != null) {
        try {
          e.getPreviousDataSet().removeDataSourceListener(DATASET_LISTENER);
        } catch (IllegalArgumentException exception) {
          // Do nothing -- there wasn't a data source listener
          Logging.trace(exception);
        }
      }
    }
  }

  @Override
  public void layerAdded(LayerAddEvent e) {
    // Don't care about this
  }

  @Override
  public void layerRemoving(LayerRemoveEvent e) {
    List<DataSet> currentDataSets = MainApplication.getLayerManager().getLayersOfType(OsmDataLayer.class).stream()
      .map(OsmDataLayer::getDataSet).collect(Collectors.toList());
    for (Map.Entry<DataSet, Set<MapillaryAbstractImage>> entry : imageViewedMap.entrySet()) {
      if (!currentDataSets.contains(entry.getKey())) {
        imageViewedMap.remove(entry.getKey());
      }
    }
  }

  @Override
  public void layerOrderChanged(LayerOrderChangeEvent e) {
    // Don't care about this
  }

  @Override
  public void visitBoundingBox(BoundingXYVisitor v) {
    // Don't care about this
  }

  /*
   * (non-Javadoc)
   * @see org.openstreetmap.josm.plugins.mapillary.MapillaryDataListener#imagesAdded()
   */
  @Override
  public void imagesAdded() {
    updateNearestImages();
  }

  /*
   * (non-Javadoc)
   * @see
   * org.openstreetmap.josm.plugins.mapillary.MapillaryDataListener#selectedImageChanged(org.openstreetmap.josm.plugins.
   * mapillary.MapillaryAbstractImage, org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage)
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
   * @param limit  the maximum length of the returned array
   * @return An array containing the closest images belonging to different sequences sorted by distance from target.
   */
  private MapillaryImage[] getNearestImagesFromDifferentSequences(MapillaryAbstractImage target, int limit) {
    return data.getSequences().parallelStream()
      .filter(seq -> seq.getKey() != null && !seq.getKey().equals(target.getSequence().getKey()))
      .map(seq -> { // Maps sequence to image from sequence that is nearest to target
        Optional<MapillaryAbstractImage> resImg = seq.getImages().parallelStream()
          .filter(img -> img instanceof MapillaryImage && !((MapillaryImage) img).isDeleted()&& img.isVisible())
          .min(new NearestImgToTargetComparator(target));
        return resImg.orElse(null);
      })
      .filter(img -> // Filters out images too far away from target
      img != null &&
        img.getMovingLatLon()
          .greatCircleDistance(target.getMovingLatLon()) < MapillaryProperties.SEQUENCE_MAX_JUMP_DISTANCE.get())
      .sorted(new NearestImgToTargetComparator(target))
      .limit(limit)
      .toArray(MapillaryImage[]::new);
  }

  private synchronized void updateNearestImages() {
    final MapillaryAbstractImage selected = data.getSelectedImage();
    if (selected != null && !(selected instanceof MapillaryImage &&((MapillaryImage) selected).isDeleted())) {
      nearestImages = getNearestImagesFromDifferentSequences(selected, 2);
    } else {
      nearestImages = new MapillaryImage[0];
    }
    if (MainApplication.isDisplayingMapView()) {
      if (SwingUtilities.isEventDispatchThread()) {
        updateRedBlueButtons();
      } else {
        SwingUtilities.invokeLater(this::updateRedBlueButtons);
      }
    }
    if (nearestImages.length >= 1) {
      CacheUtils.downloadPicture(nearestImages[0]);
      if (nearestImages.length >= 2) {
        CacheUtils.downloadPicture(nearestImages[1]);
      }
    }
  }

  private void updateRedBlueButtons() {
    MapillaryMainDialog.getInstance().redButton.setEnabled(nearestImages.length >= 1);
    MapillaryMainDialog.getInstance().blueButton.setEnabled(nearestImages.length >= 2);
  }

  /**
   * Action used to delete images.
   *
   * @author nokutu
   */
  class DeleteImageAction extends AbstractAction {

    private static final long serialVersionUID = -982809854631863962L;

    @Override
    public void actionPerformed(ActionEvent e) {
      if (hasInstance() && !getData().getMultiSelectedImages().isEmpty())
        MapillaryRecord.getInstance().addCommand(
          new CommandDelete(getData().getMultiSelectedImages()));
    }
  }

  private static class NearestImgToTargetComparator implements Comparator<MapillaryAbstractImage> {
    private final MapillaryAbstractImage target;

    public NearestImgToTargetComparator(MapillaryAbstractImage target) {
      this.target = target;
    }

    /*
     * (non-Javadoc)
     * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
     */
    @Override
    public int compare(MapillaryAbstractImage img1, MapillaryAbstractImage img2) {
      return (int) Math.signum(
        img1.getMovingLatLon().greatCircleDistance(target.getMovingLatLon()) -
          img2.getMovingLatLon().greatCircleDistance(target.getMovingLatLon()));
    }
  }

  @Override
  public void modifyChangesetTags(Map<String, String> tags) {
    // This is used to clear the viewed image list
    Set<MapillaryAbstractImage> imageViewedList = imageViewedMap
      .get(MainApplication.getLayerManager().getActiveDataSet());
    if (imageViewedList != null)
      imageViewedList.clear();
  }
}
