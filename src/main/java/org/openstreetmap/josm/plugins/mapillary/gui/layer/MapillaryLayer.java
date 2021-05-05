// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.layer;

import org.openstreetmap.gui.jmapviewer.TileXY;
import org.openstreetmap.gui.jmapviewer.interfaces.TileJob;
import org.openstreetmap.josm.actions.UploadAction;
import org.openstreetmap.josm.actions.upload.UploadHook;
import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.data.coor.ILatLon;
import org.openstreetmap.josm.data.imagery.vectortile.mapbox.MVTTile;
import org.openstreetmap.josm.data.osm.*;
import org.openstreetmap.josm.data.osm.event.IDataSelectionListener;
import org.openstreetmap.josm.data.osm.visitor.BoundingXYVisitor;
import org.openstreetmap.josm.data.vector.VectorDataSet;
import org.openstreetmap.josm.data.vector.VectorNode;
import org.openstreetmap.josm.data.vector.VectorPrimitive;
import org.openstreetmap.josm.data.vector.VectorRelation;
import org.openstreetmap.josm.data.vector.VectorWay;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.MapView;
import org.openstreetmap.josm.gui.dialogs.LayerListDialog;
import org.openstreetmap.josm.gui.dialogs.LayerListPopup;
import org.openstreetmap.josm.gui.layer.Layer;
import org.openstreetmap.josm.gui.layer.LayerManager.LayerAddEvent;
import org.openstreetmap.josm.gui.layer.LayerManager.LayerChangeListener;
import org.openstreetmap.josm.gui.layer.LayerManager.LayerOrderChangeEvent;
import org.openstreetmap.josm.gui.layer.LayerManager.LayerRemoveEvent;
import org.openstreetmap.josm.gui.layer.MainLayerManager.ActiveLayerChangeEvent;
import org.openstreetmap.josm.gui.layer.MainLayerManager.ActiveLayerChangeListener;
import org.openstreetmap.josm.gui.layer.OsmDataLayer;
import org.openstreetmap.josm.gui.layer.imagery.MVTLayer;
import org.openstreetmap.josm.gui.mappaint.Range;
import org.openstreetmap.josm.gui.mappaint.mapcss.Selector;
import org.openstreetmap.josm.gui.util.GuiHelper;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.actions.SelectNextImageAction;
import org.openstreetmap.josm.plugins.mapillary.cache.CacheUtils;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryMainDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.dialog.MapillaryFilterDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.dialog.OldVersionDialog;
import org.openstreetmap.josm.plugins.mapillary.utils.*;
import org.openstreetmap.josm.spi.preferences.Config;
import org.openstreetmap.josm.tools.Geometry;
import org.openstreetmap.josm.tools.I18n;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.ImageProvider.ImageSizes;
import org.openstreetmap.josm.tools.Logging;

import javax.swing.*;
import java.awt.*;
import java.awt.geom.AffineTransform;
import java.awt.geom.Line2D;
import java.awt.geom.Point2D;
import java.util.List;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.locks.Lock;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils.KEY;

/**
 * This class represents the layer shown in JOSM. There can only exist one
 * instance of this object.
 *
 * @author nokutu
 */
public final class MapillaryLayer extends MVTLayer implements ActiveLayerChangeListener, LayerChangeListener,
  UploadHook, IDataSelectionListener<VectorPrimitive, VectorNode, VectorWay, VectorRelation, VectorDataSet> {

  /** The radius of the image marker */
  private static final int IMG_MARKER_RADIUS = 7;
  /** The radius of the circular sector that indicates the camera angle */
  private static final int CA_INDICATOR_RADIUS = 15;
  /** Length of the edge of the small sign, which indicates that traffic signs have been found in an image. */
  private static final int TRAFFIC_SIGN_SIZE = (int) (ImageProvider.ImageSizes.MAP.getAdjustedWidth() / 1.5);
  /** The range to paint the full detection image at */
  private static final Range IMAGE_CA_PAINT_RANGE = Selector.GeneralSelector.fromLevel(18, Integer.MAX_VALUE);

  /** The sprite to use to indicate that there are sign detections in the image */
  private static final Image YIELD_SIGN = new ImageProvider("josm-ca", "sign-detection").setMaxSize(TRAFFIC_SIGN_SIZE)
    .get().getImage();

  private static final String IMAGE_SPRITE_DIR = "josm-ca";
  /** The default sprite for a Mapillary image */
  private static final ImageIcon DEFAULT_SPRITE = new ImageProvider(IMAGE_SPRITE_DIR, "default-ca")
    .setMaxWidth(ImageProvider.ImageSizes.MAP.getAdjustedHeight()).get();
  /** The sprite to use for the active Mapillary sequence */
  private static final ImageIcon ACTIVE_SEQUENCE_SPRITE = new ImageProvider(IMAGE_SPRITE_DIR, "sequence-ca")
    .setMaxWidth(ImageProvider.ImageSizes.MAP.getAdjustedHeight()).get();
  /** The sprite to use for the currently selected image */
  private static final ImageIcon SELECTED_IMAGE = new ImageProvider(IMAGE_SPRITE_DIR, "current-ca")
    .setMaxWidth(ImageProvider.ImageSizes.MAP.getAdjustedHeight()).get();

  /** Unique instance of the class. */
  private static MapillaryLayer instance;
  /** The nearest images to the selected image from different sequences sorted by distance from selection. */
  // Use ArrayList instead of an array, since there will not be thousands of instances, and allows for better
  // synchronization
  private final List<INode> nearestImages = Collections.synchronizedList(new ArrayList<>());

  /** The images that have been viewed since the last upload */
  private final ConcurrentHashMap<DataSet, Set<INode>> imageViewedMap = new ConcurrentHashMap<>();

  private boolean destroyed;
  private static AlphaComposite fadeComposite = AlphaComposite.getInstance(AlphaComposite.SRC_OVER,
    MapillaryProperties.UNSELECTED_OPACITY.get().floatValue());
  private static Point2D standardImageCentroid = null;

  private MapillaryLayer() {
    super(MapillaryKeys.MAPILLARY_IMAGES);
    this.getData().addSelectionListener(this);
    Stream.of(MapillaryPlugin.getMapillaryDataListeners())
      .forEach(listener -> this.getData().addSelectionListener(listener));
    UploadAction.registerUploadHook(this, true);
    SwingUtilities.invokeLater(OldVersionDialog::showOldVersion);
  }

  /**
   * Initializes the Layer.
   */
  private void init() {
    MainApplication.getLayerManager().addActiveLayerChangeListener(this);

    // Does not execute when in headless mode
    if (MainApplication.getMainFrame() != null && !MapillaryMainDialog.getInstance().isShowing()) {
      MapillaryMainDialog.getInstance().showDialog();
    }
    if (MapillaryPlugin.getMapView() != null) {
      MapillaryMainDialog.getInstance().imageViewer.repaint();
    }
    invalidate();
  }

  public static void invalidateInstance() {
    if (hasInstance()) {
      getInstance().invalidate();
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
   * Check if there is a {@link MapillaryLayer} instance
   *
   * @return if the unique instance of this layer is currently instantiated
   */
  public static boolean hasInstance() {
    return instance != null;
  }

  /**
   * Returns the n-nearest image, for n=1 the nearest one is returned, for n=2 the second nearest one and so on.
   * The "n-nearest image" is picked from the list of one image from every sequence that is nearest to the currently
   * selected image, excluding the sequence to which the selected image belongs.
   *
   * @param n the index for picking from the list of "nearest images", beginning from 1
   * @return the n-nearest image to the currently selected image, or null if no such image can be found
   */
  public INode getNNearestImage(final int n) {
    synchronized (this.nearestImages) {
      return n >= 1 && n <= this.nearestImages.size() ? this.nearestImages.get(n - 1) : null;
    }
  }

  /**
   * Set an image as viewed
   *
   * @param image The image that has been viewed
   * @return {@code true} if the image wasn't viewed before in the current editing session.
   */
  public boolean setImageViewed(INode image) {
    DataSet ds = MainApplication.getLayerManager().getActiveDataSet();
    if (image != null && ds != null) {
      Set<INode> imageViewedList = imageViewedMap.getOrDefault(ds, Collections.synchronizedSet(new HashSet<>()));
      imageViewedMap.putIfAbsent(ds, imageViewedList);
      return imageViewedList.add(image);
    }
    return false;
  }

  @Override
  public synchronized void destroy() {
    if (!destroyed) {
      this.getData().clearSelection();
      clearInstance();
      if (MapillaryMainDialog.hasInstance()) {
        MapillaryMainDialog.getInstance().setImage(null);
        MapillaryMainDialog.getInstance().updateImage();
      }
      UploadAction.unregisterUploadHook(this);
    }
    super.destroy();
    destroyed = true;
  }

  @Override
  public void setVisible(boolean visible) {
    super.setVisible(visible);
    // Avoid an NPE during initialization
    if (this.getData() == null) {
      return;
    }
    this.getData().getNodes().parallelStream().filter(node -> node.hasKey(KEY)).forEach(img -> img.setVisible(visible));
    if (MainApplication.getMap() != null) {
      MapillaryFilterDialog.getInstance().refresh();
    }
  }

  @Override
  public void paint(final Graphics2D g, final MapView mv, final Bounds box) {
    final Lock lock = this.getData().getReadLock();
    try {
      lock.lockInterruptibly();
      this.paintWithLock(g, mv, box);
    } catch (InterruptedException e) {
      Logging.error(e);
      Thread.currentThread().interrupt();
    } finally {
      lock.unlock();
    }
  }

  private void paintWithLock(final Graphics2D g, final MapView mv, final Bounds box) {
    this.getData().setZoom(this.getZoomLevel());
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
    fadeComposite = AlphaComposite.getInstance(AlphaComposite.SRC_OVER,
      MapillaryProperties.UNSELECTED_OPACITY.get().floatValue());

    // Draw the blue and red line
    final INode selectedImage = this.getData().getSelectedNodes().stream().findFirst().orElse(null);
    synchronized (this) {
      for (int i = 0; i < this.nearestImages.size() && selectedImage != null; i++) {
        if (i == 0) {
          g.setColor(Color.RED);
        } else {
          g.setColor(Color.BLUE);
        }
        if (selectedImage != null) {
          final Point selected = mv.getPoint(selectedImage.getCoor());
          final Point p = mv.getPoint(this.nearestImages.get(i).getCoor());
          g.draw(new Line2D.Double(p.getX(), p.getY(), selected.getX(), selected.getY()));
        }
      }
    }

    // Draw sequence line
    g.setStroke(new BasicStroke(2));
    final String sequenceKey = MapillarySequenceUtils.getKey(MapillaryImageUtils.getSequence(selectedImage));
    Collection<IWay<?>> selectedSequences = new ArrayList<>();
    for (IWay<?> seq : getData().searchWays(box.toBBox()).stream().distinct().collect(Collectors.toList())) {
      if (seq.getNodes().contains(selectedImage) || sequenceKey.equals(MapillarySequenceUtils.getKey(seq))) {
        selectedSequences.add(seq);
      } else {
        drawSequence(g, mv, seq, false, selectedImage != null);
      }
    }
    final Collection<INode> images = this.getData().searchNodes(box.toBBox()).stream().distinct()
      .collect(Collectors.toList());
    if (images.size() < Config.getPref().getInt("mapillary.images.max_draw", 1000)) {
      for (INode imageAbs : images) {
        if (imageAbs.isVisible()) {
          drawImageMarker(g, imageAbs);
        }
      }
    }
    // Paint the selected sequences
    for (IWay<?> sequence : selectedSequences) {
      drawSequence(g, mv, sequence, true, selectedImage != null);
    }
    // Paint selected images last. Not particularly worried about painting too much, since most people don't select
    // thousands of images.
    for (INode imageAbs : this.getData().getSelectedNodes()) {
      if (imageAbs.isVisible() && mv != null && mv.contains(mv.getPoint(imageAbs.getCoor()))) {
        drawImageMarker(g, imageAbs);
      }
    }
  }

  private void drawSequence(final Graphics2D g, final MapView mv, final IWay<?> sequence, boolean selected,
    boolean selectedImage) {
    if (selected) {
      g.setColor(
        !sequence.hasKey(KEY) ? MapillaryColorScheme.SEQ_IMPORTED_SELECTED : MapillaryColorScheme.SEQ_SELECTED);
    } else if (!selectedImage) {
      g.setColor(!MapillarySequenceUtils.hasKey(sequence) ? MapillaryColorScheme.SEQ_IMPORTED_UNSELECTED
        : MapillaryColorScheme.SEQ_UNSELECTED);
    } else {
      g.setColor(!MapillarySequenceUtils.hasKey(sequence) ? MapillaryColorScheme.SEQ_IMPORTED_UNSELECTED
        : MapillaryColorScheme.SEQ_UNSELECTED);
      g.setComposite(fadeComposite);
    }
    g.draw(MapViewGeometryUtil.getSequencePath(mv, sequence));
    g.setComposite(AlphaComposite.SrcOver);
  }

  /**
   * Draws an image marker onto the given Graphics context.
   *
   * @param g the Graphics context
   * @param img the image to be drawn onto the Graphics context
   */
  private void drawImageMarker(final Graphics2D g, final INode img) {
    if (img == null || img.getCoor() == null) {
      Logging.warn("An image is not painted, because it is null or has no LatLon!");
      return;
    }
    final INode selectedImg = getData().getSelectedNodes().stream().findFirst().orElse(null);
    if (!IMAGE_CA_PAINT_RANGE.contains(MainApplication.getMap().mapView.getDist100Pixel()) && !img.equals(selectedImg)
      && !getData().getSelectedNodes().contains(img)
      && (selectedImg == null || (img.hasKey(MapillaryImageUtils.SEQUENCE_KEY)
        && !img.get(MapillaryImageUtils.SEQUENCE_KEY).equals(selectedImg.get(MapillaryImageUtils.SEQUENCE_KEY))))) {
      Logging.trace("An image was not painted due to a high zoom level, and not being the selected image/sequence");
      return;
    }
    final Point p = MainApplication.getMap().mapView.getPoint(img.getCoor());
    Composite composite = g.getComposite();
    if (selectedImg != null && selectedImg.hasKey(MapillaryImageUtils.SEQUENCE_KEY)
      && !selectedImg.get(MapillaryImageUtils.SEQUENCE_KEY).equals(img.get(MapillaryImageUtils.SEQUENCE_KEY))) {
      g.setComposite(fadeComposite);
    }
    // Determine colors
    final Color directionC;
    final Image i;
    if (selectedImg != null && getData().getSelectedNodes().contains(img)) {
      i = SELECTED_IMAGE.getImage();
      directionC = MapillaryColorScheme.SEQ_HIGHLIGHTED_CA;
    } else if (selectedImg != null && selectedImg.hasKey(MapillaryImageUtils.SEQUENCE_KEY)
      && selectedImg.get(MapillaryImageUtils.SEQUENCE_KEY).equals(img.get(MapillaryImageUtils.SEQUENCE_KEY))) {
      directionC = MapillaryColorScheme.SEQ_SELECTED_CA;
      i = ACTIVE_SEQUENCE_SPRITE.getImage();
    } else {
      i = DEFAULT_SPRITE.getImage();
      directionC = MapillaryColorScheme.SEQ_UNSELECTED_CA;
    }
    // Paint direction indicator
    g.setColor(directionC);
    if (img.hasKey(MapillaryKeys.PANORAMIC) && MapillaryKeys.PANORAMIC_TRUE.equals(img.get(MapillaryKeys.PANORAMIC))) {
      Composite currentComposite = g.getComposite();
      g.setComposite(fadeComposite);
      g.fillOval(p.x - CA_INDICATOR_RADIUS, p.y - CA_INDICATOR_RADIUS, 2 * CA_INDICATOR_RADIUS,
        2 * CA_INDICATOR_RADIUS);
      g.setComposite(currentComposite);
    }

    // This _must_ be set after operations complete (see JOSM 19516 for more information)
    AffineTransform backup = g.getTransform();
    double angle = MapillaryImageUtils.getAngle(img);
    angle = Double.isNaN(angle) ? 0 : angle;
    if (Objects.equals(selectedImg, img))
      angle += MapillaryMainDialog.getInstance().imageViewer.getRotation();
    g.setTransform(getTransform(angle, p, getOriginalCentroid(i), backup));
    g.drawImage(i, p.x, p.y, null);
    g.setTransform(backup);

    // Paint highlight for selected or highlighted images
    if (getData().getHighlighted().contains(img.getPrimitiveId())) {
      g.setColor(Color.WHITE);
      g.setStroke(new BasicStroke(2));
      g.drawOval(p.x - IMG_MARKER_RADIUS, p.y - IMG_MARKER_RADIUS, 2 * IMG_MARKER_RADIUS, 2 * IMG_MARKER_RADIUS);
    }
    // TODO get the following working
    /*
     * if (img instanceof Detections && !((Detections) img).getDetections().isEmpty()) {
     * g.drawImage(YIELD_SIGN, (int) (p.getX() - TRAFFIC_SIGN_SIZE / 3d), (int) (p.getY() - TRAFFIC_SIGN_SIZE / 3d),
     * null);
     * }
     */
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
      Set<INode> imageViewedList = imageViewedMap.getOrDefault(ds, Collections.emptySet());
      synchronized (imageViewedList) {
        for (INode image : imageViewedList) {
          BBox bbox = new BBox();
          // 96m-556m, depending upon N/S location (low at 80 degrees, high at 0)
          bbox.addLatLon(image.getCoor(), 0.005);
          List<OsmPrimitive> searchPrimitives = ds.searchPrimitives(bbox);
          if (primitives.parallelStream().filter(searchPrimitives::contains)
            .mapToDouble(prim -> Geometry.getDistance(prim, new Node(image.getCoor())))
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
    throw new UnsupportedOperationException("This layer does not support merging yet");
  }

  @Override
  public Action[] getMenuEntries() {
    return new Action[] { LayerListDialog.getInstance().createShowHideLayerAction(),
      LayerListDialog.getInstance().createDeleteLayerAction(), new LayerListPopup.InfoAction(this) };
  }

  @Override
  public Object getInfoComponent() {
    Map<String, List<VectorNode>> nodeCollection = getData().getNodes().stream().filter(node -> node.hasKey(KEY))
      .collect(Collectors.groupingBy(node -> node.get(MapillaryImageUtils.SEQUENCE_KEY)));
    IntSummaryStatistics seqSizeStats = nodeCollection.values().stream().mapToInt(List::size).summaryStatistics();
    final long numTotal = seqSizeStats.getSum();
    return new StringBuilder(I18n.tr("Mapillary layer")).append('\n')
      .append(I18n.trn("{0} sequence, containing between {1} and {2} images (ø {3})",
        "{0} sequences, each containing between {1} and {2} images (ø {3})", getData().getWays().size(),
        getData().getWays().size(), seqSizeStats.getCount() <= 0 ? 0 : seqSizeStats.getMin(),
        seqSizeStats.getCount() <= 0 ? 0 : seqSizeStats.getMax(), seqSizeStats.getAverage()))
      .append("\n= ").append(I18n.trn("{0} image in total", "{0} images in total", numTotal, numTotal)).toString();
  }

  @Override
  public String getToolTipText() {
    return I18n.tr("{0} images in {1} sequences", getData().getNodes().size(), getData().getWays().size());
  }

  @Override
  public void activeOrEditLayerChanged(ActiveLayerChangeEvent e) {
    if (MainApplication.getLayerManager().getActiveLayer() == this) {
      MapillaryUtils.updateHelpText();
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
    for (Map.Entry<DataSet, Set<INode>> entry : imageViewedMap.entrySet()) {
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

  @Override
  public void selectionChanged(
    IDataSelectionListener.SelectionChangeEvent<VectorPrimitive, VectorNode, VectorWay, VectorRelation, VectorDataSet> event) {
    // Fix a RejectedExecutionException on JOSM shutdown
    if (!MainApplication.worker.isShutdown()) {
      MainApplication.worker.execute(this::updateNearestImages);
    }
    if (MapillaryMainDialog.hasInstance()) {
      INode image = null;
      for (SelectNextImageAction action : Arrays.asList(SelectNextImageAction.NEXT_ACTION,
        SelectNextImageAction.PREVIOUS_ACTION, SelectNextImageAction.BLUE_ACTION, SelectNextImageAction.RED_ACTION)) {
        INode possible = action.getDestinationImageSupplier().get();
        // possible is always null initially
        if (possible != null) {
          Optional<INode> node = event.getSelection().stream().filter(INode.class::isInstance).map(INode.class::cast)
            .filter(possible::equals).findFirst();
          if (node.isPresent()) {
            image = node.get();
          }
        }
      }
      if (image == null) {
        image = event.getSelection().stream().filter(INode.class::isInstance).map(INode.class::cast)
          .filter(MapillaryImageUtils.IS_IMAGE).findFirst().orElse(null);
      }
      MapillaryMainDialog.getInstance().setImage(image);
    }
    this.invalidate();
  }

  /**
   * Returns the closest images belonging to a different sequence and
   * different from the specified target image.
   *
   * @param target the image for which you want to find the nearest other images
   * @param limit the maximum length of the returned array
   * @return An array containing the closest images belonging to different sequences sorted by distance from target.
   */
  private INode[] getNearestImagesFromDifferentSequences(INode target, int limit) {
    if (!target.hasKey(MapillaryImageUtils.SEQUENCE_KEY)) {
      return new INode[] {};
    }
    BBox searchBBox = new BBox();
    searchBBox.addPrimitive(target, 0.0001);
    // Locked MapillaryLayer, waiting for a java.util.stream.Nodes$CollectorTask$OfRef
    return getData().searchWays(searchBBox).parallelStream()
      .filter(seq -> seq.get(KEY) != null && !seq.get(KEY).equals(target.get(MapillaryImageUtils.SEQUENCE_KEY)))
      .map(seq -> {
        // Maps sequence to image from sequence that is nearest to target
        Optional<VectorNode> resImg = seq.getNodes().parallelStream().filter(img -> img.hasKey(KEY))
          .min(new NearestImgToTargetComparator(target));
        return resImg.orElse(null);
      }).filter(img -> // Filters out images too far away from target
      img != null
        && img.getCoor().greatCircleDistance(target.getCoor()) < MapillaryProperties.SEQUENCE_MAX_JUMP_DISTANCE.get())
      .sorted(new NearestImgToTargetComparator(target)).limit(limit).toArray(INode[]::new);
  }

  private void updateNearestImages() {
    ForkJoinPool pool = MapillaryUtils.getForkJoinPool();
    final INode selected;
    if (MapillaryMainDialog.hasInstance()) {
      selected = MapillaryMainDialog.getInstance().getImage();
    } else {
      selected = getData().getSelectedNodes().stream().findFirst().orElse(null);
    }
    INode[] newNearestImages;
    if (selected != null && selected.hasKey(KEY)) {
      newNearestImages = getNearestImagesFromDifferentSequences(selected, 2);
    } else {
      newNearestImages = new INode[0];
    }
    synchronized (this.nearestImages) {
      this.nearestImages.clear();
      Stream.of(newNearestImages).filter(Objects::nonNull).forEach(this.nearestImages::add);

      if (MainApplication.isDisplayingMapView()) {
        GuiHelper.runInEDT(this::updateRedBlueButtons);
      }
      this.nearestImages.stream().filter(Objects::nonNull)
        .forEach(image -> pool.execute(() -> CacheUtils.downloadPicture(image)));
    }
    this.invalidate();
  }

  private void updateRedBlueButtons() {
    synchronized (this.nearestImages) {
      MapillaryMainDialog.getInstance().redButton.setEnabled(!nearestImages.isEmpty());
      MapillaryMainDialog.getInstance().blueButton.setEnabled(nearestImages.size() >= 2);
    }
  }

  /**
   * The the tile for a location
   *
   * @param location The location to load the tile for
   * @return {@code true} if the tile has finished loading and was actually loaded in the method.
   */
  public boolean loadTileFor(ILatLon location) {
    TileXY tileXY = this.tileSource.latLonToTileXY(location.lat(), location.lon(), this.getZoomLevel());
    if (this.tileCache.getTile(this.tileSource, tileXY.getXIndex(), tileXY.getYIndex(), this.getZoomLevel()) != null) {
      return false;
    }
    MVTTile tile = (MVTTile) this.createTile(this.tileSource, tileXY.getXIndex(), tileXY.getYIndex(),
      this.getZoomLevel());
    TileJob job = this.tileLoader.createTileLoaderJob(tile);
    synchronized (job) {
      job.submit();
      try {
        // Typically < 700 ms
        job.wait(1000);
      } catch (InterruptedException e) {
        Logging.error(e);
        Thread.currentThread().interrupt();
      }
    }
    this.tileCache.addTile(tile);
    this.tileLoadingFinished(tile, tile.isLoaded());
    this.getData().addTileData(tile);
    return tile.isLoaded();
  }

  private static class NearestImgToTargetComparator implements Comparator<INode> {
    private final INode target;

    public NearestImgToTargetComparator(INode target) {
      this.target = target;
    }

    /*
     * (non-Javadoc)
     * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
     */
    @Override
    public int compare(INode img1, INode img2) {
      return (int) Math.signum(
        img1.getCoor().greatCircleDistance(target.getCoor()) - img2.getCoor().greatCircleDistance(target.getCoor()));
    }
  }
}
