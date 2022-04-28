// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.layer;

import java.awt.AlphaComposite;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Composite;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Point;
import java.awt.RenderingHints;
import java.awt.geom.AffineTransform;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Line2D;
import java.awt.geom.Point2D;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.IntSummaryStatistics;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.locks.Lock;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.SwingUtilities;

import org.openstreetmap.gui.jmapviewer.Tile;
import org.openstreetmap.gui.jmapviewer.TileXY;
import org.openstreetmap.gui.jmapviewer.interfaces.TileJob;
import org.openstreetmap.josm.actions.UploadAction;
import org.openstreetmap.josm.actions.upload.UploadHook;
import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.data.coor.ILatLon;
import org.openstreetmap.josm.data.imagery.street_level.IImageEntry;
import org.openstreetmap.josm.data.imagery.vectortile.mapbox.MVTTile;
import org.openstreetmap.josm.data.osm.BBox;
import org.openstreetmap.josm.data.osm.DataSet;
import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.data.osm.IWay;
import org.openstreetmap.josm.data.osm.Node;
import org.openstreetmap.josm.data.osm.OsmPrimitive;
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
import org.openstreetmap.josm.gui.layer.geoimage.ImageViewerDialog;
import org.openstreetmap.josm.gui.layer.imagery.MVTLayer;
import org.openstreetmap.josm.gui.mappaint.Range;
import org.openstreetmap.josm.gui.mappaint.mapcss.Selector;
import org.openstreetmap.josm.gui.util.GuiHelper;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.MapillaryNode;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.MapillarySequence;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.OrganizationRecord;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.VectorDataSelectionListener;
import org.openstreetmap.josm.plugins.mapillary.gui.dialog.MapillaryFilterDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.dialog.OldVersionDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.geoimage.MapillaryImageEntry;
import org.openstreetmap.josm.plugins.mapillary.io.download.MapillaryDownloader;
import org.openstreetmap.josm.plugins.mapillary.utils.MapViewGeometryUtil;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryColorScheme;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryKeys;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillarySequenceUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.OffsetUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.ReflectionUtils;
import org.openstreetmap.josm.tools.ColorHelper;
import org.openstreetmap.josm.tools.Geometry;
import org.openstreetmap.josm.tools.I18n;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.ImageProvider.ImageSizes;
import org.openstreetmap.josm.tools.ListenerList;
import org.openstreetmap.josm.tools.Logging;
import org.openstreetmap.josm.tools.Pair;
import org.openstreetmap.josm.tools.Utils;

/**
 * This class represents the layer shown in JOSM. There can only exist one
 * instance of this object.
 *
 * @author nokutu
 */
public final class MapillaryLayer extends MVTLayer
    implements ActiveLayerChangeListener, LayerChangeListener, UploadHook, VectorDataSelectionListener {

    /** The radius of the image marker */
    private static final int IMG_MARKER_RADIUS = 7;
    /** The radius of the circular sector that indicates the camera angle */
    private static final int CA_INDICATOR_RADIUS = 15;

    /** The color for really old imagery */
    private static final Color REALLY_OLD_COLOR = ColorHelper.html2color("e17155");
    /** The color for older imagery */
    private static final Color OLD_COLOR = ColorHelper.html2color("fbc01b");

    /** Length of the edge of the small sign, which indicates that traffic signs have been found in an image. */
    private static final int TRAFFIC_SIGN_SIZE = (int) (ImageProvider.ImageSizes.MAP.getAdjustedWidth() / 1.5);
    /** The range to paint the full detection image at */
    private static final Range IMAGE_CA_PAINT_RANGE = Selector.GeneralSelector.fromLevel(18, Integer.MAX_VALUE);

    private static final String IMAGE_SPRITE_DIR = "josm-ca";
    /** The sprite to use to indicate that there are sign detections in the image */
    private static final Image YIELD_SIGN = new ImageProvider(IMAGE_SPRITE_DIR, "sign-detection")
        .setMaxSize(TRAFFIC_SIGN_SIZE).get().getImage();
    /** The default sprite for a Mapillary image */
    private static final ImageIcon DEFAULT_SPRITE = new ImageProvider(IMAGE_SPRITE_DIR, "default-ca")
        .setMaxWidth(ImageProvider.ImageSizes.MAP.getAdjustedHeight()).get();
    /** The sprite to use for the active Mapillary sequence */
    private static final ImageIcon ACTIVE_SEQUENCE_SPRITE = new ImageProvider(IMAGE_SPRITE_DIR, "sequence-ca")
        .setMaxWidth(ImageProvider.ImageSizes.MAP.getAdjustedHeight()).get();
    /** The sprite to use for the currently selected image */
    private static final ImageIcon SELECTED_IMAGE = new ImageProvider(IMAGE_SPRITE_DIR, "current-ca")
        .setMaxWidth(ImageProvider.ImageSizes.MAP.getAdjustedHeight()).get();

    private static final Ellipse2D IMAGE_CIRCLE = new Ellipse2D.Double(-IMG_MARKER_RADIUS, -IMG_MARKER_RADIUS,
        2 * IMG_MARKER_RADIUS, 2 * IMG_MARKER_RADIUS);

    /** The milliseconds in a year (approx) */
    private static final long YEAR_MILLIS = 31_557_600_000L;

    /** The nearest images to the selected image from different sequences sorted by distance from selection. */
    // Use ArrayList instead of an array, since there will not be thousands of instances, and allows for better
    // synchronization
    private final List<INode> nearestImages = Collections.synchronizedList(new ArrayList<>(2));

    /** The images that have been viewed since the last upload */
    private final ConcurrentHashMap<DataSet, Set<INode>> imageViewedMap = new ConcurrentHashMap<>(1);

    /** {@code true} if this layer is destroyed */
    private boolean destroyed;

    private static AlphaComposite fadeComposite = AlphaComposite.getInstance(AlphaComposite.SRC_OVER,
        MapillaryProperties.UNSELECTED_OPACITY.get().floatValue());
    private static Point2D standardImageCentroid = null;
    private final ListenerList<MVTTile.TileListener> tileListeners = ListenerList.create();
    private MapillaryNode image = null;

    private MapillaryLayer() {
        super(MapillaryKeys.MAPILLARY_IMAGES);
        Stream.of(MapillaryPlugin.getMapillaryDataListeners())
            .forEach(listener -> this.getData().addSelectionListener(listener));
        UploadAction.registerUploadHook(this, true);
        SwingUtilities.invokeLater(OldVersionDialog::showOldVersion);

        this.addTileDownloadListener(OrganizationRecord::addFromTile);
        this.addTileDownloadListener(MapillaryFilterDialog.getInstance());
        this.getData().addSelectionListener(this);
    }

    /**
     * Initializes the Layer.
     */
    private void init() {
        MainApplication.getLayerManager().addActiveLayerChangeListener(this);
        invalidate();
    }

    public static void invalidateInstance() {
        if (hasInstance()) {
            getInstance().invalidate();
        }
    }

    /**
     * Returns the unique instance of this class.
     *
     * @return The unique instance of this class.
     */
    public static MapillaryLayer getInstance() {
        final Optional<MapillaryLayer> layerOptional = MainApplication.getLayerManager()
            .getLayersOfType(MapillaryLayer.class).stream().findFirst();
        if (layerOptional.isPresent()) {
            return layerOptional.get();
        }
        synchronized (MapillaryLayer.class) {
            AtomicReference<MapillaryLayer> instance = new AtomicReference<>(MainApplication.getLayerManager()
                .getLayersOfType(MapillaryLayer.class).stream().findFirst().orElse(null));
            if (instance.get() != null) {
                return instance.get();
            }
            // Running in the EDT can help avoid deadlocks due to synchronization in LayerManager
            GuiHelper.runInEDT(() -> {
                final MapillaryLayer layer = new MapillaryLayer();
                layer.init();
                synchronized (MapillaryLayer.class) {
                    instance.set(layer);
                    // Only set instance field after initialization is complete
                    MainApplication.getLayerManager().addLayer(layer);
                    // Finally, wake up threads.
                    MapillaryLayer.class.notifyAll();
                }
            });
            while (instance.get() == null) {
                try {
                    MapillaryLayer.class.wait(500);
                } catch (InterruptedException e) {
                    Logging.error(e);
                    Thread.currentThread().interrupt();
                }
            }
            return instance.get();
        }
    }

    /**
     * Check if there is a {@link MapillaryLayer} instance
     *
     * @return if the unique instance of this layer is currently instantiated
     */
    public static boolean hasInstance() {
        return !MainApplication.getLayerManager().getLayersOfType(MapillaryLayer.class).isEmpty();
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
            Set<INode> imageViewedList = imageViewedMap.computeIfAbsent(ds,
                dataSet -> Collections.synchronizedSet(new HashSet<>(1)));
            return imageViewedList.add(image);
        }
        return false;
    }

    @Override
    public synchronized void destroy() {
        if (!destroyed) {
            this.getData().clearSelection();
            if (MainApplication.getMap() != null
                && MainApplication.getMap().getToggleDialog(ImageViewerDialog.class) != null
                && ImageViewerDialog.getCurrentImage() instanceof MapillaryImageEntry) {
                ImageViewerDialog.getInstance().displayImage(null);
            }
            UploadAction.unregisterUploadHook(this);
            super.destroy();
        }
        destroyed = true;
    }

    @Override
    public void setVisible(boolean visible) {
        super.setVisible(visible);
        // Avoid an NPE during initialization
        if (this.getData() == null) {
            return;
        }
        this.getData().getNodes().parallelStream().filter(MapillaryImageUtils::isImage)
            .forEach(img -> img.setVisible(visible));
        if (MainApplication.getMap() != null) {
            MapillaryFilterDialog.getInstance().refresh();
        }
    }

    @Override
    public void paint(final Graphics2D g, final MapView mv, final Bounds box) {
        final Lock lock = this.getData().getReadLock();
        try {
            // This _must_ be set after operations complete (see JOSM #19516 for more information)
            AffineTransform backup = g.getTransform();
            if (lock.tryLock(100, TimeUnit.MILLISECONDS)) {
                try {
                    this.paintWithLock(g, mv, box);
                } finally {
                    lock.unlock();
                    g.setTransform(backup);
                }
            }
        } catch (InterruptedException e) {
            Logging.error(e);
            Thread.currentThread().interrupt();
        }
    }

    private void paintWithLock(final Graphics2D g, final MapView mv, final Bounds box) {
        this.getData().setZoom(this.getZoomLevel());
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        fadeComposite = AlphaComposite.getInstance(AlphaComposite.SRC_OVER,
            MapillaryProperties.UNSELECTED_OPACITY.get().floatValue());

        final INode selectedImage = this.image;
        synchronized (this) {
            for (int i = 0; i < this.nearestImages.size(); i++) {
                if (i % 2 == 0) {
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
        final String sequenceKey = MapillaryImageUtils.getSequenceKey(selectedImage);
        for (IWay<?> seq : getData().searchWays(box.toBBox()).stream().distinct().collect(Collectors.toList())) {
            if (!Objects.equals(sequenceKey, MapillarySequenceUtils.getKey(seq))) {
                drawSequence(g, mv, seq, selectedImage);
            }
        }
        final Collection<INode> images = this.getData().searchNodes(box.toBBox()).stream().distinct()
            .collect(Collectors.toList());
        if (images.size() < MapillaryProperties.MAXIMUM_DRAW_IMAGES.get()) {
            for (INode imageAbs : images) {
                if (imageAbs.isVisible() && MapillaryImageUtils.isImage(imageAbs)
                    && !MapillaryImageUtils.equals(this.image, imageAbs)
                    && !Objects.equals(MapillaryImageUtils.getSequenceKey(imageAbs), sequenceKey)) {
                    drawImageMarker(selectedImage, g, imageAbs, false);
                }
            }
        }
        if (selectedImage != null) {
            // Paint the selected sequences
            for (IWay<?> way : selectedImage.getReferrers().stream().filter(IWay.class::isInstance)
                .map(IWay.class::cast).collect(Collectors.toSet())) {
                drawSequence(g, mv, way, selectedImage);
                for (INode n : way.getNodes()) {
                    drawImageMarker(selectedImage, g, n, false);
                }
            }
            // Paint selected images last. Not particularly worried about painting too much, since most people don't
            // select
            // thousands of images.
            drawImageMarker(selectedImage, g, selectedImage, true);
        }
    }

    private static void drawSequence(final Graphics2D g, final MapView mv, final IWay<?> sequence, final INode image) {
        if (sequence.getNodes().contains(image)) {
            g.setColor(!MapillarySequenceUtils.hasKey(sequence) ? MapillaryColorScheme.SEQ_IMPORTED_SELECTED
                : MapillaryColorScheme.SEQ_SELECTED);
        } else {
            final Color color;
            if (MapillarySequenceUtils.hasKey(sequence)) {
                final INode toCheck = sequence.getNodes().stream()
                    .filter(inode -> !Instant.EPOCH.equals(MapillaryImageUtils.getDate(inode))).map(INode.class::cast)
                    .findFirst().orElse(sequence.firstNode());
                color = getAgedColor(toCheck, MapillaryColorScheme.SEQ_UNSELECTED);
            } else {
                color = MapillaryColorScheme.SEQ_IMPORTED_UNSELECTED;
            }
            g.setColor(color);
            if (image != null) {
                g.setComposite(fadeComposite);
            }
        }
        g.draw(MapViewGeometryUtil.getSequencePath(mv, sequence));
        g.setComposite(AlphaComposite.SrcOver);
    }

    private static Color getAgedColor(final INode node, final Color defaultColor) {
        if (Boolean.TRUE.equals(MapillaryProperties.COLOR_BY_CAPTURE_DATE.get())) {
            final long timeDiff = Instant.now().toEpochMilli() - MapillaryImageUtils.getDate(node).toEpochMilli();
            // ms per year is ~31_556_952_000 ms
            if (timeDiff > 4 * YEAR_MILLIS) {
                return REALLY_OLD_COLOR;
            } else if (timeDiff > YEAR_MILLIS) {
                return OLD_COLOR;
            }
        }
        return defaultColor;
    }

    /**
     * Draws an image marker onto the given Graphics context.
     *
     * @param selectedImg Currently selected nodes
     * @param g the Graphics context
     * @param img the image to be drawn onto the Graphics context
     * @param offset {@code true} if we may be painting the offset for an image
     */
    private void drawImageMarker(final INode selectedImg, final Graphics2D g, final INode img, final boolean offset) {
        if (img == null || img.getCoor() == null) {
            Logging.warn("An image is not painted, because it is null or has no LatLon!");
            return;
        }
        if (!IMAGE_CA_PAINT_RANGE.contains(MainApplication.getMap().mapView.getDist100Pixel())
            && !img.equals(selectedImg)
            && (selectedImg == null || (MapillaryImageUtils.getSequence(img) != null && !Objects
                .equals(MapillaryImageUtils.getSequenceKey(img), MapillaryImageUtils.getSequenceKey(selectedImg))))) {
            Logging
                .trace("An image was not painted due to a high zoom level, and not being the selected image/sequence");
            return;
        }
        final ILatLon drawnCoordinates;
        if (offset) {
            drawnCoordinates = OffsetUtils.getOffsetLocation(img);
        } else {
            drawnCoordinates = img;
        }
        final Point p = MainApplication.getMap().mapView.getPoint(drawnCoordinates);
        Composite composite = g.getComposite();
        if (MapillaryImageUtils.getSequenceKey(selectedImg) != null && !Objects
            .equals(MapillaryImageUtils.getSequenceKey(selectedImg), MapillaryImageUtils.getSequenceKey(img))) {
            g.setComposite(fadeComposite);
        }
        // Determine colors
        final Color directionC;
        final Image i;
        if (selectedImg != null && selectedImg.equals(img)) {
            i = SELECTED_IMAGE.getImage();
            directionC = MapillaryColorScheme.SEQ_HIGHLIGHTED_CA;
        } else if (MapillaryImageUtils.getSequenceKey(selectedImg) != null && Objects
            .equals(MapillaryImageUtils.getSequenceKey(selectedImg), MapillaryImageUtils.getSequenceKey(img))) {
            directionC = MapillaryColorScheme.SEQ_SELECTED_CA;
            i = ACTIVE_SEQUENCE_SPRITE.getImage();
        } else {
            if (getData().getHighlighted().contains(img.getPrimitiveId())) {
                i = DEFAULT_SPRITE.getImage();
            } else {
                i = null;
            }
            directionC = getAgedColor(img, MapillaryColorScheme.SEQ_UNSELECTED);
        }
        // Paint direction indicator
        g.setColor(directionC);
        if (MapillaryImageUtils.IS_PANORAMIC.test(img)) {
            Composite currentComposite = g.getComposite();
            AffineTransform scale = AffineTransform.getTranslateInstance(p.x, p.y);
            scale.scale(2, 2);
            g.setTransform(scale);
            g.setComposite(fadeComposite);
            g.fill(IMAGE_CIRCLE);
            g.setComposite(currentComposite);
        }
        g.setTransform(AffineTransform.getTranslateInstance(p.x, p.y));
        g.fill(IMAGE_CIRCLE);
        if (i != null) {
            // This _must_ be set after operations complete (see JOSM #19516 for more information)
            AffineTransform backup = g.getTransform();
            // convert the angle to radians from degrees
            double angle = MapillaryImageUtils.getAngle(img);

            angle = Double.isNaN(angle) ? 0 : angle;
            // TODO update
            /*
             * if (Objects.equals(selectedImg, img))
             * angle += MapillaryMainDialog.getInstance().imageViewer.getRotation();
             */
            g.setTransform(getTransform(angle, p, getOriginalCentroid(i)));
            g.drawImage(i, p.x, p.y, null);
            g.setTransform(backup);
        }

        // Paint highlight for selected or highlighted images
        if (getData().getHighlighted().contains(img.getPrimitiveId())
            || (selectedImg != null && selectedImg.equals(img))) {
            g.setColor(Color.WHITE);
            g.setStroke(new BasicStroke(2));
            g.drawOval(-IMG_MARKER_RADIUS, -IMG_MARKER_RADIUS, 2 * IMG_MARKER_RADIUS, 2 * IMG_MARKER_RADIUS);
        }
        // TODO get the following working
        /*
         * if (img instanceof Detections && !((Detections) img).getDetections().isEmpty()) {
         * g.drawImage(YIELD_SIGN, (int) (p.getX() - TRAFFIC_SIGN_SIZE / 3d), (int) (p.getY() - TRAFFIC_SIGN_SIZE / 3d),
         * null);
         * }
         */
        g.setComposite(composite);

        // Draw a line to the original location
        if (offset && (Double.compare(drawnCoordinates.lat(), img.lat()) != 0
            || Double.compare(drawnCoordinates.lon(), img.lon()) != 0)) {
            final Point originalPoint = MainApplication.getMap().mapView.getPoint(img);
            g.setColor(MapillaryColorScheme.SEQ_IMPORTED_HIGHLIGHTED);
            g.drawLine(p.x, p.y, originalPoint.x, originalPoint.y);
            // Draw a small dot at original location to indicate original
            final int radius = IMG_MARKER_RADIUS / 2;
            g.setColor(MapillaryColorScheme.MAPILLARY_GREEN);
            g.fillOval(originalPoint.x - radius, originalPoint.y - radius, 2 * radius, 2 * radius);
            g.setColor(MapillaryColorScheme.SEQ_IMPORTED_HIGHLIGHTED);
            g.fillOval(p.x - radius, p.y - radius, 2 * radius, 2 * radius);
        }
        g.setTransform(AffineTransform.getTranslateInstance(0, 0));
    }

    /**
     * Get the transformation of an angle and point
     *
     * @param angle The angle to rotate (radians)
     * @param p The point to transform around
     * @param origin The origin of the transform
     * @return An affine transform to rotate around an arbitrary point
     */
    private static AffineTransform getTransform(double angle, Point p, Point2D origin) {
        AffineTransform move = AffineTransform.getRotateInstance(angle, p.getX(), p.getY());
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
            final double maxDistance = MapillaryProperties.MAXIMUM_DISTANCE_FOR_CHANGESET_SOURCE.get();
            Set<INode> imageViewedList = imageViewedMap.getOrDefault(ds,
                Collections.synchronizedSet(Collections.emptySet()));
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
        Map<String, List<VectorNode>> nodeCollection = getData().getNodes().stream()
            .filter(node -> MapillaryImageUtils.getKey(node) != 0)
            .collect(Collectors.groupingBy(MapillaryImageUtils::getSequenceKey));
        IntSummaryStatistics seqSizeStats = nodeCollection.values().stream().mapToInt(List::size).summaryStatistics();
        final long numTotal = seqSizeStats.getSum();
        return I18n.tr("Mapillary layer") + '\n'
            + I18n.trn("{0} sequence, containing between {1} and {2} images (ø {3})",
                "{0} sequences, each containing between {1} and {2} images (ø {3})", getData().getWays().size(),
                getData().getWays().size(), seqSizeStats.getCount() <= 0 ? 0 : seqSizeStats.getMin(),
                seqSizeStats.getCount() <= 0 ? 0 : seqSizeStats.getMax(), seqSizeStats.getAverage())
            + "\n= " + I18n.trn("{0} image in total", "{0} images in total", numTotal, numTotal);
    }

    @Override
    public String getToolTipText() {
        final Lock readLock = getData().getReadLock();
        if (readLock.tryLock()) {
            try {
                return I18n.tr("{0} images in {1} sequences", getData().getNodes().size(), getData().getWays().size());
            } finally {
                readLock.unlock();
            }
        }
        return I18n.tr("Dataset is currently being updated");
    }

    @Override
    public void activeOrEditLayerChanged(ActiveLayerChangeEvent e) {
        if (this.equals(MainApplication.getLayerManager().getActiveLayer())) {
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
    public boolean isSavable() {
        // This layer can change at any time, so it isn't worth trying to save
        return false;
    }

    /**
     * The tile for a location
     *
     * @param location The location to load the tile for
     * @return A future indicating if loading finished
     */
    public Future<Tile> loadTileFor(ILatLon location) {
        if (this.tileSource == null) {
            return CompletableFuture.completedFuture(null);
        }
        TileXY tileXY = this.tileSource.latLonToTileXY(location.lat(), location.lon(), this.getZoomLevel());
        final MVTTile tile;
        final boolean first;
        if (this.tileCache.getTile(this.tileSource, tileXY.getXIndex(), tileXY.getYIndex(),
            this.getZoomLevel()) != null) {
            tile = (MVTTile) this.tileCache.getTile(this.tileSource, tileXY.getXIndex(), tileXY.getYIndex(),
                this.getZoomLevel());
            first = false;
        } else {
            tile = (MVTTile) this.createTile(this.tileSource, tileXY.getXIndex(), tileXY.getYIndex(),
                this.getZoomLevel());
            first = true;
        }
        CompletableFuture<Tile> futureTile = new CompletableFuture<>();
        tile.addTileLoaderFinisher(t -> {
            if (first) {
                this.tileLoadingFinished(tile, tile.isLoaded());
                this.getData().addTileData(tile);
                this.tileCache.addTile(tile);
            }
            futureTile.complete(tile);
        });
        // If not first, the first job may have already finished.
        if (tile.isLoaded()) {
            return CompletableFuture.completedFuture(tile);
        }
        if (first) {
            TileJob job = this.tileLoader.createTileLoaderJob(tile);
            job.submit();
        }
        return futureTile;
    }

    @Override
    public void finishedLoading(final MVTTile tile) {
        tile.getData().getAllPrimitives().stream().filter(MapillaryImageUtils::isImage)
            .forEach(primitive -> MapillaryImageUtils.getKey(primitive, true));
        // We need to ensure that the primitives are reset
        final Set<VectorPrimitive> primitives = new HashSet<>(tile.getData().getAllPrimitives());
        tile.getData().getPrimitivesMap().clear();
        primitives.forEach(primitive -> tile.getData().getPrimitivesMap().put(primitive.getPrimitiveId(), primitive));
        super.finishedLoading(tile);
        this.tileListeners.fireEvent(l -> l.finishedLoading(tile));
    }

    /**
     * Add a tile listener
     *
     * @param tileListener The listener to add
     */
    public void addTileDownloadListener(final MVTTile.TileListener tileListener) {
        this.tileListeners.addListener(tileListener);
    }

    public void removeTileDownloadListener(final MVTTile.TileListener tileListener) {
        this.tileListeners.removeListener(tileListener);
    }

    public void setCurrentImage(final MapillaryNode image) {
        this.setImageViewed(image);
        this.image = image;
        this.invalidate();
        if (ReflectionUtils.hasImageViewerDialog()) {
            if (image == null) {
                GuiHelper.runInEDT(() -> ImageViewerDialog.getInstance().displayImage(null));
            } else {
                MapillaryImageEntry entry = MapillaryImageEntry.getCachedEntry(image);
                if (Objects.equals(entry, ImageViewerDialog.getCurrentImage())) {
                    entry.reload();
                } else {
                    GuiHelper.runInEDT(() -> ImageViewerDialog.getInstance().displayImage(entry));
                }
            }
        }
    }

    public MapillaryNode getImage() {
        return this.image;
    }

    @Override
    public void selectionChanged(
        SelectionChangeEvent<VectorPrimitive, VectorNode, VectorWay, VectorRelation, VectorDataSet> event) {
        final Collection<VectorNode> nodes = Utils.filteredCollection(event.getSelection(), VectorNode.class);
        if (nodes.size() == 1) {
            final VectorNode node = nodes.iterator().next();
            final IImageEntry<?> displayImage = ReflectionUtils.hasImageViewerDialog()
                ? ImageViewerDialog.getCurrentImage()
                : null;
            final MapillaryNode currentImage = this.image;
            if (currentImage != null && displayImage instanceof MapillaryImageEntry
                && currentImage.getSequence() != null) {
                final MapillaryNode tImage = currentImage.getSequence().getNodes().stream()
                    .filter(n -> MapillaryImageUtils.equals(n, node)).findFirst().orElse(null);
                if (tImage != null) {
                    this.setCurrentImage(tImage);
                    return;
                }
            }
            MapillaryUtils.getForkJoinPool().execute(() -> this.downloadSequence(this.downloadNode(node)));
        }
    }

    /**
     * Download a singular node. This is faster than downloading large sequences.
     *
     * @param node The node to download
     * @return The downloaded node
     */
    private MapillaryNode downloadNode(VectorNode node) {
        final MapillaryNode tImage = MapillaryDownloader.downloadImages(MapillaryImageUtils.getKey(node)).values()
            .stream().flatMap(Collection::stream).filter(n -> MapillaryImageUtils.equals(n, node)).findFirst()
            .orElse(null);
        this.setCurrentImage(tImage);
        return tImage;
    }

    /**
     * Download the rest of the sequence
     *
     * @param node The node to download
     */
    private void downloadSequence(MapillaryNode node) {
        final Collection<MapillarySequence> sequences = MapillaryDownloader.downloadSequences(node,
            nodes -> this.updateSequence(node, nodes), MapillaryImageUtils.getSequenceKey(node));
        this.updateSequence(node, sequences);
    }

    /**
     * Update the current sequence
     *
     * @param node The currently selected node
     * @param sequences The sequences that have been updated
     */
    private void updateSequence(MapillaryNode node, Collection<MapillarySequence> sequences) {
        final List<MapillaryNode> downloadedNodes = sequences.stream().flatMap(seq -> seq.getNodes().stream())
            .collect(Collectors.toList());
        final MapillaryNode tImage = downloadedNodes.stream().filter(n -> MapillaryImageUtils.equals(n, node))
            .findFirst()
            .orElseGet(() -> downloadedNodes.stream().map(n -> new Pair<>(n, n.getCoor().distanceSq(node.getCoor())))
                .max(Comparator.comparingDouble(pair -> pair.b)).map(pair -> pair.a).orElse(null));
        if (Objects.equals(this.image, tImage)) {
            this.setCurrentImage(tImage);
        }
    }
}
