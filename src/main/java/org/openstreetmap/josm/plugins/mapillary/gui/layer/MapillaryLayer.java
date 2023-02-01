// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.layer;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.AlphaComposite;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Composite;
import java.awt.Graphics2D;
import java.awt.GraphicsEnvironment;
import java.awt.Image;
import java.awt.Point;
import java.awt.RenderingHints;
import java.awt.Stroke;
import java.awt.geom.AffineTransform;
import java.awt.geom.Point2D;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
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
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.locks.Lock;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.annotation.Nonnull;
import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.SwingUtilities;

import org.openstreetmap.josm.actions.UploadAction;
import org.openstreetmap.josm.actions.upload.UploadHook;
import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.data.coor.ILatLon;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.data.imagery.street_level.IImageEntry;
import org.openstreetmap.josm.data.imagery.vectortile.mapbox.MVTTile;
import org.openstreetmap.josm.data.osm.BBox;
import org.openstreetmap.josm.data.osm.DataSet;
import org.openstreetmap.josm.data.osm.HighlightUpdateListener;
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
import org.openstreetmap.josm.gui.layer.geoimage.IGeoImageLayer;
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
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.visitor.paint.MapillaryMapRenderer;
import org.openstreetmap.josm.plugins.mapillary.gui.dialog.MapillaryFilterDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.dialog.OldVersionDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.geoimage.MapillaryImageEntry;
import org.openstreetmap.josm.plugins.mapillary.gui.workers.MapillaryNodeDownloader;
import org.openstreetmap.josm.plugins.mapillary.gui.workers.MapillarySequenceDownloader;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryColorScheme;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryKeys;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillarySequenceUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.OffsetUtils;
import org.openstreetmap.josm.tools.ColorHelper;
import org.openstreetmap.josm.tools.ColorScale;
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
public final class MapillaryLayer extends MVTLayer implements ActiveLayerChangeListener, LayerChangeListener,
    UploadHook, VectorDataSelectionListener, HighlightUpdateListener, IGeoImageLayer {

    /** The radius of the image marker */
    private static final int IMG_MARKER_RADIUS = 7;

    /** The range to paint the full detection image at */
    private static final Range IMAGE_CA_PAINT_RANGE = Selector.GeneralSelector.fromLevel(18, Integer.MAX_VALUE);

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

    /** The color scale used when drawing using velocity */
    private final ColorScale velocityScale = ColorScale.createHSBScale(256);
    /** The color scale used when drawing using date */
    private final ColorScale dateScale = ColorScale.createFixedScale(new Color[] { ColorHelper.html2color("e17155"), // Really
                                                                                                                     // old
                                                                                                                     // color
        ColorHelper.html2color("fbc01b"), // Old color
        MapillaryColorScheme.SEQ_UNSELECTED // New color
    }).addTitle(tr("Time"));

    /** The color scale used when drawing using direction */
    private final ColorScale directionScale = ColorScale.createCyclicScale(256).setIntervalCount(4)
        .addTitle(tr("Direction"));

    /** The nearest images to the selected image from different sequences sorted by distance from selection. */
    // Use ArrayList instead of an array, since there will not be thousands of instances, and allows for better
    // synchronization

    /** The images that have been viewed since the last upload */
    private final ConcurrentHashMap<DataSet, Set<INode>> imageViewedMap = new ConcurrentHashMap<>(1);

    /** {@code true} if this layer is destroyed */
    private boolean destroyed;

    private static AlphaComposite fadeComposite = AlphaComposite.getInstance(AlphaComposite.SRC_OVER,
        MapillaryProperties.UNSELECTED_OPACITY.get().floatValue());
    private static Point2D standardImageCentroid;
    private final ListenerList<MVTTile.TileListener> tileListeners = ListenerList.create();
    private final ListenerList<IGeoImageLayer.ImageChangeListener> imageChangeListeners = ListenerList.create();
    private MapillaryNode image;
    @Nonnull
    private MapillaryLayerDrawTypes drawType = MapillaryLayerDrawTypes.DEFAULT;

    private MapillaryLayer() {
        super(MapillaryKeys.MAPILLARY_IMAGES);
        Stream.of(MapillaryPlugin.getMapillaryDataListeners())
            .forEach(listener -> this.getData().addSelectionListener(listener));
        UploadAction.registerUploadHook(this, true);
        SwingUtilities.invokeLater(OldVersionDialog::showOldVersion);

        this.addTileDownloadListener(OrganizationRecord::addFromTile);
        this.addTileDownloadListener(MapillaryFilterDialog.getInstance());
        this.getData().addSelectionListener(this);
        this.getData().addHighlightUpdateListener(this);
        if (!GraphicsEnvironment.isHeadless() && !ImageViewerDialog.hasInstance()) {
            ImageViewerDialog.getInstance();
        }
        if (Utils.getSystemEnv("CI") != null) {
            Logging.error(
                new IllegalStateException("Checking to see where MapillaryLayer is instantiated in Jenkins tests"));
        }
        setupColorScales();
    }

    private void setupColorScales() {
        this.dateScale.setNoDataColor(MapillaryColorScheme.SEQ_UNSELECTED);
        // ChronoUnit.YEARS isn't supported since a year can be either 365 or 366.
        this.dateScale.setRange(Instant.now().minus(4 * 365L, ChronoUnit.DAYS).toEpochMilli(),
            Instant.now().toEpochMilli());
        this.directionScale.setNoDataColor(MapillaryColorScheme.SEQ_UNSELECTED);
        this.velocityScale.setNoDataColor(MapillaryColorScheme.SEQ_UNSELECTED);
    }

    /**
     * Initializes the Layer.
     */
    private void init() {
        MainApplication.getLayerManager().addActiveLayerChangeListener(this);
        invalidate();
    }

    /**
     * Invalidate the MapillaryLayer instance
     */
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
        this.drawType = MapillaryProperties.MAPILLARY_LAYER_DRAW_TYPE.get();
        final Lock lock = this.getData().getReadLock();
        try {
            // This _must_ be set after operations complete (see JOSM #19516 for more information)
            AffineTransform oldTransform = g.getTransform();
            Object oldAARenderingHint = g.getRenderingHint(RenderingHints.KEY_ANTIALIASING);
            Stroke oldStroke = g.getStroke();
            if (lock.tryLock(100, TimeUnit.MILLISECONDS)) {
                try {
                    this.paintWithLock(g, mv, box);
                } finally {
                    lock.unlock();
                    g.setTransform(oldTransform);
                    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, oldAARenderingHint);
                    g.setStroke(oldStroke);
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

        // Draw sequence line
        g.setStroke(new BasicStroke(2));
        final AffineTransform originalTransform = g.getTransform();
        final double distPer100Pixel = mv.getDist100Pixel();
        final String sequenceKey = MapillaryImageUtils.getSequenceKey(selectedImage);
        for (IWay<?> seq : getData().searchWays(box.toBBox()).stream()
            .sorted(Comparator.comparingInt(IWay::getRawTimestamp)).distinct().collect(Collectors.toList())) {
            if (!Objects.equals(sequenceKey, MapillarySequenceUtils.getKey(seq))) {
                drawSequence(g, mv, seq, selectedImage, originalTransform, distPer100Pixel);
            }
        }

        if (selectedImage != null) {
            // Paint the selected sequences
            for (IWay<?> way : selectedImage.getReferrers().stream().filter(IWay.class::isInstance)
                .map(IWay.class::cast).collect(Collectors.toSet())) {
                drawSequence(g, mv, way, selectedImage, originalTransform, distPer100Pixel);
                for (INode n : way.getNodes()) {
                    if (n != selectedImage) {
                        drawImageMarker(originalTransform, selectedImage, g, n, distPer100Pixel, false);
                    }
                }
            }
            // Paint selected images last. Not particularly worried about painting too much, since most people don't
            // select thousands of images.
            drawImageMarker(originalTransform, selectedImage, g, selectedImage, distPer100Pixel,
                OffsetUtils.getOffset(selectedImage) != 0);
        }
    }

    private void drawSequence(final Graphics2D g, final MapView mv, final IWay<?> sequence, final INode selectedImage,
        AffineTransform originalTransform, double distPer100Pixel) {
        final List<? extends INode> nodes = sequence.getNodes();
        if (selectedImage != null && !nodes.contains(selectedImage)) {
            g.setComposite(fadeComposite);
        }
        final Color forcedColor;
        if (sequence.getNodes().contains(selectedImage)) {
            forcedColor = !MapillarySequenceUtils.hasKey(sequence) ? MapillaryColorScheme.SEQ_IMPORTED_SELECTED
                : MapillaryColorScheme.SEQ_SELECTED;
        } else {
            forcedColor = null;
        }
        final int zoom = getZoomLevel();
        INode previous = null;
        Point previousPoint = null;
        for (INode current : nodes) {
            final Point currentPoint = mv.getPoint(current);
            if (MapillaryImageUtils.isImage(current)) {
                g.setColor(forcedColor != null ? forcedColor : getColor(current));
            }
            if (previous != null && (mv.contains(currentPoint) || mv.contains(previousPoint))) {
                // FIXME get the right color for the line segment
                final boolean visibleNode = zoom >= 14 && sequence.isVisible() && MapillaryImageUtils.isImage(previous);
                g.drawLine(previousPoint.x, previousPoint.y, currentPoint.x, currentPoint.y);
                if (visibleNode) {
                    // FIXME draw the image here (avoid calculating the points twice)
                    drawImageMarker(originalTransform, selectedImage, g, previous, distPer100Pixel, false);
                }
            }
            previous = current;
            previousPoint = currentPoint;
        }
        final boolean visibleNode = zoom >= 14 && sequence.isVisible() && MapillaryImageUtils.isImage(previous);
        if (visibleNode) {
            // FIXME draw the image here (avoid calculating the points twice)
            drawImageMarker(originalTransform, selectedImage, g, previous, distPer100Pixel, false);
        }
        g.setComposite(AlphaComposite.SrcOver);
    }

    private Color getColor(final INode node) {
        switch (this.drawType) {
        case DATE:
            return this.dateScale.getColor(MapillaryImageUtils.getDate(node).toEpochMilli());
        case VELOCITY_FOOT:
        case VELOCITY_BIKE:
        case VELOCITY_CAR:
            final double normalized = calculateVelocity(node);
            if (Double.isNaN(normalized)) {
                return this.velocityScale.getNoDataColor();
            }
            return this.velocityScale.getColor(normalized);
        case DIRECTION:
            final double direction = MapillaryImageUtils.getAngle(node);
            if (Double.isNaN(direction)) {
                return this.directionScale.getNoDataColor();
            }
            return this.directionScale.getColor(direction);
        case DEFAULT:
        }
        return MapillaryColorScheme.SEQ_UNSELECTED;
    }

    /**
     * Calculate the velocity at the node
     *
     * @param node The node to get the velocity of
     * @return The velocity (m/s), or {@link Double#NaN}
     */
    private static double calculateVelocity(@Nonnull INode node) {
        final IWay<?> sequence = MapillaryImageUtils.getSequence(node);
        if (sequence != null && sequence.getNodesCount() >= 2) {
            final INode first;
            final INode second;
            if (sequence.isFirstLastNode(node)) {
                if (node.equals(sequence.firstNode())) {
                    first = node;
                    second = sequence.getNode(1);
                } else {
                    first = sequence.getNode(sequence.getNodesCount() - 2);
                    second = node;
                }
            } else {
                first = MapillarySequenceUtils.getNextOrPrevious(node, MapillarySequenceUtils.NextOrPrevious.PREVIOUS);
                second = MapillarySequenceUtils.getNextOrPrevious(node, MapillarySequenceUtils.NextOrPrevious.NEXT);
            }
            if (first != null && second != null) {
                final long dt = MapillaryImageUtils.getDate(second).toEpochMilli()
                    - MapillaryImageUtils.getDate(first).toEpochMilli();
                final double dd = second.greatCircleDistance(first);
                return dd / (dt / 1000d);
            }
        }
        return Double.NaN;
    }

    /**
     * Draws an image marker onto the given Graphics context.
     *
     * @param originalTransform The original transformation (do not modify this), precalculated for HiDPI displays.
     * @param selectedImg Currently selected nodes
     * @param g the Graphics context
     * @param img the image to be drawn onto the Graphics context
     * @param dist100Pixel The distance per 100 px
     * @param offset {@code true} if we may be painting the offset for an image
     */
    private void drawImageMarker(final AffineTransform originalTransform, final INode selectedImg, final Graphics2D g,
        final INode img, final double dist100Pixel, final boolean offset) {
        if (img == null || !img.isLatLonKnown()) {
            Logging.warn("An image is not painted, because it is null or has no LatLon!");
            return;
        }
        Composite originalComposite = g.getComposite();
        if (!IMAGE_CA_PAINT_RANGE.contains(dist100Pixel) && !img.equals(selectedImg)
            && (selectedImg == null || (MapillaryImageUtils.getSequence(img) != null && !Objects
                .equals(MapillaryImageUtils.getSequenceKey(img), MapillaryImageUtils.getSequenceKey(selectedImg))))) {
            Logging
                .trace("An image was not painted due to a high zoom level, and not being the selected image/sequence");
            return;
        }
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
            directionC = getColor(img);
        }
        MapView mv = MainApplication.getMap().mapView;

        final Point p = mv.getPoint(img.getEastNorth());
        paintDirectionIndicator(g, directionC, img, originalTransform, p, i);
        this.paintHighlight(g, img, selectedImg);

        if (offset && selectedImg == img) {
            final ILatLon drawnCoordinates = OffsetUtils.getOffsetLocation(img);
            final Point offsetP = drawnCoordinates instanceof INode
                // INode implementations may optimize getEastNorth, so prefer that where possible.
                ? mv.getPoint(((INode) drawnCoordinates).getEastNorth())
                : mv.getPoint(drawnCoordinates);
            paintDirectionIndicator(g, directionC, img, originalTransform, offsetP, i);
            g.setTransform(originalTransform);
            paintOffsetLine(g, offsetP, drawnCoordinates, img, true);
        }

        // TODO get the following working
        /*
         * if (img instanceof Detections && !((Detections) img).getDetections().isEmpty()) {
         * TRAFFIC_SIGN_SIZE = (int) (ImageProvider.ImageSizes.MAP.getAdjustedWidth() / 1.5);
         * YIELD_SIGN = new ImageProvider(IMAGE_SPRITE_DIR,
         * "sign-detection").setMaxSize(TRAFFIC_SIGN_SIZE).get().getImage()
         * g.drawImage(YIELD_SIGN, (int) (p.getX() - TRAFFIC_SIGN_SIZE / 3d), (int) (p.getY() - TRAFFIC_SIGN_SIZE / 3d),
         * null);
         * }
         */

        g.setComposite(originalComposite);
        g.setTransform(originalTransform);
    }

    private static void paintDirectionIndicator(Graphics2D g, Color directionC, INode img,
        AffineTransform originalTransform, Point p, Image i) {
        // Paint direction indicator
        g.setColor(directionC);
        if (MapillaryImageUtils.IS_PANORAMIC.test(img)) {
            Composite currentComposite = g.getComposite();
            AffineTransform scale = AffineTransform.getTranslateInstance(p.x, p.y);
            scale.preConcatenate(originalTransform);
            scale.scale(2, 2);
            g.setTransform(scale);
            g.setComposite(fadeComposite);
            g.fillOval(-IMG_MARKER_RADIUS, -IMG_MARKER_RADIUS, 2 * IMG_MARKER_RADIUS, 2 * IMG_MARKER_RADIUS);
            g.setComposite(currentComposite);
        }
        final AffineTransform translate = AffineTransform.getTranslateInstance(p.x, p.y);
        translate.preConcatenate(originalTransform);
        g.setTransform(translate);
        g.fillOval(-IMG_MARKER_RADIUS, -IMG_MARKER_RADIUS, 2 * IMG_MARKER_RADIUS, 2 * IMG_MARKER_RADIUS);
        if (i != null) {
            // This _must_ be set after operations complete (see JOSM #19516 for more information)
            // convert the angle to radians from degrees
            double angle = MapillaryImageUtils.getAngle(img);

            angle = Double.isNaN(angle) ? 0 : angle;
            // TODO update
            /*
             * if (Objects.equals(selectedImg, img))
             * angle += MapillaryMainDialog.getInstance().imageViewer.getRotation();
             */
            Point2D origin = getOriginalCentroid(i);
            AffineTransform move = AffineTransform.getRotateInstance(angle, p.getX(), p.getY());
            move.translate(-origin.getX(), -origin.getY());
            Point2D.Double d2 = new Point2D.Double(p.x + origin.getX(), p.y + origin.getY());
            move.transform(d2, d2);
            move.preConcatenate(originalTransform);
            g.setTransform(move);
            g.drawImage(i, p.x, p.y, null);
            g.setTransform(translate);
        }
    }

    private void paintHighlight(Graphics2D g, INode img, INode selectedImg) {
        // Paint highlight for selected or highlighted images
        if (getData().getHighlighted().contains(img.getPrimitiveId())
            || (selectedImg != null && selectedImg.equals(img))) {
            g.setColor(Color.WHITE);
            g.setStroke(new BasicStroke(2));
            g.drawOval(-IMG_MARKER_RADIUS, -IMG_MARKER_RADIUS, 2 * IMG_MARKER_RADIUS, 2 * IMG_MARKER_RADIUS);
        }
    }

    private static void paintOffsetLine(Graphics2D g, Point p, ILatLon drawnCoordinates, ILatLon img, boolean offset) {
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
    }

    private static synchronized Point2D getOriginalCentroid(Image i) {
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
                for (INode imageToCheck : imageViewedList) {
                    BBox bbox = new BBox();
                    final LatLon coor = new LatLon(imageToCheck.lat(), imageToCheck.lon());
                    // 96m-556m, depending upon N/S location (low at 80 degrees, high at 0)
                    bbox.addPrimitive(imageToCheck, 0.005);
                    List<OsmPrimitive> searchPrimitives = ds.searchPrimitives(bbox);
                    if (primitives.parallelStream().filter(searchPrimitives::contains)
                        .mapToDouble(prim -> Geometry.getDistance(prim, new Node(coor)))
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
            LayerListDialog.getInstance().createDeleteLayerAction(), new LayerListPopup.InfoAction(this),
            new MapillaryCustomizeDrawingAction(Collections.singletonList(this)) };
    }

    @Override
    public Object getInfoComponent() {
        Map<String, List<VectorNode>> nodeCollection = getData().getNodes().stream()
            .filter(node -> MapillaryImageUtils.getKey(node) != 0)
            .filter(node -> MapillaryImageUtils.getSequence(node) != null)
            .collect(Collectors.groupingBy(MapillaryImageUtils::getSequenceKey));
        IntSummaryStatistics seqSizeStats = nodeCollection.values().stream().mapToInt(List::size).summaryStatistics();
        final long numTotal = seqSizeStats.getSum();
        return tr("Mapillary layer") + '\n'
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
                return tr("{0} images in {1} sequences", getData().getNodes().size(), getData().getWays().size());
            } finally {
                readLock.unlock();
            }
        }
        return tr("Dataset is currently being updated");
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

    @Override
    public void finishedLoading(final MVTTile tile) {
        final Set<VectorPrimitive> primitives = new HashSet<>(tile.getData().getAllPrimitives());
        Stream<VectorPrimitive> setKeys = primitives.stream();
        setKeys.filter(MapillaryImageUtils::isImage).forEach(primitive -> MapillaryImageUtils.getKey(primitive, true));
        // We need to ensure that the primitives are reset
        tile.getData().getPrimitivesMap().clear();
        primitives.forEach(primitive -> tile.getData().getPrimitivesMap().put(primitive.getPrimitiveId(), primitive));
        // Set the date for the images
        primitives.stream().filter(INode.class::isInstance).map(INode.class::cast)
            .forEach(MapillaryImageUtils::getDate);
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

    public void setCurrentImage(final MapillaryNode image) {
        this.setImageViewed(image);
        if (image != null && image.isReferredByWays(0)) {
            new MapillarySequenceDownloader(image, this::updateSequence).execute();
        }
        this.image = image;
        this.invalidate();
        if (ImageViewerDialog.hasInstance()) {
            if (image == null) {
                GuiHelper.runInEDT(() -> ImageViewerDialog.getInstance().displayImage(null));
            } else {
                MapillaryImageEntry entry = MapillaryImageEntry.getCachedEntry(image);
                IImageEntry<?> currentEntry = ImageViewerDialog.getCurrentImage();
                if (Objects.equals(entry, currentEntry)) {
                    ImageViewerDialog.getInstance().displayImages(Collections.singletonList(entry));
                } else {
                    IImageEntry<?> old = ImageViewerDialog.getCurrentImage();
                    GuiHelper.runInEDT(() -> ImageViewerDialog.getInstance().displayImage(entry));
                    MainApplication.worker.execute(() -> this.imageChangeListeners.fireEvent(f -> f.imageChanged(this,
                        old instanceof MapillaryImageEntry ? Collections.singletonList(old) : Collections.emptyList(),
                        Collections.singletonList(entry))));
                }
            }
        }
    }

    /**
     * Get the currently visible image
     *
     * @return The image node
     */
    public MapillaryNode getImage() {
        return this.image;
    }

    @Override
    public void selectionChanged(
        SelectionChangeEvent<VectorPrimitive, VectorNode, VectorWay, VectorRelation, VectorDataSet> event) {
        final Collection<VectorNode> nodes = Utils.filteredCollection(event.getSelection(), VectorNode.class);
        if (nodes.size() == 1) {
            final VectorNode node = nodes.iterator().next();
            final IImageEntry<?> displayImage = ImageViewerDialog.hasInstance() ? ImageViewerDialog.getCurrentImage()
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

            new MapillaryNodeDownloader(node, MapillaryLayer.getInstance()::setCurrentImage).execute();
        }
        MapillaryMapRenderer.selectionOrHighlightChanged();
    }

    @Override
    public void highlightUpdated(HighlightUpdateEvent e) {
        MapillaryMapRenderer.selectionOrHighlightChanged();
        this.invalidate();
    }

    /**
     * Update the current sequence
     *
     * @param sequence The sequences that have been updated
     */
    private void updateSequence(MapillarySequence sequence) {
        MapillaryNode node = this.image;
        if (node != null) {
            final List<MapillaryNode> downloadedNodes = sequence.getNodes();
            final MapillaryNode tImage = downloadedNodes.stream().filter(n -> MapillaryImageUtils.equals(n, node))
                .findFirst().orElseGet(
                    () -> downloadedNodes.stream().map(n -> new Pair<>(n, n.getCoor().distanceSq(node.getCoor())))
                        .max(Comparator.comparingDouble(pair -> pair.b)).map(pair -> pair.a).orElse(null));
            // Use this.image instead of node to avoid changing sequences if the user selected a different
            // image while this method was running.
            if (tImage != null && Objects.equals(this.image, tImage)) {
                this.setCurrentImage(tImage);
            }
        }
    }

    @Override
    public void clearSelection() {
        this.setCurrentImage(null);
    }

    @Override
    public List<? extends IImageEntry<?>> getSelection() {
        if (this.image instanceof IImageEntry) {
            return Collections.singletonList((IImageEntry<?>) this.image);
        } else if (this.image != null) {
            return Collections.singletonList(MapillaryImageEntry.getCachedEntry(this.image));
        }
        return Collections.emptyList();
    }

    @Override
    public boolean containsImage(IImageEntry<?> imageEntry) {
        if (!(imageEntry instanceof MapillaryImageEntry)) {
            return false;
        }
        MapillaryImageEntry entry = (MapillaryImageEntry) imageEntry;
        if (Objects.equals(entry.getImage(), this.image)) {
            return true;
        }
        if (entry.getImage() instanceof VectorNode) {
            return this.getData().containsNode((VectorNode) entry.getImage());
        }
        return this.getData().getPrimitiveById(entry.getImage().getPrimitiveId()) != null;
    }

    @Override
    public void addImageChangeListener(ImageChangeListener listener) {
        this.imageChangeListeners.addListener(listener);
    }

    @Override
    public void removeImageChangeListener(ImageChangeListener listener) {
        this.imageChangeListeners.removeListener(listener);
    }
}
