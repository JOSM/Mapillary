package org.openstreetmap.josm.plugins.mapillary.gui.layer.geoimage;

import static org.openstreetmap.josm.tools.I18n.marktr;
import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.text.DateFormat;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.TimeZone;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.function.BiConsumer;
import java.util.function.UnaryOperator;
import java.util.regex.Pattern;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import javax.swing.ImageIcon;

import org.apache.commons.jcs3.access.CacheAccess;
import org.openstreetmap.josm.data.cache.JCSCacheManager;
import org.openstreetmap.josm.data.coor.ILatLon;
import org.openstreetmap.josm.data.imagery.street_level.IImageEntry;
import org.openstreetmap.josm.data.imagery.street_level.Projections;
import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.data.osm.IWay;
import org.openstreetmap.josm.data.preferences.AbstractProperty;
import org.openstreetmap.josm.data.vector.VectorDataSet;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.layer.geoimage.ImageViewerDialog;
import org.openstreetmap.josm.gui.util.GuiHelper;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.ImageMode;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.ObjectDetections;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.OrganizationRecord;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.PointObjectLayer;
import org.openstreetmap.josm.plugins.mapillary.io.download.MapillaryDownloader;
import org.openstreetmap.josm.plugins.mapillary.model.ImageDetection;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryMapFeatureUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillarySequenceUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;
import org.openstreetmap.josm.tools.Logging;
import org.openstreetmap.josm.tools.MemoryManager;
import org.openstreetmap.josm.tools.Utils;
import org.openstreetmap.josm.tools.date.DateUtils;

public class MapillaryImageEntry
    implements IImageEntry<MapillaryImageEntry>, BiConsumer<Long, List<ImageDetection<?>>> {
    /** A pattern to use to check for numbers (avoids NumberFormatException) */
    private static final Pattern NUMBER_PATTERN = Pattern.compile("^[0-9]+$");
    private static final CacheAccess<Long, MapillaryImageEntry> CACHE = JCSCacheManager
        .getCache("mapillary:mapillaryimageentry");
    private static final String BASE_TITLE = marktr("Mapillary image");
    private static final String MESSAGE_SEPARATOR = " — ";
    /** This is the estimated memory used per image */
    private static final int MEMORY_PER_IMAGE_2048 = 2048 * 2048 * 4;
    private final INode image;
    private final List<ImageDetection<?>> imageDetections = new ArrayList<>();
    private MapillaryImageEntry next;
    private MapillaryImageEntry previous;
    private BufferedImage originalImage;
    private BufferedImage layeredImage;

    private static class MapillaryValueChangeListener implements AbstractProperty.ValueChangeListener<Boolean> {
        static MapillaryValueChangeListener instance = new MapillaryValueChangeListener();

        @Override
        public void valueChanged(AbstractProperty.ValueChangeEvent<? extends Boolean> e) {
            Optional.ofNullable(ImageViewerDialog.getCurrentImage()).filter(MapillaryImageEntry.class::isInstance)
                .map(MapillaryImageEntry.class::cast).ifPresent(entry -> {
                    entry.imageDetections.clear();
                    entry.updateDetections(5_000);
                });
        }
    }

    static {
        MapillaryProperties.SHOW_DETECTED_SIGNS.addListener(MapillaryValueChangeListener.instance);
        MapillaryProperties.SHOW_DETECTION_OUTLINES.addListener(MapillaryValueChangeListener.instance);
    }

    private MapillarySequenceUtils.NextOrPrevious nextOrPrevious = MapillarySequenceUtils.NextOrPrevious.NEXT;

    public static MapillaryImageEntry getCachedEntry(final INode image) {
        final long id = MapillaryImageUtils.getKey(image);
        if (id > 0) {
            return CACHE.get(id, () -> new MapillaryImageEntry(image));
        }
        return new MapillaryImageEntry(image);
    }

    /**
     * Create a new image entry
     *
     * @param image the image we want to display
     */
    public MapillaryImageEntry(final INode image) {
        Objects.requireNonNull(image, "image cannot be null");
        this.image = image;
        final IWay<?> sequence = MapillaryImageUtils.getSequence(this.image);
        // First get the specific information for this image. This does mean we rerequest the same information, but
        // it should be livable, since it will be more cache-friendly.
        MapillaryDownloader.downloadImages(MapillaryImageUtils.getKey(this.image));
        // Then get the information for the rest of the sequence
        ForkJoinPool.commonPool()
            .execute(() -> Optional.ofNullable(sequence).map(IWay::getNodes)
                .map(nodes -> nodes.stream().mapToLong(MapillaryImageUtils::getKey).toArray())
                .ifPresent(MapillaryDownloader::downloadImages));
        this.updateDetections(5_000);
    }

    /**
     * Clone another entry. Mostly useful to avoid != checks
     *
     * @param other The entry to clone
     */
    MapillaryImageEntry(final MapillaryImageEntry other) {
        this.image = other.image;
        this.imageDetections.addAll(other.imageDetections);
        this.layeredImage = other.layeredImage;
        this.originalImage = other.originalImage;
        this.nextOrPrevious = other.nextOrPrevious;
        this.next = other.next;
        this.previous = other.previous;
    }

    @Override
    public MapillaryImageEntry getNextImage() {
        if (this.next == null) {
            this.next = Optional
                .ofNullable(
                    MapillarySequenceUtils.getNextOrPrevious(this.image, MapillarySequenceUtils.NextOrPrevious.NEXT))
                .map(MapillaryImageEntry::getCachedEntry).map(entry -> {
                    entry.previous = this;
                    return entry;
                }).orElse(null);
        }
        return next;
    }

    @Override
    public MapillaryImageEntry getPreviousImage() {
        if (this.previous == null) {
            this.previous = Optional
                .ofNullable(MapillarySequenceUtils.getNextOrPrevious(this.image,
                    MapillarySequenceUtils.NextOrPrevious.PREVIOUS))
                .map(MapillaryImageEntry::getCachedEntry).map(entry -> {
                    entry.next = this;
                    return entry;
                }).orElse(null);
        }
        return this.previous;
    }

    @Override
    public MapillaryImageEntry getFirstImage() {
        return Optional.ofNullable(MapillaryImageUtils.getSequence(this.image)).map(IWay::firstNode)
            .map(MapillaryImageEntry::getCachedEntry).orElse(null);
    }

    @Override
    public MapillaryImageEntry getLastImage() {
        return Optional.ofNullable(MapillaryImageUtils.getSequence(this.image)).map(IWay::lastNode)
            .map(MapillaryImageEntry::getCachedEntry).orElse(null);
    }

    @Override
    public void selectNextImage(ImageViewerDialog imageViewerDialog) {
        IImageEntry.super.selectNextImage(imageViewerDialog);
        this.selectImage(this.getNextImage());
    }

    @Override
    public void selectPreviousImage(ImageViewerDialog imageViewerDialog) {
        IImageEntry.super.selectPreviousImage(imageViewerDialog);
        this.selectImage(this.getPreviousImage());
    }

    @Override
    public void selectFirstImage(ImageViewerDialog imageViewerDialog) {
        IImageEntry.super.selectFirstImage(imageViewerDialog);
        this.selectImage(this.getFirstImage());
    }

    @Override
    public void selectLastImage(ImageViewerDialog imageViewerDialog) {
        IImageEntry.super.selectLastImage(imageViewerDialog);
        this.selectImage(this.getLastImage());
    }

    private void selectImage(@Nullable final MapillaryImageEntry entry) {
        if (entry != null) {
            MapillaryLayer.getInstance().getData().setSelected(entry.image);
        }
    }

    @Override
    public String getDisplayName() {
        StringBuilder title = new StringBuilder(tr(BASE_TITLE));
        if (MapillaryImageUtils.getKey(this.image) != 0) {
            INode mapillaryImage = this.image;
            OrganizationRecord organizationRecord = MapillaryImageUtils.getOrganization(mapillaryImage);
            if (organizationRecord != OrganizationRecord.NULL_RECORD) {
                title.append(MESSAGE_SEPARATOR).append(organizationRecord.getNiceName());
            }
            if (!Instant.EPOCH.equals(MapillaryImageUtils.getDate(mapillaryImage))) {
                final boolean showHour = Boolean.TRUE.equals(MapillaryProperties.DISPLAY_HOUR.get());
                final Instant pictureTime = MapillaryImageUtils.getDate(mapillaryImage);
                title.append(MESSAGE_SEPARATOR);
                final DateFormat formatter;
                if (showHour) {
                    formatter = DateUtils.getDateTimeFormat(DateFormat.DEFAULT, DateFormat.DEFAULT);
                } else {
                    formatter = DateUtils.getDateFormat(DateFormat.DEFAULT);
                }
                // Use UTC, since mappers may be outside of "their" timezone, which would be even more confusing.
                formatter.setTimeZone(TimeZone.getTimeZone(ZoneOffset.UTC));
                title.append(formatter.format(Date.from(pictureTime)));
            }
        } else if (this.image.hasKey(MapillaryImageUtils.IMPORTED_KEY)) {
            INode mapillaryImportedImage = this.image;
            title.append(MESSAGE_SEPARATOR).append(mapillaryImportedImage.get(MapillaryImageUtils.IMPORTED_KEY));
            title.append(MESSAGE_SEPARATOR).append(MapillaryImageUtils.getDate(mapillaryImportedImage));
        }
        return title.toString();
    }

    @Override
    public BufferedImage read(Dimension target) throws IOException {
        if (this.imageDetections.isEmpty()) {
            this.updateDetections(5_000);
        }
        ForkJoinPool.commonPool().execute(() -> preCacheImages(this));
        if (this.originalImage == null) {
            this.loadImage();
        }
        if (this.layeredImage == null) {
            this.layeredImage = new BufferedImage(this.originalImage.getWidth(), this.originalImage.getHeight(),
                this.originalImage.getType());
        }
        this.drawDetections();
        return this.layeredImage;
    }

    /**
     * Load the image for this object
     *
     * @throws IOException If something happens
     */
    private synchronized void loadImage() throws IOException {
        if (this.originalImage != null) {
            return;
        }
        final int memoryNeeded = getMemoryNeeded();
        final int actualMemoryNeeded = memoryNeeded > 0 ? memoryNeeded : MEMORY_PER_IMAGE_2048;
        final List<IOException> exceptions = new ArrayList<>(1);
        if (MemoryManager.getInstance().isAvailable(actualMemoryNeeded)
            && MemoryManager.getInstance().getAvailableMemory() > actualMemoryNeeded
            && Runtime.getRuntime().freeMemory() > actualMemoryNeeded) {
            try {
                MemoryManager.MemoryHandle<BufferedImage> handle = MemoryManager.getInstance().allocateMemory(
                    "MapillaryImageEntry", memoryNeeded != 0 ? memoryNeeded : MEMORY_PER_IMAGE_2048, () -> {
                        Future<BufferedImage> future = MapillaryImageUtils.getImage(this.image);
                        try {
                            return future.get(5, TimeUnit.SECONDS);
                        } catch (ExecutionException | TimeoutException exception) {
                            exceptions.add(new IOException(exception));
                        } catch (InterruptedException exception) {
                            Thread.currentThread().interrupt();
                            exceptions.add(new IOException(exception));
                        }
                        return null;
                    });
                this.originalImage = handle.get();
                handle.free();
            } catch (MemoryManager.NotEnoughMemoryException notEnoughMemoryException) {
                throw new IOException(notEnoughMemoryException);
            }
            if (!exceptions.isEmpty()) {
                throw exceptions.get(0);
            }
        }
    }

    /**
     * Get the estimated memory needed for an image. Probably high.
     *
     * @return The number of bytes needed (width * height * 4, 4 from rgba)
     */
    private int getMemoryNeeded() {
        int width = 0;
        int height = 0;
        final String widthString = this.image.get(MapillaryImageUtils.ImageProperties.WIDTH.toString());
        final String heightString = this.image.get(MapillaryImageUtils.ImageProperties.HEIGHT.toString());
        if (!Utils.isBlank(widthString) && NUMBER_PATTERN.matcher(widthString).matches()) {
            width = Integer.parseInt(widthString);
        }
        if (!Utils.isBlank(heightString) && NUMBER_PATTERN.matcher(heightString).matches()) {
            height = Integer.parseInt(heightString);
        }
        return width * height * 4;
    }

    private static void preCacheImages(final MapillaryImageEntry entry) {
        final Integer prefetch = MapillaryProperties.PRE_FETCH_IMAGE_COUNT.get();
        final int realPrefetch;
        if (prefetch == null) {
            realPrefetch = MapillaryProperties.PRE_FETCH_IMAGE_COUNT.getDefaultValue();
        } else {
            realPrefetch = prefetch > 0 ? prefetch : 0;
        }
        final IWay<?> iWay = MapillaryImageUtils.getSequence(entry.image);
        if (iWay != null) {
            // Avoid CME -- getImage may remove nodes from the way
            final List<? extends INode> wayNodes = new ArrayList<>(iWay.getNodes());
            final int index = wayNodes.indexOf(entry.image);
            final List<? extends INode> nodes = wayNodes.subList(Math.max(0, index - realPrefetch),
                Math.min(wayNodes.size(), index + realPrefetch));
            nodes.forEach(MapillaryImageUtils::getImage);
        }
        preloadImages(entry, realPrefetch);
    }

    private static void preloadImages(final MapillaryImageEntry entry, final int preloadNumber) {
        final List<MapillaryImageEntry> toPreload = new ArrayList<>(preloadNumber);
        MapillaryImageEntry current = entry;
        UnaryOperator<MapillaryImageEntry> getNext;
        if (entry.nextOrPrevious == MapillarySequenceUtils.NextOrPrevious.NEXT) {
            // Ensure we don't keep stuff around in memory
            if (entry.previous != null && entry.previous.previous != null) {
                entry.previous.previous.next = null;
                entry.previous.previous = null;
            }
            getNext = MapillaryImageEntry::getNextImage;
        } else {
            // Ensure we don't keep stuff around in memory
            if (entry.next != null && entry.next.next != null) {
                entry.next.next.previous = null;
                entry.next.next = null;
            }
            getNext = MapillaryImageEntry::getPreviousImage;
        }
        for (int i = 0; i < preloadNumber; i++) {
            Optional<MapillaryImageEntry> next = Optional.ofNullable(getNext.apply(current));
            if (next.isPresent()) {
                current = next.get();
                toPreload.add(current);
            } else {
                break;
            }
        }
        for (MapillaryImageEntry imageToPreload : toPreload) {
            try {
                imageToPreload.loadImage();
            } catch (IOException ioException) {
                Logging.info(ioException);
                if (ioException.getCause() instanceof MemoryManager.NotEnoughMemoryException) {
                    break;
                }
            }
        }
    }

    private void drawDetections() throws IOException {
        if (this.originalImage == null || this.layeredImage == null) {
            this.read(null);
        }
        final int width = this.layeredImage.getWidth();
        final int height = this.layeredImage.getHeight();
        List<PointObjectLayer> detectionLayers = MainApplication.getLayerManager()
            .getLayersOfType(PointObjectLayer.class);
        final AffineTransform unit2CompTransform = AffineTransform.getTranslateInstance(0, 0);
        unit2CompTransform.concatenate(AffineTransform.getScaleInstance(width, height));

        final Graphics2D graphics = this.layeredImage.createGraphics();
        graphics.drawImage(this.originalImage, 0, 0, null);
        graphics.setStroke(new BasicStroke(2));
        for (ImageDetection<?> imageDetection : this.imageDetections) {
            if (MapillaryUtils.checkIfDetectionIsFilteredBasic(detectionLayers, imageDetection)
                || ImageMode.getMode() == ImageMode.SMART_EDIT && detectionLayers.stream()
                    .map(PointObjectLayer::getData).map(VectorDataSet::getSelected).flatMap(Collection::stream)
                    .map(MapillaryMapFeatureUtils::getId).anyMatch(imageDetection.getKey()::equals)) {
                continue;
            }
            final Color color = imageDetection.getColor();
            graphics.setColor(color);
            final Shape transformedShape = unit2CompTransform.createTransformedShape(imageDetection.getShape());
            graphics.draw(transformedShape);
            ImageIcon icon = imageDetection.getValue().getIcon();
            if (imageDetection.isTrafficSign() && icon != null && !icon.equals(ObjectDetections.NO_ICON)) {
                final Rectangle bounds = transformedShape.getBounds();
                // graphics.drawImage(icon.getImage(), bounds.x, bounds.y, bounds.width, bounds.height, null);
                // graphics.drawImage(icon.getImage().getScaledInstance(-1, bounds.height, Image.SCALE_DEFAULT),
                // bounds.x, bounds.y, null);
                graphics.drawImage(icon.getImage(), bounds.x, bounds.y, null);
            }
        }
    }

    @Override
    public void setWidth(int width) {
        // No-op for now
    }

    @Override
    public void setHeight(int height) {
        // No-op for now
    }

    @Override
    @Nullable
    public File getFile() {
        return null;
    }

    @Override
    @Nullable
    public ILatLon getPos() {
        return this.image.getCoor();
    }

    @Override
    @Nullable
    public Double getSpeed() {
        return null;
    }

    @Override
    @Nullable
    public Double getElevation() {
        for (MapillaryImageUtils.ImageProperties property : Arrays.asList(
            MapillaryImageUtils.ImageProperties.COMPUTED_ALTITUDE, MapillaryImageUtils.ImageProperties.ALTITUDE)) {
            if (this.image.hasKey(property.toString())) {
                try {
                    return Double.parseDouble(this.image.get(property.toString()));
                } catch (NumberFormatException e) {
                    Logging.trace(e);
                }
            }
        }
        return null;
    }

    @Override
    @Nonnull
    public Double getExifImgDir() {
        return MapillaryImageUtils.getAngle(this.image);
    }

    @Override
    public boolean hasExifTime() {
        return !Instant.EPOCH.equals(MapillaryImageUtils.getDate(this.image));
    }

    @Override
    @Nonnull
    public Instant getExifInstant() {
        return MapillaryImageUtils.getDate(this.image);
    }

    @Override
    public boolean hasGpsTime() {
        return false;
    }

    @Override
    public Instant getGpsInstant() {
        return null;
    }

    @Override
    public String getIptcCaption() {
        return null;
    }

    @Override
    public String getIptcHeadline() {
        return null;
    }

    @Override
    // null is an option for returning -- this removes the IPTC line in the viewer
    @Nullable
    public List<String> getIptcKeywords() {
        return null;
    }

    @Override
    public String getIptcObjectName() {
        return null;
    }

    /**
     * Update detections for this object
     *
     * @param timeout The timeout to use (milliseconds)
     */
    public void updateDetections(final long timeout) {
        ImageDetection.getDetectionsLaterOptional(MapillaryImageUtils.getKey(this.image), this, timeout);
    }

    @Override
    public void accept(Long key, List<ImageDetection<?>> imageDetections) {
        if (key != null && key == MapillaryImageUtils.getKey(this.image)) {
            this.originalImage = null;
            boolean reload = !this.imageDetections.containsAll(imageDetections)
                || !imageDetections.containsAll(this.imageDetections);
            synchronized (this.imageDetections) {
                if (reload) {
                    this.imageDetections.clear();
                    this.imageDetections.addAll(imageDetections);
                }
            }
            if (reload && this.equals(ImageViewerDialog.getCurrentImage())) {
                // Clone this entry
                final MapillaryImageEntry temporaryImageEntry = new MapillaryImageEntry(this);
                GuiHelper.runInEDT(() -> {
                    ImageViewerDialog.getInstance().displayImage(temporaryImageEntry);
                    ImageViewerDialog.getInstance().displayImage(this);
                });
            }
        }
    }

    @Override
    public Projections getProjectionType() {
        if (MapillaryImageUtils.IS_PANORAMIC.test(this.image)) {
            return Projections.EQUIRECTANGULAR;
        }
        return IImageEntry.super.getProjectionType();
    }
}
