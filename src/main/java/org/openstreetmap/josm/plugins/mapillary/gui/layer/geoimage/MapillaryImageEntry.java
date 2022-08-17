// License: GPL. For details, see LICENSE file.
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
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.lang.ref.SoftReference;
import java.net.URI;
import java.net.URISyntaxException;
import java.text.DateFormat;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.TimeZone;
import java.util.concurrent.CompletableFuture;
import java.util.function.BiConsumer;
import java.util.stream.Collectors;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import javax.swing.ImageIcon;
import javax.swing.SwingUtilities;

import com.drew.imaging.jpeg.JpegMetadataReader;
import com.drew.imaging.jpeg.JpegProcessingException;
import com.drew.metadata.Directory;
import com.drew.metadata.Metadata;
import com.drew.metadata.MetadataException;
import com.drew.metadata.exif.ExifIFD0Directory;
import org.apache.commons.jcs3.access.CacheAccess;
import org.openstreetmap.josm.actions.ExpertToggleAction;
import org.openstreetmap.josm.data.cache.BufferedImageCacheEntry;
import org.openstreetmap.josm.data.cache.JCSCacheManager;
import org.openstreetmap.josm.data.coor.ILatLon;
import org.openstreetmap.josm.data.imagery.street_level.IImageEntry;
import org.openstreetmap.josm.data.imagery.street_level.Projections;
import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.data.osm.IPrimitive;
import org.openstreetmap.josm.data.preferences.AbstractProperty;
import org.openstreetmap.josm.data.vector.VectorDataSet;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.layer.geoimage.ImageViewerDialog;
import org.openstreetmap.josm.gui.util.GuiHelper;
import org.openstreetmap.josm.plugins.mapillary.cache.MapillaryCache;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.ImageMode;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.MapillaryNode;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.ObjectDetections;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.OrganizationRecord;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.PointObjectLayer;
import org.openstreetmap.josm.plugins.mapillary.model.ImageDetection;
import org.openstreetmap.josm.plugins.mapillary.model.KeyIndexedObject;
import org.openstreetmap.josm.plugins.mapillary.spi.preferences.MapillaryConfig;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillarySequenceUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.OffsetUtils;
import org.openstreetmap.josm.tools.ExifReader;
import org.openstreetmap.josm.tools.JosmRuntimeException;
import org.openstreetmap.josm.tools.Logging;
import org.openstreetmap.josm.tools.Utils;
import org.openstreetmap.josm.tools.date.DateUtils;

/**
 * A container to show Mapillary images in JOSM
 *
 * @author Taylor Smock
 */
public class MapillaryImageEntry
    implements IImageEntry<MapillaryImageEntry>, BiConsumer<Long, Collection<ImageDetection<?>>> {
    private static final CacheAccess<Long, MapillaryImageEntry> CACHE = JCSCacheManager
        .getCache("mapillary:mapillaryimageentry");
    private static final String BASE_TITLE = marktr("Mapillary image");
    private static final String MESSAGE_SEPARATOR = " — ";
    private final INode image;
    private final List<ImageDetection<?>> imageDetections = new ArrayList<>();
    private SoftReference<BufferedImageCacheEntry> originalImage;
    private SoftReference<BufferedImage> layeredImage;
    private int exifOrientation = 1;
    private boolean fullImage;

    private static class MapillaryValueChangeListener implements AbstractProperty.ValueChangeListener<Boolean> {
        static final MapillaryValueChangeListener INSTANCE = new MapillaryValueChangeListener();

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
        MapillaryProperties.SHOW_DETECTED_SIGNS.addListener(MapillaryValueChangeListener.INSTANCE);
        MapillaryProperties.SHOW_DETECTION_OUTLINES.addListener(MapillaryValueChangeListener.INSTANCE);
    }

    /**
     * Get a cached instance of an image, if available
     *
     * @param image The image to get
     * @return The cached instance, or a new object
     */
    public static MapillaryImageEntry getCachedEntry(final INode image) {
        Objects.requireNonNull(image);
        final long id = MapillaryImageUtils.getKey(image);
        if (id > 0) {
            MapillaryImageEntry entry = CACHE.get(id, () -> new MapillaryImageEntry(image));
            if (entry.image.getNumKeys() <= image.getNumKeys()
                || image != entry.image /* Object reference equality */) {
                CACHE.remove(id);
                entry = CACHE.get(id, () -> new MapillaryImageEntry(image));
            }
            return entry;
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
        if (MapillaryImageUtils.getKey(image) <= 0) {
            throw new IllegalArgumentException("We can only show images with an actual id to get the download");
        }
        if (!(image instanceof MapillaryNode)) {
            throw new IllegalStateException("Image not downloaded");
        }
        this.image = image;
    }

    /**
     * Clone another entry. Mostly useful to avoid != checks. See {@link ImageViewerDialog#displayImages(List)} for a
     * specific location.
     *
     * @param other The entry to clone
     */
    MapillaryImageEntry(final MapillaryImageEntry other) {
        this.image = other.image;
        this.imageDetections.addAll(other.imageDetections);
        this.layeredImage = other.layeredImage;
        this.originalImage = other.originalImage;
        this.fullImage = other.fullImage;
        this.exifOrientation = other.exifOrientation;
    }

    @Override
    public MapillaryImageEntry getNextImage() {
        return Optional
            .ofNullable(
                MapillarySequenceUtils.getNextOrPrevious(this.image, MapillarySequenceUtils.NextOrPrevious.NEXT))
            .map(MapillaryImageEntry::getCachedEntry).orElse(null);
    }

    @Override
    public MapillaryImageEntry getPreviousImage() {
        return Optional
            .ofNullable(
                MapillarySequenceUtils.getNextOrPrevious(this.image, MapillarySequenceUtils.NextOrPrevious.PREVIOUS))
            .map(MapillaryImageEntry::getCachedEntry).orElse(null);
    }

    @Override
    public MapillaryImageEntry getFirstImage() {
        return Optional.ofNullable(MapillaryImageUtils.getSequence(this.image))
            .map(way -> way.getNodes().stream().filter(n -> n.getUniqueId() > 0).findFirst().orElse(this.image))
            .map(MapillaryImageEntry::getCachedEntry).orElse(null);
    }

    @Override
    public MapillaryImageEntry getLastImage() {
        return Optional.ofNullable(MapillaryImageUtils.getSequence(this.image))
            .flatMap(way -> way.getNodes().stream().filter(MapillaryImageUtils::isImage)
                .reduce((first, second) -> second).map(MapillaryImageEntry::getCachedEntry))
            .orElse(null);
    }

    @Override
    public void selectImage(ImageViewerDialog imageViewerDialog, IImageEntry<?> entry) {
        IImageEntry.super.selectImage(imageViewerDialog, entry);
        if (entry instanceof MapillaryImageEntry) {
            selectImage((MapillaryImageEntry) entry);
        }
    }

    private static void selectImage(@Nullable final MapillaryImageEntry entry) {
        if (entry != null) {
            MapillaryLayer.getInstance().getData().setSelected(entry.image);
            MapillaryLayer.getInstance().invalidate();
        }
    }

    @Override
    public String getDisplayName() {
        StringBuilder title = new StringBuilder(tr(BASE_TITLE));
        if (MapillaryImageUtils.getKey(this.image) != 0) {
            INode mapillaryImage = this.image;
            OrganizationRecord organizationRecord = MapillaryImageUtils.getOrganization(mapillaryImage);
            if (!OrganizationRecord.NULL_RECORD.equals(organizationRecord)) {
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
        }
        return title.toString();
    }

    @Override
    public BufferedImage read(Dimension target) throws IOException {
        if (SwingUtilities.isEventDispatchThread()) {
            throw new JosmRuntimeException(tr("Mapillary image read should never occur on UI thread"));
        }
        BufferedImageCacheEntry bufferedImageCacheEntry = Optional.ofNullable(this.originalImage)
            .map(SoftReference::get).orElse(null);
        boolean tFullImage = this.fullImage;
        CompletableFuture<BufferedImageCacheEntry> bestForMemory;
        if (tFullImage) {
            bestForMemory = CompletableFuture.completedFuture(bufferedImageCacheEntry);
        } else {
            bestForMemory = MapillaryImageUtils.getImage(this.image, null).thenApplyAsync(this::setFullImage,
                MapillaryUtils.getForkJoinPool());
        }
        if (bufferedImageCacheEntry == null) {
            CompletableFuture<BufferedImageCacheEntry> quickLoad = MapillaryImageUtils
                .getImage(this.image, MapillaryCache.Type.THUMB_256)
                .thenApplyAsync(this::setQuickImage, MapillaryUtils.getForkJoinPool());
            try {
                // This should only every be called on a non-UI thread.
                synchronized (this) {
                    while ((this.originalImage == null || this.originalImage.get() == null)
                        && !(quickLoad.isDone() || quickLoad.isCancelled())
                        && !(bestForMemory.isDone() || bestForMemory.isCancelled())) {
                        this.wait(10);
                    }
                    if (this.originalImage == null || this.originalImage.get() == null) {
                        throw new IOException(tr("Could not load image: {0}", this.getImageURI()));
                    }
                    bufferedImageCacheEntry = this.originalImage.get();
                    tFullImage = this.fullImage;
                }
                MapillaryCache.cacheSurroundingImages(this.image);
            } catch (InterruptedException exception) {
                Thread.currentThread().interrupt();
                throw new IOException(exception);
            }
            if (bufferedImageCacheEntry == null) {
                // This should never be hit
                return null;
            }
        }
        if (tFullImage && this.imageDetections.isEmpty() && !this.image.getReferrers().isEmpty()) {
            this.updateDetections(5_000);
            return this.applyRotation(bufferedImageCacheEntry.getImage());
        }
        BufferedImage bufferedLayeredImage = Optional.ofNullable(this.layeredImage).map(SoftReference::get)
            .orElse(null);
        if (bufferedLayeredImage == null) {
            bufferedLayeredImage = new BufferedImage(bufferedImageCacheEntry.getImage().getWidth(),
                bufferedImageCacheEntry.getImage().getHeight(), bufferedImageCacheEntry.getImage().getType());
            this.layeredImage = new SoftReference<>(bufferedLayeredImage);
        }
        this.drawDetections();
        return this.applyRotation(bufferedLayeredImage);
    }

    private BufferedImageCacheEntry setQuickImage(final BufferedImageCacheEntry entry) {
        // Load image
        try {
            entry.getImage();
        } catch (IOException e) {
            Logging.error(e);
        }
        synchronized (this) {
            if (this.originalImage == null || this.originalImage.get() == null) {
                this.originalImage = new SoftReference<>(entry);
                this.fullImage = false;
            }
            this.notifyAll();
        }
        return entry;
    }

    private BufferedImageCacheEntry setFullImage(final BufferedImageCacheEntry entry) {
        final byte[] content = entry.getContent();
        // Load image
        try {
            entry.getImage();
        } catch (IOException e) {
            Logging.error(e);
        }
        synchronized (this) {
            final boolean forceRefresh = this.layeredImage != null;
            this.originalImage = new SoftReference<>(entry);
            if (forceRefresh) {
                this.layeredImage.clear();
            }
            if (content.length > 0) {
                this.updateExifInformation(content);
            }
            this.fullImage = true;
            if (forceRefresh) {
                this.reload();
            }
            this.notifyAll();
        }
        return entry;
    }

    private void drawDetections() throws IOException {
        final BufferedImage bufferedLayeredImage = Optional.ofNullable(this.layeredImage).map(SoftReference::get)
            .orElse(null);
        final BufferedImageCacheEntry originalCacheEntry = Optional.ofNullable(this.originalImage)
            .map(SoftReference::get).orElse(null);
        if (originalCacheEntry == null || bufferedLayeredImage == null) {
            this.read(null);
            return;
        }
        final int width = bufferedLayeredImage.getWidth();
        final int height = bufferedLayeredImage.getHeight();
        List<PointObjectLayer> detectionLayers = MainApplication.getLayerManager()
            .getLayersOfType(PointObjectLayer.class);
        final AffineTransform unit2CompTransform = AffineTransform.getTranslateInstance(0, 0);
        unit2CompTransform.concatenate(AffineTransform.getScaleInstance(width, height));

        final Graphics2D graphics = bufferedLayeredImage.createGraphics();
        // Draw the original image first
        graphics.drawImage(originalCacheEntry.getImage(), 0, 0, null);
        // Then get the affine transform for the exif orientation
        int currentExifOrientation = getExifOrientation();
        if (ExifReader.orientationNeedsCorrection(currentExifOrientation)) {
            AffineTransform transform = ExifReader.getRestoreOrientationTransform(currentExifOrientation,
                bufferedLayeredImage.getWidth(), bufferedLayeredImage.getHeight());
            graphics.setTransform(transform);
        }
        // Then draw the detections
        graphics.setStroke(new BasicStroke(2));
        for (ImageDetection<?> imageDetection : this.imageDetections) {
            if (MapillaryUtils.checkIfDetectionIsFilteredBasic(detectionLayers, imageDetection)
                && (ImageMode.getMode() != ImageMode.SMART_EDIT
                    || !checkIfDetectionInImageAndSelected(detectionLayers, imageDetection))) {
                continue;
            }
            final Color color = imageDetection.getColor();
            graphics.setColor(color);
            final Shape transformedShape = unit2CompTransform.createTransformedShape(imageDetection.getShape());
            graphics.draw(transformedShape);
            ImageIcon icon = imageDetection.getValue().getIcon();
            if (imageDetection.isTrafficSign() && !icon.equals(ObjectDetections.NO_ICON)) {
                final Rectangle bounds = transformedShape.getBounds();
                // graphics.drawImage(icon.getImage(), bounds.x, bounds.y, bounds.width, bounds.height, null);
                // graphics.drawImage(icon.getImage().getScaledInstance(-1, bounds.height, Image.SCALE_DEFAULT),
                // bounds.x, bounds.y, null);
                graphics.drawImage(icon.getImage(), bounds.x, bounds.y, null);
            }
        }
        graphics.dispose();
    }

    /**
     * Check if a detection is selected
     *
     * @param detectionLayers The layers to look at
     * @param detection The detection to check
     * @return {@code true} if the detection is selected
     */
    private static boolean checkIfDetectionInImageAndSelected(List<PointObjectLayer> detectionLayers,
        ImageDetection<?> detection) {
        Set<ImageDetection<?>> selectedDetections = detectionLayers.stream().map(PointObjectLayer::getData)
            .map(VectorDataSet::getSelected).flatMap(Collection::stream).mapToLong(IPrimitive::getId)
            .mapToObj(l -> ImageDetection.getDetections(l, ImageDetection.Options.WAIT)).flatMap(Collection::stream)
            .collect(Collectors.toSet());
        return selectedDetections.stream().mapToLong(KeyIndexedObject::getKey).anyMatch(l -> l == detection.getKey());
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
        return new File(MapillaryConfig.getUrls().browseImage(Long.toString(MapillaryImageUtils.getKey(this.image)))
            .toExternalForm());
    }

    // @Override -- added in JOSM r18427
    public URI getImageURI() {
        try {
            return new URI(MapillaryConfig.getUrls().browseImage(Long.toString(MapillaryImageUtils.getKey(this.image)))
                .toExternalForm());
        } catch (URISyntaxException e) {
            // This should never happen.
            throw new JosmRuntimeException(this.toString(), e);
        }
    }

    @Override
    @Nullable
    public ILatLon getPos() {
        return OffsetUtils.getOffsetLocation(this.image);
    }

    @Override
    @Nullable
    public Double getSpeed() {
        return null;
    }

    @Override
    @Nullable
    public Double getElevation() {
        // Sometimes the computed altitude is very wrong. See JOSM #21871
        try {
            final Double computedAltitude = Optional
                .ofNullable(this.image.get(MapillaryImageUtils.ImageProperties.COMPUTED_ALTITUDE.toString()))
                .map(Double::parseDouble).orElse(null);
            final Double originalAltitude = Optional
                .ofNullable(this.image.get(MapillaryImageUtils.ImageProperties.ALTITUDE.toString()))
                .map(Double::parseDouble).orElse(null);
            // Assume that the max hdop is 3m, and vdop is 3x that.
            if (computedAltitude != null && originalAltitude != null) {
                if (Math.abs(computedAltitude - originalAltitude) < 3 * MapillaryProperties.ASSUMED_HDOP.get()) {
                    return computedAltitude;
                }
                return originalAltitude;
            }
            // Fall back to whichever is not null, preferring computedAltitude
            return Utils.firstNonNull(computedAltitude, originalAltitude);
        } catch (NumberFormatException e) {
            // Experts hopefully know how to file a bug report.
            if (ExpertToggleAction.isExpert()) {
                throw e;
            }
            Logging.debug(e);
        }
        return null;
    }

    @Override
    @Nonnull
    public Double getExifImgDir() {
        return Math.toDegrees(MapillaryImageUtils.getAngle(this.image));
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
    public void accept(Long key, Collection<ImageDetection<?>> imageDetections) {
        if (key != null && key == MapillaryImageUtils.getKey(this.image)) {
            boolean reload = !new HashSet<>(this.imageDetections).containsAll(imageDetections)
                || !imageDetections.containsAll(this.imageDetections);
            synchronized (this.imageDetections) {
                if (reload) {
                    if (this.layeredImage != null) {
                        this.layeredImage.clear();
                    }
                    this.imageDetections.clear();
                    this.imageDetections.addAll(imageDetections);
                }
            }
            if (reload) {
                this.updateImageEntry();
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

    /**
     * Refresh the image in the image viewer
     */
    public void reload() {
        this.updateImageEntry();
    }

    private void updateImageEntry() {
        // Clone this entry. Needed to ensure that the image display refreshes.
        final MapillaryImageEntry temporaryImageEntry = new MapillaryImageEntry(this);
        // Ensure that detections are repainted
        if (this.layeredImage != null) {
            this.layeredImage.clear();
        }
        if (this.equals(ImageViewerDialog.getCurrentImage())) {
            GuiHelper.runInEDT(() -> {
                ImageViewerDialog.getInstance().displayImage(temporaryImageEntry);
                ImageViewerDialog.getInstance().displayImage(this);
            });
        }
    }

    // FIXME copied from ImageEntry
    private BufferedImage applyRotation(BufferedImage img) {
        int currentExifOrientation = getExifOrientation();
        if (!ExifReader.orientationNeedsCorrection(currentExifOrientation)) {
            return img;
        }
        boolean switchesDimensions = ExifReader.orientationSwitchesDimensions(currentExifOrientation);
        int width = switchesDimensions ? img.getHeight() : img.getWidth();
        int height = switchesDimensions ? img.getWidth() : img.getHeight();
        BufferedImage rotated = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
        AffineTransform transform = ExifReader.getRestoreOrientationTransform(currentExifOrientation, img.getWidth(),
            img.getHeight());
        Graphics2D g = rotated.createGraphics();
        g.drawImage(img, transform, null);
        g.dispose();
        return rotated;
    }

    /**
     * Update exif information based off of the byte buffer
     *
     * @param imageBytes The image bytes
     */
    private void updateExifInformation(byte[] imageBytes) {
        try {
            final Metadata metadata = JpegMetadataReader.readMetadata(new ByteArrayInputStream(imageBytes));
            final Directory dirExif = metadata.getFirstDirectoryOfType(ExifIFD0Directory.class);
            try {
                if (dirExif != null && dirExif.containsTag(ExifIFD0Directory.TAG_ORIENTATION)) {
                    setExifOrientation(dirExif.getInt(ExifIFD0Directory.TAG_ORIENTATION));
                }
            } catch (MetadataException ex) {
                Logging.debug(ex);
            }
        } catch (JpegProcessingException | IOException e) {
            Logging.error(e);
        }
    }

    private void setExifOrientation(int exifOrientation) {
        this.exifOrientation = exifOrientation;
    }

    private int getExifOrientation() {
        return this.exifOrientation;
    }
}
