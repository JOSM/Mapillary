package org.openstreetmap.josm.plugins.mapillary.utils;

import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.util.Collection;
import java.util.Locale;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Future;
import java.util.function.Predicate;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import javax.imageio.ImageIO;
import javax.json.Json;
import javax.json.JsonReader;

import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.data.osm.IPrimitive;
import org.openstreetmap.josm.data.osm.IWay;
import org.openstreetmap.josm.data.vector.VectorNode;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.plugins.mapillary.cache.CacheUtils;
import org.openstreetmap.josm.plugins.mapillary.cache.Caches;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.OrganizationRecord;
import org.openstreetmap.josm.plugins.mapillary.oauth.OAuthUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.api.JsonDecoder;
import org.openstreetmap.josm.plugins.mapillary.utils.api.JsonImageDetailsDecoder;
import org.openstreetmap.josm.tools.Logging;
import org.openstreetmap.josm.tools.UncheckedParseException;
import org.openstreetmap.josm.tools.date.DateUtils;

/**
 * Keys and utility methods for Mapillary Images
 */
public final class MapillaryImageUtils {
    /** The base image url key pattern (v4 sizes are 256, 1024, 2048) */
    public static final Pattern BASE_IMAGE_KEY = Pattern.compile("^thumb_([0-9]+)_url$");
    // Image specific
    /** Check if the node is for a panoramic image */
    public static final Predicate<INode> IS_PANORAMIC = node -> node != null
        && MapillaryKeys.PANORAMIC_TRUE.equals(node.get(ImageProperties.IS_PANO.toString()));

    public static final Predicate<INode> IS_DOWNLOADABLE = node -> node != null
        && node.getKeys().keySet().stream().anyMatch(key -> BASE_IMAGE_KEY.matcher(key).matches());
    public static final String IMPORTED_KEY = "import_file";

    /**
     * A pattern to look for strings that are only numbers -- mostly used during switchover from v3 to v4 API
     *
     * @deprecated Figure out if this needs to be kept
     */
    @Deprecated
    private static final Pattern NUMBERS = Pattern.compile("\\d+");

    /**
     * Get the sequence for an image
     *
     * @param image the image to get a sequence for
     * @return The sequence, if it exists
     */
    @Nullable
    public static IWay<?> getSequence(@Nullable INode image) {
        if (image == null) {
            return null;
        }
        return image.getReferrers().stream().filter(IWay.class::isInstance).map(IWay.class::cast).findFirst()
            .orElse(null);
    }

    /**
     * Get the date an image was created at
     *
     * @param img The image
     * @return The instant the image was created
     */
    @Nonnull
    public static Instant getDate(@Nonnull INode img) {
        if (Instant.EPOCH.equals(img.getInstant()) && !Instant.EPOCH.equals(getCapturedAt(img))) {
            try {
                Instant instant = getCapturedAt(img);
                img.setInstant(instant);
                return instant;
            } catch (NumberFormatException e) {
                Logging.error(e);
            }
        }
        return img.getInstant();
    }

    /**
     * Get the quality score for an image
     *
     * @param img The image to get the quality score for
     * @return The quality score (1, 2, 3, 4, 5, or {@link Float#MIN_VALUE})
     */
    public static double getQuality(@Nonnull INode img) {
        if (img.hasKey(ImageProperties.QUALITY_SCORE.toString())) {
            try {
                return Double.parseDouble(img.get(ImageProperties.QUALITY_SCORE.toString()));
            } catch (final NumberFormatException e) {
                Logging.error(e);
            }
        }
        return Float.MIN_VALUE;
    }

    /**
     * Get the angle for an image
     *
     * @param img The image to get the angle for
     * @return The angle (radians), or {@link Double#NaN}.
     */
    public static double getAngle(@Nonnull INode img) {
        if (Boolean.TRUE.equals(MapillaryProperties.USE_COMPUTED_LOCATIONS.get())
            && img.hasKey(ImageProperties.COMPUTED_COMPASS_ANGLE.toString())) {
            return Math.toRadians(Double.parseDouble(img.get(ImageProperties.COMPUTED_COMPASS_ANGLE.toString())));
        }
        return img.hasKey(ImageProperties.COMPASS_ANGLE.toString())
            ? Math.toRadians(Double.parseDouble(img.get(ImageProperties.COMPASS_ANGLE.toString())))
            : Double.NaN;
    }

    /**
     * Get the file for the image
     *
     * @param img The image to get the file for
     * @return The image file. May be {@code null}.
     */
    @Nullable
    public static File getFile(@Nonnull INode img) {
        return img.hasKey(IMPORTED_KEY) ? new File(img.get(IMPORTED_KEY)) : null;
    }

    /**
     * Get a future for an image
     *
     * @param image The node with image information
     * @return The future with a potential image (image may be {@code null})
     */
    @Nonnull
    public static Future<BufferedImage> getImage(@Nonnull INode image) {
        if (MapillaryImageUtils.IS_DOWNLOADABLE.test(image)) {
            CompletableFuture<BufferedImage> completableFuture = new CompletableFuture<>();
            CacheUtils.submit(image, (entry, attributes, result) -> {
                try {
                    BufferedImage realImage = ImageIO.read(new ByteArrayInputStream(entry.getContent()));
                    completableFuture.complete(realImage);
                } catch (IOException e) {
                    // Remove the key from the metadata cache -- this way we can try again later if the image URL became
                    // stale.
                    Caches.META_DATA_CACHE.getICacheAccess().remove(
                        MapillaryURL.APIv4.getImageInformation(new long[] { MapillaryImageUtils.getKey(image) }));
                    Logging.error(e);
                    completableFuture.complete(null);
                }
            });
            return completableFuture;
        } else if (image.hasKey(IMPORTED_KEY)) {
            return MainApplication.worker.submit(() -> ImageIO.read(new File(image.get(IMPORTED_KEY))));
        }
        return CompletableFuture.completedFuture(null);
    }

    /**
     * Get the captured at time
     *
     * @param image The image to get the captured at time
     * @return The time the image was captured at, or {@link Instant#EPOCH} if not known.
     */
    @Nonnull
    private static Instant getCapturedAt(@Nonnull INode image) {
        String time = "";
        if (image.hasKey(ImageProperties.CAPTURED_AT.toString())) {
            time = image.get(ImageProperties.CAPTURED_AT.toString());
        }
        if (NUMBERS.matcher(time).matches()) {
            return Instant.ofEpochMilli(Long.parseLong(time));
        } else if (!"".equals(time)) {
            try {
                return DateUtils.parseInstant(time);
            } catch (UncheckedParseException e) {
                Logging.error(e);
            }
        }
        return Instant.EPOCH;
    }

    /**
     * Get The key for a node
     *
     * @param image The image
     * @return The key, or {@code 0} if no key exists
     */
    public static long getKey(@Nullable IPrimitive image) {
        return getKey(image, false);
    }

    /**
     * Get a key for an image
     *
     * @param image The image to get the id for
     * @param ignoreId {@code true} to not use the current id and read from the tags
     * @return The key, or {@code 0} if no key exists
     */
    public static long getKey(@Nullable IPrimitive image, boolean ignoreId) {
        if (image != null) {
            if (image.getUniqueId() > 0 && !ignoreId) {
                return image.getId();
            }
            if (image.hasKey(ImageProperties.ID.toString()) && NUMBERS.matcher(image.get(ImageProperties.ID.toString())).matches()) {
                final long id = Long.parseLong(image.get(ImageProperties.ID.toString()));
                image.setOsmId(id, 1);
                return id;
            }
        }
        return 0;
    }

    /**
     * Get the sequence key
     *
     * @param image The image with a sequence key
     * @return The sequence key or {@code null} if no sequence key exists
     */
    @Nullable
    public static String getSequenceKey(@Nullable INode image) {
        if (image != null) {
            if (image.hasKey(ImageProperties.SEQUENCE.toString())) {
                return image.get(ImageProperties.SEQUENCE.toString());
            } else if (image.hasKey(ImageProperties.SEQUENCE_ID.toString())) {
                return image.get(ImageProperties.SEQUENCE_ID.toString());
            }
        }
        return null;
    }

    /**
     * Download image details
     *
     * @param images The image details to download
     */
    public static void downloadImageDetails(@Nonnull Collection<VectorNode> images) {
        downloadImageDetails(images.toArray(new VectorNode[0]));
    }

    /**
     * Download additional image details
     *
     * @param images The image(s) to get additional details for
     */
    public static void downloadImageDetails(@Nonnull VectorNode... images) {
        MapillaryUtils.getForkJoinPool().execute(() -> {
            final long[] keys = Stream.of(images).filter(Objects::nonNull).mapToLong(IPrimitive::getId)
                .filter(key -> key != 0).toArray();
            downloadImageDetails(keys);
        });
    }

    /**
     * Check if the node has one of the Mapillary keys
     *
     * @param node The node to check
     * @return {@code true} if the node is for an image
     */
    public static boolean isImage(@Nullable IPrimitive node) {
        return node instanceof INode
            && (node.hasKey(ImageProperties.ID.toString()) || node.hasKey(MapillaryImageUtils.IMPORTED_KEY));
    }

    /**
     * Get image details for some specific keys
     *
     * @param keys the keys to get details for
     */
    private static void downloadImageDetails(long... keys) {
        Objects.requireNonNull(keys, "Image keys cannot be null");
        for (long key : keys) {
            final String imageUrl = MapillaryURL.APIv4.getImageInformation(key);
            final String cacheData = Caches.META_DATA_CACHE.get(imageUrl, () -> {
                try {
                    return OAuthUtils.getWithHeader(new URL(imageUrl)).toString();
                } catch (IOException e) {
                    Logging.error(e);
                    return null;
                }
            });
            try (JsonReader reader = Json
                .createReader(new ByteArrayInputStream(cacheData.getBytes(StandardCharsets.UTF_8)))) {
                JsonDecoder.decodeData(reader.readObject(), JsonImageDetailsDecoder::decodeImageInfos);
            }
        }
    }

    /**
     * Get the organization for an image
     *
     * @param img The image to get an organization for
     * @return The organization (never null, may be {@link OrganizationRecord#NULL_RECORD}).
     */
    @Nonnull
    public static OrganizationRecord getOrganization(@Nullable INode img) {
        if (img != null) {
            final String organizationKey = ImageProperties.ORGANIZATION_ID.toString();
            if (img.hasKey(organizationKey)) {
                return OrganizationRecord.getOrganization(img.get(organizationKey));
            }
            IWay<?> sequence = getSequence(img);
            if (sequence != null && sequence.hasKey(organizationKey)) {
                return OrganizationRecord.getOrganization(sequence.get(organizationKey));
            }
        }
        return OrganizationRecord.NULL_RECORD;
    }

    private MapillaryImageUtils() {
        /* No op */
    }

    /**
     * Properties for images
     */
    public enum ImageProperties {
        /** The identifier of the image */
        ID,
        /**
         * Original altitude from EXIF
         *
         * @see #COMPUTED_ALTITUDE
         */
        ALTITUDE, ATOMIC_SCALE, CAMERA_PARAMETERS, CAMERA_TYPE,
        /** Timestamp, original capture time */
        CAPTURED_AT,
        /**
         * Original angle
         *
         * @see #COMPUTED_COMPASS_ANGLE
         */
        COMPASS_ANGLE,
        /**
         * Altitude after image processing
         *
         * @see #ALTITUDE
         */
        COMPUTED_ALTITUDE,
        /**
         * Compass angle after image processing
         *
         * @see #COMPASS_ANGLE
         */
        COMPUTED_COMPASS_ANGLE,
        /**
         * Geometry after image processing
         *
         * @see #GEOMETRY
         */
        COMPUTED_GEOMETRY,
        /**
         * Orientation of image after image processing
         *
         * @see #EXIF_ORIENTATION
         */
        COMPUTED_ROTATION,
        /**
         * Original orientation of the image
         *
         * @see #COMPUTED_ROTATION
         */
        EXIF_ORIENTATION,
        /**
         * Original geometry of the image
         *
         * @see #COMPUTED_GEOMETRY
         */
        GEOMETRY,
        /** The original height of the image (int) */
        HEIGHT,
        /** 1 if the image is panoramic */
        IS_PANO,
        /** The id of the organization */
        ORGANIZATION_ID,
        /** A 256px image (max width). You should prefer {@link #WORST_IMAGE}. */
        THUMB_256_URL,
        /** A 1024px image (max width) */
        THUMB_1024_URL,
        /** A 2048px image (max width). You should prefer {@link #BEST_IMAGE}. */
        THUMB_2048_URL, MERGE_CC, MESH,
        /**
         * The quality score of the image (float)
         */
        QUALITY_SCORE,
        /**
         * The sequence key
         *
         * @see #SEQUENCE_ID
         */
        SEQUENCE,
        /**
         * The sequence id (key)
         *
         * @see #SEQUENCE
         */
        SEQUENCE_ID, SFM_CLUSTER,
        /** The original width of the image */
        WIDTH;

        /** This is the highest quality image known to us at this time. Prefer this to {@link #THUMB_2048_URL}. */
        public static final ImageProperties BEST_IMAGE = THUMB_2048_URL;
        /** This is the lowest quality image known to us at this time. Prefer this to {@link #THUMB_256_URL}. */
        public static final ImageProperties WORST_IMAGE = THUMB_256_URL;

        @Override
        public String toString() {
            return super.toString().toLowerCase(Locale.ROOT);
        }
    }
}
