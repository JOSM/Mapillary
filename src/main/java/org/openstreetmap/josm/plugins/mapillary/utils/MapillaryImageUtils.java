// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import java.io.IOException;
import java.time.Instant;
import java.util.Locale;
import java.util.concurrent.CancellationException;
import java.util.concurrent.CompletableFuture;
import java.util.function.Predicate;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import org.openstreetmap.josm.data.cache.BufferedImageCacheEntry;
import org.openstreetmap.josm.data.cache.CacheEntry;
import org.openstreetmap.josm.data.cache.ICachedLoaderListener;
import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.data.osm.IPrimitive;
import org.openstreetmap.josm.data.osm.IWay;
import org.openstreetmap.josm.plugins.mapillary.cache.CacheUtils;
import org.openstreetmap.josm.plugins.mapillary.cache.Caches;
import org.openstreetmap.josm.plugins.mapillary.cache.MapillaryCache;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.OrganizationRecord;
import org.openstreetmap.josm.plugins.mapillary.gui.workers.MapillaryNodeDownloader;
import org.openstreetmap.josm.tools.Logging;
import org.openstreetmap.josm.tools.UncheckedParseException;
import org.openstreetmap.josm.tools.date.DateUtils;

/**
 * Keys and utility methods for Mapillary Images
 */
public final class MapillaryImageUtils {
    /** The base image url key pattern (v4 sizes are 256, 1024, 2048, and original) */
    public static final Pattern BASE_IMAGE_KEY = Pattern.compile("^thumb_(\\d+|original)_url$");
    // Image specific
    /** Check if the node is for a panoramic image */
    public static final Predicate<INode> IS_PANORAMIC = node -> node != null
        && (MapillaryKeys.PANORAMIC_TRUE.equals(node.get(ImageProperties.IS_PANO.toString()))
            || (!MapillaryKeys.PANORAMIC_FALSE.equals(node.get(ImageProperties.IS_PANO.toString()))
                && ("spherical".equals(node.get(ImageProperties.CAMERA_TYPE.toString()))
                    || "equirectangular".equals(node.get(ImageProperties.CAMERA_TYPE.toString())))));

    /**
     * A pattern to look for strings that are only numbers -- mostly used during switchover from v3 to v4 API
     */
    private static final Pattern NUMBERS = Pattern.compile("\\d+");

    /**
     * Get the sequence for an image
     *
     * @param image the image to get a sequence for
     * @param <N> The node type
     * @return The sequence, if it exists
     */
    @Nullable
    @SuppressWarnings("unchecked") // Only ways with type N should have nodes of type N
    public static <N extends INode> IWay<N> getSequence(@Nullable N image) {
        if (image == null) {
            return null;
        }
        for (IPrimitive p : image.getReferrers()) {
            if (p instanceof IWay) {
                return (IWay<N>) p;
            }
        }
        return null;
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
     * Get a future for an image
     *
     * @param image The node with image information
     * @param cacheType The type of image to cache. May be {@code null}.
     * @return The future with a potential image (image may be {@code null})
     */
    @Nonnull
    public static CompletableFuture<BufferedImageCacheEntry> getImage(@Nonnull INode image,
        @Nullable MapillaryCache.Type cacheType) {
        final MapillaryCache.Type type;
        if (cacheType == null) {
            type = MapillaryCache.Type.getTypeForMemory(image);
        } else {
            type = cacheType;
        }
        if (MapillaryImageUtils.isDownloadable(image)) {
            CompletableFuture<BufferedImageCacheEntry> completableFuture = new CompletableFuture<>();
            CacheUtils.submit(image, type, (entry, attributes, result) -> {
                if (result == ICachedLoaderListener.LoadResult.SUCCESS) {
                    cacheImageFuture(image, completableFuture, entry);
                } else if (result == ICachedLoaderListener.LoadResult.CANCELED) {
                    completableFuture.completeExceptionally(new CancellationException(
                        "Mapillary Image download was cancelled: " + MapillaryImageUtils.getKey(image)));
                } else {
                    completableFuture.completeExceptionally(new IOException(
                        "Mapillary Image could not be downloaded: " + MapillaryImageUtils.getKey(image)));
                }
            });
            return completableFuture;
        } else if (getKey(image) > 0) {
            CompletableFuture<BufferedImageCacheEntry> completableFuture = new CompletableFuture<>();
            new MapillaryNodeDownloader(image, i -> {
                if (MapillaryImageUtils.isDownloadable(i)) {
                    getImage(image, type).thenAccept(completableFuture::complete);
                } else {
                    completableFuture.complete(null);
                }
            }).execute();
            return completableFuture;
        }
        return CompletableFuture.completedFuture(null);
    }

    /**
     * Check if a node is downloadable
     *
     * @param node The node to check
     * @return {@code true} if the node is downloadable
     */
    public static boolean isDownloadable(@Nullable INode node) {
        if (node != null) {
            Matcher matcher = BASE_IMAGE_KEY.matcher("");
            for (String key : node.getKeys().keySet()) {
                matcher.reset(key);
                if (matcher.matches()) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * Complete a future with an entry. This should be called on a separate thread when the entry is ready.
     *
     * @param image The image we are getting
     * @param completableFuture The future to complete when the data is downloaded
     * @param entry The entry to put in the future
     */
    private static void cacheImageFuture(INode image, CompletableFuture<BufferedImageCacheEntry> completableFuture,
        CacheEntry entry) {
        if (entry instanceof BufferedImageCacheEntry) {
            // Using the BufferedImageCacheEntry may speed up processing, if the image is already loaded.
            completableFuture.complete((BufferedImageCacheEntry) entry);
        } else if (entry != null && entry.getContent() != null) {
            // Fall back. More expensive if the image has already been loaded twice.
            completableFuture.complete(new BufferedImageCacheEntry(entry.getContent()));
        } else {
            Caches.META_DATA_CACHE.getICacheAccess().getMatching("/" + getKey(image) + "?").keySet()
                .forEach(Caches.META_DATA_CACHE.getICacheAccess()::remove);
            completableFuture.completeExceptionally(
                new NullPointerException("MapillaryImageUtils#getImage did not have required information"));
        }
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
            // This should always be a parseable integer according to API docs. By not checking that all characters are
            // digits, we save 55.6 MB of allocations in the test area during download.
            if (image.hasKey(ImageProperties.ID.toString())) {
                final long id = Long.parseLong(image.get(ImageProperties.ID.toString()));
                if (id > 0) {
                    image.setOsmId(id, 1);
                }
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
     * Check if the node has one of the Mapillary keys
     *
     * @param node The node to check
     * @return {@code true} if the node is for an image
     */
    public static boolean isImage(@Nullable IPrimitive node) {
        return node instanceof INode && node.hasKey(ImageProperties.ID.toString());
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
     * Check if two images are equal (using their ids/keys)
     *
     * @param imageOne The first image
     * @param imageTwo The second image
     * @return {@code true} if the two images are equal
     */
    public static boolean equals(INode imageOne, INode imageTwo) {
        return getKey(imageOne) == getKey(imageTwo);
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
        ALTITUDE,
        /** The scale of the SfM reconstruction around the image */
        ATOMIC_SCALE,
        /**
         * Focal length, k1, k2
         *
         * @see <a href="https://readthedocs.org/projects/opensfm/downloads/pdf/latest/">OpenSfM for details</a>
         */
        CAMERA_PARAMETERS,
        /**
         * Technically an enum of "perspective", "fisheye", "equirectangular" (or "spherical").
         */
        CAMERA_TYPE,
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
        THUMB_2048_URL,
        /** URL to the original wide thumbnail. You should prefer {@link #BEST_IMAGE} */
        THUMB_ORIGINAL_URL,
        /** Connected component of images aligned together */
        MERGE_CC,
        /** The 3d mesh url */
        MESH,
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
        SEQUENCE_ID,
        /** URL to the point cloud in ply format */
        SFM_CLUSTER,
        /** The original width of the image */
        WIDTH,
        /** Detections from the image (at this time, ids only, docs indicate it is the full detection object) */
        DETECTIONS;

        /** This is the highest quality image known to us at this time. Prefer this to {@link #THUMB_2048_URL}. */
        public static final ImageProperties BEST_IMAGE = THUMB_ORIGINAL_URL;
        /** This is the lowest quality image known to us at this time. Prefer this to {@link #THUMB_256_URL}. */
        public static final ImageProperties WORST_IMAGE = THUMB_256_URL;

        /** The field value that the Mapillary server expects */
        private final String fieldValue;

        ImageProperties() {
            this.fieldValue = super.toString().toLowerCase(Locale.ROOT);
        }

        @Override
        public String toString() {
            return this.fieldValue;
        }
    }
}
