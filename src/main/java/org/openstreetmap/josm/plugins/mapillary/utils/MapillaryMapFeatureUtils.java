// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.util.Locale;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.json.Json;
import jakarta.json.JsonReader;
import org.openstreetmap.josm.data.osm.IPrimitive;
import org.openstreetmap.josm.data.vector.VectorPrimitive;
import org.openstreetmap.josm.plugins.mapillary.cache.Caches;
import org.openstreetmap.josm.plugins.mapillary.oauth.OAuthUtils;
import org.openstreetmap.josm.plugins.mapillary.spi.preferences.MapillaryConfig;
import org.openstreetmap.josm.plugins.mapillary.utils.api.JsonDecoder;
import org.openstreetmap.josm.plugins.mapillary.utils.api.JsonMapObjectDecoder;
import org.openstreetmap.josm.tools.Logging;
import org.openstreetmap.josm.tools.date.DateUtils;

/**
 * Parse point objects (map features) from Mapillary
 */
public final class MapillaryMapFeatureUtils {
    /**
     * v4 API keys for Map Features
     */
    public enum MapFeatureProperties {
        /* Start info in vector layer */
        /** int, ID of the image */
        ID,
        /** string, name of the class which this object represent */
        VALUE,
        /*
         * Start queryable info (seen_at in tiles as timestamp in ms but is in query as a timestamp in
         * YYYY-mm-DDTHH:MM:SS
         * format)
         */
        /**
         * Float, compass direction that the object faces
         */
        ALIGNED_DIRECTION,
        /**
         * int, timestamp in ms since epoch, capture time of the earliest image on which the detection contribute to
         * this
         * map feature, can also be a timestamp (YYYY-MM-DDTHH:MM:SS)
         */
        FIRST_SEEN_AT,
        /**
         * int, timestamp in ms since epoch, capture time of the latest image on which the detection contribute to this
         * map
         * feature, can also be a timestamp (YYYY-MM-DDTHH:MM:SS)
         */
        LAST_SEEN_AT,
        /* End info in vector layer */
        /** string, what kind of map feature it is (seems to be same as {@link #VALUE}, prefer that) */
        OBJECT_VALUE,
        /** string, either a traffic_sign or point, seems to be same as layer, use {@link VectorPrimitive#getLayer()} */
        OBJECT_TYPE,
        /** GeoJSON Point geometry, can be used to update location */
        GEOMETRY,
        /** list of IDs, which images this map feature was derived from */
        IMAGES;
        /* End queryable info */

        @Override
        public String toString() {
            return super.toString().toLowerCase(Locale.ROOT);
        }
    }

    /** For when a map feature doesn't have any image ids */
    private static final long[] EMPTY_IDS = new long[0];

    /** Pattern to use for checking instants */
    private static final Pattern NUMBERS_ONLY = Pattern.compile("^\\d+$");
    /** Pattern to use for checking instants for trailing +0000 */
    private static final Pattern TIMESTAMP_TRAILING_ZONE = Pattern.compile("\\+0{4}$");

    private MapillaryMapFeatureUtils() {
        // Hide constructor
    }

    /**
     * Get the object class of the detection
     *
     * @param primitive The primitive to check
     * @return The object class of the feature
     */
    @Nullable
    public static String getValue(@Nullable final IPrimitive primitive) {
        return getKeyValue(primitive, MapFeatureProperties.VALUE);
    }

    /**
     * Get the time the object was first seen at
     *
     * @param primitive The primitive to check
     * @return The instant it was first seen at, or {@link Instant#MIN}.
     */
    @Nonnull
    public static Instant getFirstSeenAt(@Nullable final IPrimitive primitive) {
        final Instant instant = parseInstant(getKeyValue(primitive, MapFeatureProperties.FIRST_SEEN_AT));
        return instant != null ? instant : Instant.MIN;
    }

    /**
     * Get the time the object was last seen at
     *
     * @param primitive The primitive to check
     * @return the instant it was last seen at, or {@link Instant#MAX}
     */
    @Nonnull
    public static Instant getLastSeenAt(@Nullable final IPrimitive primitive) {
        final Instant instant = parseInstant(getKeyValue(primitive, MapFeatureProperties.LAST_SEEN_AT));
        return instant != null ? instant : Instant.MAX;
    }

    /**
     * Parse an instant from a string
     *
     * @param instantString The string to parse
     * @return An instant from the string
     */
    @Nullable
    private static Instant parseInstant(@Nullable final String instantString) {
        if (instantString != null) {
            if (NUMBERS_ONLY.matcher(instantString).matches()) {
                return Instant.ofEpochMilli(Long.parseLong(instantString));
            }
            final Matcher matcher = TIMESTAMP_TRAILING_ZONE.matcher(instantString);
            return DateUtils.parseInstant(matcher.replaceFirst(""));
        }
        return null;
    }

    /**
     * Get the images the object was seen in
     *
     * @param primitive The primitive to get the image ids for
     * @return The image ids
     */
    @Nonnull
    public static long[] getImageIds(@Nullable final IPrimitive primitive) {
        if (primitive == null) {
            return EMPTY_IDS;
        }
        String images = getKeyValue(primitive, MapFeatureProperties.IMAGES);
        if (images == null) {
            // Synchronize on primitive -- we want to avoid multiple threads getting the metadata
            synchronized (primitive.getStyleCacheSyncObject()) {
                // short-circuit
                images = getKeyValue(primitive, MapFeatureProperties.IMAGES);
                if (images == null) {
                    updateMapFeature(primitive);
                    images = getKeyValue(primitive, MapFeatureProperties.IMAGES);
                }
            }
        }
        if (images == null) {
            return EMPTY_IDS;
        }
        return Stream.of(images.split(",", 0)).filter(str -> NUMBERS_ONLY.matcher(str).matches())
            .mapToLong(Long::parseLong).toArray();
    }

    /**
     * Get the value of a property in a null-safe manner
     *
     * @param primitive The primitive to get
     * @param property The property to get
     * @return The value, if present, or {@code null}
     */
    @Nullable
    private static String getKeyValue(@Nullable final IPrimitive primitive,
        @Nonnull final MapFeatureProperties property) {
        if (primitive != null && primitive.hasKey(property.toString())) {
            return primitive.get(property.toString());
        }
        return null;
    }

    /**
     * Update a map feature with additional information
     *
     * @param primitive The primitive to update
     */
    private static void updateMapFeature(@Nonnull final IPrimitive primitive) {
        final String url = MapillaryConfig.getUrls().getMapFeatureInformation(primitive.getId(),
            MapFeatureProperties.GEOMETRY, MapFeatureProperties.IMAGES, MapFeatureProperties.ALIGNED_DIRECTION);
        final String json = Caches.META_DATA_CACHE.get(url, () -> {
            try {
                return OAuthUtils.getWithHeader(URI.create(url)).toString();
            } catch (IOException e) {
                Logging.error(e);
                return null;
            }
        });
        if (json != null) {
            try (JsonReader reader = Json
                .createReader(new ByteArrayInputStream(json.getBytes(StandardCharsets.UTF_8)))) {
                JsonDecoder.decodeData(reader.readObject(),
                    tjson -> JsonMapObjectDecoder.decodeMapFeatureObject(tjson, primitive));
            }
        }
    }
}
