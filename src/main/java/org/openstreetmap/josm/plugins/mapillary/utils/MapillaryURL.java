// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.LongStream;
import java.util.stream.Stream;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import javax.json.Json;
import javax.json.JsonObject;
import javax.json.JsonReader;

import org.openstreetmap.josm.plugins.mapillary.spi.preferences.MapillaryConfig;
import org.openstreetmap.josm.spi.preferences.Config;
import org.openstreetmap.josm.tools.JosmRuntimeException;
import org.openstreetmap.josm.tools.Logging;
import org.openstreetmap.josm.tools.ResourceProvider;
import org.openstreetmap.josm.tools.Utils;

public final class MapillaryURL {
    private static final String FIELDS = "fields";

    private static final class ApiKeyReader {
        private static final String API_KEY_FILE = "mapillary_api_keys.json";

        static String readValue(final String key) {
            // Prefer system property (this is something that can be changed fairly easily)
            if (System.getProperty(key) != null) {
                return System.getProperty(key);
            }
            // Then check if there was something stored in JOSM preferences
            if (Config.getPref() != null && !Utils.isBlank(Config.getPref().get("mapillary.api." + key))) {
                return Config.getPref().get("mapillary.api." + key);
            }
            // Then check and see if the api key file has the key
            final InputStream fileStream = ResourceProvider.getResourceAsStream(API_KEY_FILE);
            if (fileStream != null) {
                try (JsonReader reader = Json.createReader(fileStream)) {
                    final JsonObject object = reader.readObject();
                    if (object.containsKey(key)) {
                        return object.getString(key);
                    }
                } finally {
                    try {
                        fileStream.close();
                    } catch (IOException e) {
                        Logging.error(e);
                    }
                }
            }
            // Finally, return "test" no value has been found
            return "test";
        }

        static long readMapillaryClientId() {
            try {
                final String stringValue = readValue("MAPILLARY_CLIENT_ID");
                return Long.parseLong(stringValue);
            } catch (NumberFormatException numberFormatException) {
                Logging.error(numberFormatException);
            }
            return 4_280_585_711_960_869L;
        }
    }

    /**
     * Mapillary v4 API
     *
     * @author Taylor Smock
     */
    public static final class APIv4 {
        /**
         * The API key for v4
         */
        public static final String ACCESS_ID = ApiKeyReader.readValue("MAPILLARY_CLIENT_TOKEN");
        /**
         * This is the client id for the application, used solely for authentication
         */
        public static final long CLIENT_ID = ApiKeyReader.readMapillaryClientId();

        /**
         * This is the client secret for the application, used solely for authentication
         */
        public static final String CLIENT_SECRET = ApiKeyReader.readValue("MAPILLARY_CLIENT_SECRET");

        /** Default image information to get */
        private static final MapillaryImageUtils.ImageProperties[] DEFAULT_IMAGE_INFORMATION = Stream
            .of(MapillaryImageUtils.ImageProperties.ALTITUDE, MapillaryImageUtils.ImageProperties.ATOMIC_SCALE,
                MapillaryImageUtils.ImageProperties.BEST_IMAGE, MapillaryImageUtils.ImageProperties.CAPTURED_AT,
                MapillaryImageUtils.ImageProperties.CAMERA_TYPE, MapillaryImageUtils.ImageProperties.COMPASS_ANGLE,
                MapillaryImageUtils.ImageProperties.COMPUTED_ALTITUDE,
                MapillaryImageUtils.ImageProperties.COMPUTED_COMPASS_ANGLE,
                MapillaryImageUtils.ImageProperties.COMPUTED_GEOMETRY,
                MapillaryImageUtils.ImageProperties.COMPUTED_ROTATION,
                MapillaryImageUtils.ImageProperties.EXIF_ORIENTATION, MapillaryImageUtils.ImageProperties.GEOMETRY,
                MapillaryImageUtils.ImageProperties.HEIGHT, MapillaryImageUtils.ImageProperties.ID,
                MapillaryImageUtils.ImageProperties.QUALITY_SCORE, MapillaryImageUtils.ImageProperties.SEQUENCE,
                MapillaryImageUtils.ImageProperties.THUMB_1024_URL, MapillaryImageUtils.ImageProperties.THUMB_2048_URL,
                MapillaryImageUtils.ImageProperties.THUMB_256_URL,
                MapillaryImageUtils.ImageProperties.THUMB_ORIGINAL_URL, MapillaryImageUtils.ImageProperties.WIDTH,
                MapillaryImageUtils.ImageProperties.WORST_IMAGE)
            .distinct().sorted().toArray(MapillaryImageUtils.ImageProperties[]::new);

        private APIv4() {
            // Hide constructor
        }

        /**
         * Get the Traffic Sign Tile URL
         *
         * @return A URL (String)
         */
        public static String getTrafficSigns() {
            return MapillaryConfig.getUrls().getBaseTileUrl()
                + "mly_map_feature_traffic_sign/2/{z}/{x}/{y}?access_token=" + ACCESS_ID;
        }

        /**
         * Get the Object Detection Tile URL
         *
         * @return A URL (String)
         */
        public static String getObjectDetections() {
            return MapillaryConfig.getUrls().getBaseTileUrl() + "mly_map_feature_point/2/{z}/{x}/{y}?access_token="
                + ACCESS_ID;
        }

        /**
         * Get all image/map feature detections
         *
         * @param image The object to get detections for
         * @return The URL to get detections
         */
        public static String getDetectionInformation(long image) {
            checkIds(image);
            return MapillaryConfig.getUrls().getBaseMetaDataUrl() + image + "/detections"
                + queryString(Collections.singletonMap(FIELDS, "value,created_at,image,geometry"));
        }

        /**
         * Get the Sequence/Image Tile URL
         *
         * @return A URL (String)
         */
        public static String getImages() {
            if (Boolean.TRUE.equals(MapillaryProperties.USE_COMPUTED_LOCATIONS.get())) {
                return MapillaryConfig.getUrls().getBaseTileUrl() + "mly1_computed_public/2/{z}/{x}/{y}?access_token="
                    + ACCESS_ID;
            }
            return MapillaryConfig.getUrls().getBaseTileUrl() + "mly1_public/2/{z}/{x}/{y}?access_token=" + ACCESS_ID;
        }

        /**
         * Get specific image information
         *
         * @param images The image
         * @return A URL to get for more image information (default properties ONLY)
         */
        @Nonnull
        public static String getImageInformation(long[] images) {
            checkIds(images);
            if (images.length > 1) {
                Map<String, String> queryFields = new HashMap<>(2);
                queryFields.put(FIELDS,
                    Stream.of(getDefaultImageInformation()).map(MapillaryImageUtils.ImageProperties::name)
                        .map(name -> name.toLowerCase(Locale.ROOT)).collect(Collectors.joining(",")));
                queryFields.put("image_ids",
                    LongStream.of(images).mapToObj(Long::toString).collect(Collectors.joining(",")));
                return MapillaryConfig.getUrls().getBaseMetaDataUrl() + "images" + queryString(queryFields);
            }
            return getImageInformation(images[0]);
        }

        /**
         * Get specific image information
         *
         * @param image The image
         * @param properties The specific properties to get -- default properties will be used if {@code null}.
         *        Default properties:
         *        captured_at, altitude, compass angle, geometry, orientation, urls, quality score, and sequence
         * @return A URL to get for more image information
         */
        @Nonnull
        public static String getImageInformation(long image,
            @Nullable MapillaryImageUtils.ImageProperties... properties) {
            final MapillaryImageUtils.ImageProperties[] imageProperties;
            if (properties == null || properties.length == 0) {
                return getImageInformation(image, getDefaultImageInformation());

            } else {
                imageProperties = properties;
            }
            checkIds(image);
            return MapillaryConfig.getUrls().getBaseMetaDataUrl() + image
                + queryString(Collections.singletonMap(FIELDS, Stream.of(imageProperties)
                    .map(MapillaryImageUtils.ImageProperties::toString).collect(Collectors.joining(","))));
        }

        /**
         * Get the default image properties
         *
         * @return The default image properties to get
         */
        private static MapillaryImageUtils.ImageProperties[] getDefaultImageInformation() {
            return DEFAULT_IMAGE_INFORMATION;
        }

        /**
         * Retrieve images by sequence
         *
         * @param key The sequence keys
         * @return The URL to get image identifiers with
         */
        public static String getImagesBySequences(String key) {
            return MapillaryConfig.getUrls().getBaseMetaDataUrl() + "image_ids"
                + queryString(Collections.singletonMap("sequence_id", key));
        }

        /**
         * Get additional map feature information
         *
         * @param id The id of the map feature
         * @param properties The properties to get
         * @return The URL to get additional information with
         */
        public static String getMapFeatureInformation(final long id,
            @Nonnull final MapillaryMapFeatureUtils.MapFeatureProperties... properties) {
            checkIds(id);
            return MapillaryConfig.getUrls().getBaseMetaDataUrl() + id + queryString(Collections.singletonMap(FIELDS,
                Stream.of(properties).map(Object::toString).collect(Collectors.joining(","))));
        }

        /**
         * Get the URL for organization information
         *
         * @param id The organization id
         */
        public static String getOrganizationInformation(long id) {
            // profile_photo_url for profile photos, possibly (currently not public -- errors out) TODO
            checkIds(id);
            return MapillaryConfig.getUrls().getBaseMetaDataUrl() + id
                + queryString(Collections.singletonMap(FIELDS, "slug,name,description"));
        }

        /**
         * Get user information for the current user
         *
         * @return The URL to get user information (logged in user only)
         */
        public static URL getUserInformation() {
            return string2URL(MapillaryConfig.getUrls().getBaseMetaDataUrl(), "me", queryString(null));
        }

        /**
         * The token url, which is used to obtain login and refresh tokens.
         *
         * @return The token url
         */
        public static String getTokenUrl() {
            return MapillaryConfig.getUrls().getBaseMetaDataUrl() + "token";
        }

        private static void checkIds(long... ids) {
            if (ids == null || ids.length == 0) {
                throw new JosmRuntimeException("Mapillary ids are not set");
            }
            if (ids.length == 1) {
                checkIds(ids[0]);
            } else {
                for (long id : ids) {
                    checkIds(id);
                }
            }
        }

        private static void checkIds(long id) {
            if (id <= 0) {
                throw new JosmRuntimeException("Mapillary shouldn't have in ids at 0");
            }
        }
    }

    public static final class MainWebsite {
        static String baseUrl = "https://www.mapillary.com/";

        private MainWebsite() {
            // Private constructor to avoid instantiation
        }

        /**
         * Gives you the URL for the online viewer of a specific mapillary image.
         *
         * @param key the key of the image to which you want to link
         * @return the URL of the online viewer for the image with the given image key
         * @throws IllegalArgumentException if the image key is <code>null</code>
         */
        public static URL browseImage(String key) {
            if (key == null) {
                throw new IllegalArgumentException("The image key must not be null!");
            }
            return string2URL(baseUrl, "app/?pKey=", key);
        }

        /**
         * Gives you the URL for the blur editor of the image with the given key.
         *
         * @param key the key of the image for which you want to open the blur editor
         * @return the URL of the blur editor
         * @throws IllegalArgumentException if the image key is <code>null</code>
         */
        public static URL blurEditImage(final String key) {
            if (key == null) {
                throw new IllegalArgumentException("The image key must not be null!");
            }
            String urlEncodedKey;
            try {
                urlEncodedKey = URLEncoder.encode(key, StandardCharsets.UTF_8.name());
            } catch (UnsupportedEncodingException e) {
                Logging.log(Logging.LEVEL_ERROR, "Unsupported encoding when URL encoding", e);
                urlEncodedKey = key;
            }
            return string2URL(baseUrl, "app/blur?focus=photo&pKey=", urlEncodedKey);
        }

        /**
         * Gives you the URL which the user should visit to initiate the OAuth authentication process
         *
         * @param redirectURI the URI to which the user will be redirected when the authentication is finished.
         *        When this is <code>null</code>, it's omitted from the query string.
         * @return the URL that the user should visit to start the OAuth authentication
         */
        public static URL connect(String redirectURI) {
            // We need to ensure that redirectURI is last
            HashMap<String, String> parts = new LinkedHashMap<>(4);
            parts.put("client_id", Long.toString(APIv4.CLIENT_ID));
            // Only code is supported
            parts.put("response_type", "code");
            // Scopes are read, write, upload
            parts.put("scope", "read");
            // TODO state should probably be added/generated sometime
            // redirect URI should be added last
            if (redirectURI != null && redirectURI.length() >= 1) {
                // Must be the same as the callback URL registered with Mapillary
                parts.put("redirect_uri", redirectURI);
            }
            return string2URL(baseUrl, "connect", queryString(parts));
        }
    }

    private MapillaryURL() {
        // Private constructor to avoid instantiation
    }

    /**
     * Builds a query string from its parts that are supplied as a {@link Map}
     *
     * @param parts the parts of the query string
     * @return the constructed query string (including a leading ?)
     */
    static String queryString(Map<String, String> parts) {
        if (parts != null) {
            return parts.entrySet().stream().map(entry -> {
                try {
                    return String.join("=", URLEncoder.encode(entry.getKey(), StandardCharsets.UTF_8.name()),
                        URLEncoder.encode(entry.getValue(), StandardCharsets.UTF_8.name()));
                } catch (UnsupportedEncodingException e) {
                    Logging.error(e); // This should not happen, as the encoding is hard-coded
                    return null;
                }
            }).filter(Objects::nonNull).collect(Collectors.joining("&", "?", ""));
        }
        return "";
    }

    /**
     * Converts a {@link String} into a {@link URL} without throwing a {@link MalformedURLException}.
     * Instead, such an exception will lead to an {@link Logging#error(Throwable)}.
     * So you should be very confident that your URL is well-formed when calling this method.
     *
     * @param strings the Strings describing the URL
     * @return the URL that is constructed from the given string
     */
    static URL string2URL(String... strings) {
        StringBuilder builder = new StringBuilder();
        for (int i = 0; strings != null && i < strings.length; i++) {
            builder.append(strings[i]);
        }
        try {
            return new URL(builder.toString());
        } catch (MalformedURLException e) {
            Logging.log(Logging.LEVEL_ERROR, String.format("The class '%s' produces malformed URLs like '%s'!",
                MapillaryURL.class.getName(), builder), e);
            return null;
        }
    }
}
