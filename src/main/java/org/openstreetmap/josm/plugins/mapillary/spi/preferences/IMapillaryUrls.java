// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.spi.preferences;

import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
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

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryMapFeatureUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.tools.JosmRuntimeException;
import org.openstreetmap.josm.tools.Logging;

/**
 * An interface with necessary base urls
 *
 * @author Taylor Smock
 */
public interface IMapillaryUrls {
    String FIELDS = "fields";

    /**
     * Get the base metadata URL
     *
     * @return The base metadata URl
     */
    String getBaseMetaDataUrl();

    /**
     * Get the base tile url
     *
     * @return The base tile url
     */
    String getBaseTileUrl();

    /**
     * Get the paint style URL
     *
     * @return The paint style url
     */
    String getPaintStyleUrl();

    /**
     * Get the access id
     *
     * @return The access id
     */
    String getAccessId();

    /**
     * Get the access id in a URL safe manner (the http2 plugin doesn't like {@code "|"} characters)
     *
     * @return The access id
     */
    default String getUrlEncodedAccessId() {
        final String originalAccessId = getAccessId();
        if (originalAccessId == null) {
            return null;
        }
        try {
            // Once we move to Java 10+, we can drop the name() and the catch
            return URLEncoder.encode(originalAccessId, StandardCharsets.UTF_8.name());
        } catch (UnsupportedEncodingException unsupportedEncodingException) {
            // We've hardcoded the UTF-8 charset, so this should never happen unless Java drops UTF_8.
            throw new JosmRuntimeException(unsupportedEncodingException);
        }
    }

    /**
     * Get the client id
     *
     * @return the client id
     */
    long getClientId();

    /**
     * Get the client secret
     *
     * @return The client secret
     */
    String getClientSecret();

    /**
     * The base url for the website
     *
     * @return base url (i.e. www.mapillary.com)
     */
    String getBaseUrl();

    /**
     * Get the Traffic Sign Tile URL
     *
     * @return A URL (String)
     */
    default String getTrafficSigns() {
        return MapillaryConfig.getUrls().getBaseTileUrl() + "mly_map_feature_traffic_sign/2/{z}/{x}/{y}?access_token="
            + getUrlEncodedAccessId();
    }

    /**
     * Get the Object Detection Tile URL
     *
     * @return A URL (String)
     */
    default String getObjectDetections() {
        return MapillaryConfig.getUrls().getBaseTileUrl() + "mly_map_feature_point/2/{z}/{x}/{y}?access_token="
            + getUrlEncodedAccessId();
    }

    /**
     * Get all image/map feature detections
     *
     * @param image The object to get detections for
     * @return The URL to get detections
     */
    default String getDetectionInformation(long image) {
        checkIds(image);
        return MapillaryConfig.getUrls().getBaseMetaDataUrl() + image + "/detections"
            + queryString(Collections.singletonMap(FIELDS, "value,created_at,image,geometry"));
    }

    /**
     * Get the Sequence/Image Tile URL
     *
     * @return A URL (String)
     */
    default String getImages() {
        if (Boolean.TRUE.equals(MapillaryProperties.USE_COMPUTED_LOCATIONS.get())) {
            return MapillaryConfig.getUrls().getBaseTileUrl() + "mly1_computed_public/2/{z}/{x}/{y}?access_token="
                + getUrlEncodedAccessId();
        }
        return MapillaryConfig.getUrls().getBaseTileUrl() + "mly1_public/2/{z}/{x}/{y}?access_token="
            + getUrlEncodedAccessId();
    }

    /**
     * Get specific image information
     *
     * @param images The image
     * @return A URL to get for more image information (default properties ONLY)
     */
    @Nonnull
    default String getImageInformation(long[] images) {
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
    default String getImageInformation(long image, @Nullable MapillaryImageUtils.ImageProperties... properties) {
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
    static MapillaryImageUtils.ImageProperties[] getDefaultImageInformation() {
        return Stream.of(MapillaryImageUtils.ImageProperties.ALTITUDE, MapillaryImageUtils.ImageProperties.ATOMIC_SCALE,
            MapillaryImageUtils.ImageProperties.BEST_IMAGE, MapillaryImageUtils.ImageProperties.CAPTURED_AT,
            MapillaryImageUtils.ImageProperties.CAMERA_TYPE, MapillaryImageUtils.ImageProperties.COMPASS_ANGLE,
            MapillaryImageUtils.ImageProperties.COMPUTED_ALTITUDE,
            MapillaryImageUtils.ImageProperties.COMPUTED_COMPASS_ANGLE,
            MapillaryImageUtils.ImageProperties.COMPUTED_GEOMETRY,
            MapillaryImageUtils.ImageProperties.COMPUTED_ROTATION, MapillaryImageUtils.ImageProperties.EXIF_ORIENTATION,
            MapillaryImageUtils.ImageProperties.GEOMETRY, MapillaryImageUtils.ImageProperties.HEIGHT,
            MapillaryImageUtils.ImageProperties.ID, MapillaryImageUtils.ImageProperties.QUALITY_SCORE,
            MapillaryImageUtils.ImageProperties.SEQUENCE, MapillaryImageUtils.ImageProperties.THUMB_1024_URL,
            MapillaryImageUtils.ImageProperties.THUMB_2048_URL, MapillaryImageUtils.ImageProperties.THUMB_256_URL,
            MapillaryImageUtils.ImageProperties.THUMB_ORIGINAL_URL, MapillaryImageUtils.ImageProperties.WIDTH,
            MapillaryImageUtils.ImageProperties.WORST_IMAGE).distinct().sorted()
            .toArray(MapillaryImageUtils.ImageProperties[]::new);
    }

    /**
     * Retrieve images by sequence
     *
     * @param key The sequence keys
     * @return The URL to get image identifiers with
     */
    default String getImagesBySequences(String key) {
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
    default String getMapFeatureInformation(final long id,
        @Nonnull final MapillaryMapFeatureUtils.MapFeatureProperties... properties) {
        checkIds(id);
        return MapillaryConfig.getUrls().getBaseMetaDataUrl() + id + queryString(Collections.singletonMap(FIELDS,
            Stream.of(properties).map(Object::toString).collect(Collectors.joining(","))));
    }

    /**
     * Get the URL for organization information
     *
     * @param id The organization id
     * @return The URL to get the organization information from
     */
    default String getOrganizationInformation(long id) {
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
    default URI getUserInformation() {
        return string2URL(MapillaryConfig.getUrls().getBaseMetaDataUrl(), "me", queryString(null));
    }

    /**
     * The token url, which is used to obtain login and refresh tokens.
     *
     * @return The token url
     */
    default String getTokenUrl() {
        return MapillaryConfig.getUrls().getBaseMetaDataUrl() + "token";
    }

    /**
     * Gives you the URL which the user should visit to initiate the OAuth authentication process
     *
     * @param redirectURI the URI to which the user will be redirected when the authentication is finished.
     *        When this is <code>null</code>, it's omitted from the query string.
     * @return the URL that the user should visit to start the OAuth authentication
     */
    default URI connect(String redirectURI) {
        // We need to ensure that redirectURI is last
        HashMap<String, String> parts = new LinkedHashMap<>(4);
        parts.put("client_id", Long.toString(getClientId()));
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
        return string2URL(getBaseUrl(), "connect", queryString(parts));
    }

    /**
     * Gives you the URL for the online viewer of a specific mapillary image.
     *
     * @param key the key of the image to which you want to link
     * @return the URI of the online viewer for the image with the given image key
     * @throws IllegalArgumentException if the image key is <code>null</code>
     */
    default URI browseImage(String key) {
        if (key == null) {
            throw new IllegalArgumentException("The image key must not be null!");
        }
        return string2URL(getBaseUrl(), "app/?pKey=", key);
    }

    /**
     * Gives you the URL for the blur editor of the image with the given key.
     *
     * @param key the key of the image for which you want to open the blur editor
     * @return the URL of the blur editor
     * @throws IllegalArgumentException if the image key is <code>null</code>
     */
    default URI blurEditImage(final String key) {
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
        return string2URL(getBaseUrl(), "app/blur?focus=photo&pKey=", urlEncodedKey);
    }

    /**
     * Ensure all ids are &gt; 0
     *
     * @param ids The ids to check
     */
    static void checkIds(long... ids) {
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

    /**
     * Ensure the id is &gt; 0
     *
     * @param id The id to check
     */
    static void checkIds(long id) {
        if (id <= 0) {
            throw new JosmRuntimeException("Mapillary shouldn't have in ids at 0");
        }
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
    static URI string2URL(String... strings) {
        StringBuilder builder = new StringBuilder();
        for (int i = 0; strings != null && i < strings.length; i++) {
            builder.append(strings[i]);
        }
        try {
            if (builder.length() > 0) {
                return new URI(builder.toString());
            }
        } catch (URISyntaxException e) {
            Logging.error("Bad url: {0}", builder.toString());
            Logging.error(e);
        }
        return null;
    }
}
