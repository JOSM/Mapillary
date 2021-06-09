// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.tools.Logging;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

public final class MapillaryURL {
  /** The API key for v3 */
  private static final String CLIENT_ID = "UTZhSnNFdGpxSEFFREUwb01GYzlXZzpjNGViMzQxMTIzMjY0MjZm";
  /**
   * The API key for v4 -- cannot be final since tests need to change it (the '|' characters kill WireMock's pattern
   * replacement)
   */
  private static String ACCESS_ID = "MLY|4280585711960869|80f129f78b6052f608d172c467b1d047";

  /**
   * Mapillary v4 API
   *
   * @author Taylor Smock
   */
  public static final class APIv4 {
    private APIv4() {
      // Hide constructor
    }

    /** The base URL to get metadata */
    private static String baseMetaDataUrl = "https://graph.mapillary.com/";
    /** The base URL for tiles */
    private static String baseTileUrl = "https://facebook.com/maps/vtp/";

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
       * @see #SEQUENCE_ID
       */
      SEQUENCE,
      /**
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

    /**
     * Get the Traffic Sign Tile URL
     *
     * @return A URL (String)
     */
    public static String getTrafficSigns() {
      return baseTileUrl + "mly_map_feature_traffic_sign/12/{z}/{x}/{y}" + queryString(null);
    }

    /**
     * Get the Object Detection Tile URL
     *
     * @return A URL (String)
     */
    public static String getObjectDetections() {
      return baseTileUrl + "mly_map_feature_point/12/{z}/{x}/{y}" + queryString(null);
    }

    /**
     * Get the Sequence/Image Tile URL
     *
     * @return A URL (String)
     */
    public static String getImages() {
      if (Boolean.TRUE.equals(MapillaryProperties.USE_COMPUTED_LOCATIONS.get())) {
        return baseTileUrl + "mly1_computed_public/12/{z}/{x}/{y}" + queryString(null);
      }
      return baseTileUrl + "mly1_public/12/{z}/{x}/{y}" + queryString(null);
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
    public static String getImageInformation(@Nonnull String image, @Nullable ImageProperties... properties) {
      final ImageProperties[] imageProperties;
      if (properties == null || properties.length == 0) {
        final boolean getComputed = Boolean.TRUE.equals(MapillaryProperties.USE_COMPUTED_LOCATIONS.get());
        return getImageInformation(image, ImageProperties.ID, ImageProperties.CAPTURED_AT,
          getComputed ? ImageProperties.COMPUTED_ALTITUDE : ImageProperties.ALTITUDE,
          getComputed ? ImageProperties.COMPUTED_COMPASS_ANGLE : ImageProperties.COMPASS_ANGLE,
          getComputed ? ImageProperties.COMPUTED_GEOMETRY : ImageProperties.GEOMETRY,
          getComputed ? ImageProperties.COMPUTED_ROTATION : ImageProperties.EXIF_ORIENTATION,
          ImageProperties.WORST_IMAGE, ImageProperties.BEST_IMAGE, ImageProperties.QUALITY_SCORE,
          ImageProperties.SEQUENCE);

      } else {
        imageProperties = properties;
      }
      return new StringBuilder(baseMetaDataUrl)
        .append(image).append(queryString(Collections.singletonMap("fields", Stream.of(imageProperties)
          .map(ImageProperties::name).map(name -> name.toLowerCase(Locale.ROOT)).collect(Collectors.joining(",")))))
        .toString();
    }

    /**
     * Retrieve images by sequence
     *
     * @param key The sequence keys
     * @return The URL to get image identifiers with
     */
    public static String getImagesBySequences(String key) {
      return new StringBuilder(baseMetaDataUrl).append("image_ids")
        .append(queryString(Collections.singletonMap("sequence_id", key))).toString();
    }

    /**
     * Get all image detections
     *
     * @param image The image to get detections for
     * @return The URL to get detections
     */
    private static String getImageDetections(String image) {
      // TODO get the geometry?
      return baseMetaDataUrl + image + "/detections"
        + queryString(Collections.singletonMap("fields", "value,created_at"));
    }

    /**
     * Get user information for the current user
     *
     * @return The URL to get user information (logged in user only)
     */
    public static URL getUserInformation() {
      return string2URL(baseMetaDataUrl, "me", queryString(null));
    }

    /**
     * Get the user's organizations
     *
     * @return The URL to get user orgs from
     */
    private static URL getUserOrganizations() {
      return string2URL(baseMetaDataUrl, "me", "organizations", queryString(null));
    }
  }

  public static final class APIv3 {
    static String baseUrl = "https://a.mapillary.com/v3/";

    private APIv3() {
      // Private constructor to avoid instantiation
    }

    public static URL getUser(String key) {
      return string2URL(baseUrl, "users/", key, MapillaryURL.queryString(null));
    }

    public static URL retrieveOrganizationss(String user) {
      return string2URL(baseUrl, "users/" + user + "/organizations", queryString(null));
    }

    public static URL retrieveOrganization(String organization) {
      return string2URL(baseUrl, "organizations/" + organization, queryString(null));
    }

    /**
     * Attempt to get an organization URL for the avatar
     *
     * @param organization The key to get the avatar from
     * @return A URL to get data from
     */
    public static URL retrieveOrganizationAvatar(String organization) {
      return string2URL(baseUrl, "organizations/" + organization + "/avatar", queryString(null));
    }

    /**
     * Get the upload secrets URL
     *
     * @return the URL where you'll find the upload secrets as JSON
     */
    public static URL uploadSecretsURL() {
      return string2URL(baseUrl, "me/uploads", queryString(null));
    }

    static String queryString(final Bounds bounds) {
      if (bounds != null) {
        final Map<String, String> parts = new HashMap<>();
        parts.put("bbox", bounds.toBBox().toStringCSV(","));
        return MapillaryURL.queryString(parts);
      }
      return MapillaryURL.queryString(null);
    }

    /**
     * Vote for a detection
     *
     * @param layer The object layer
     * @param key The object key
     * @return A URL to vote for the detection
     */
    public static URL vote(String layer, String key) {
      return string2URL(baseUrl, String.format("object_detections/%s/%s/votes", layer, key),
        MapillaryURL.queryString(null));
    }

    /**
     * Get the detections for an image
     *
     * @param key The image key
     * @param layer The layer to retrieve (should be one of {@code trafficsigns}, {@code segmentations}, or
     *        {@code instances})
     * @return The url for detections
     */
    public static URL getDetections(String key, String layer) {
      Objects.requireNonNull(key);
      Objects.requireNonNull(layer);
      if (!Arrays.asList("trafficsigns", "segmentations", "instances").contains(layer)) {
        throw new UnsupportedOperationException("Mapillary is unknown: " + layer);
      }
      return string2URL(baseUrl, String.format("images/%s/object_detections/%s", key, layer),
        MapillaryURL.queryString(null));
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
      return string2URL(baseUrl, "map/im/", key);
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
      HashMap<String, String> parts = new HashMap<>();
      if (redirectURI != null && redirectURI.length() >= 1) {
        parts.put("redirect_uri", redirectURI);
      }
      parts.put("response_type", "token");
      parts.put("scope", "user:read public:upload public:write private:read");
      return string2URL(baseUrl, "connect", queryString(parts));
    }

    public static URL mapObjectIcon(String key) {
      return string2URL(baseUrl, "developer/api-documentation/images/traffic_sign/" + key + ".png");
    }
  }

  private MapillaryURL() {
    // Private constructor to avoid instantiation
  }

  /**
   * Builds a query string from it's parts that are supplied as a {@link Map}
   *
   * @param parts the parts of the query string
   * @return the constructed query string (including a leading ?)
   */
  static String queryString(Map<String, String> parts) {
    StringBuilder ret = new StringBuilder("?access_token=").append(ACCESS_ID);
    if (parts != null) {
      for (Map.Entry<String, String> entry : parts.entrySet()) {
        try {
          ret.append('&').append(URLEncoder.encode(entry.getKey(), StandardCharsets.UTF_8.name())).append('=')
            .append(URLEncoder.encode(entry.getValue(), StandardCharsets.UTF_8.name()));
        } catch (UnsupportedEncodingException e) {
          Logging.error(e); // This should not happen, as the encoding is hard-coded
        }
      }
    }
    return ret.toString();
  }

  /**
   * Converts a {@link String} into a {@link URL} without throwing a {@link MalformedURLException}.
   * Instead such an exception will lead to an {@link Logging#error(Throwable)}.
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
      Logging.log(Logging.LEVEL_ERROR,
        String.format("The class '%s' produces malformed URLs like '%s'!", MapillaryURL.class.getName(), builder), e);
      return null;
    }
  }
}
