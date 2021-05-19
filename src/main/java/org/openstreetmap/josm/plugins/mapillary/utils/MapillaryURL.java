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
import java.util.Map;
import java.util.Objects;

import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.tools.Logging;

public final class MapillaryURL {
  /** Base URL of the Mapillary API. */
  private static final String CLIENT_ID = "UTZhSnNFdGpxSEFFREUwb01GYzlXZzpjNGViMzQxMTIzMjY0MjZm";

  public static final class APIv4 {
    static String baseUrl = "https://a.mapillary.com/v3/";

    /**
     * Get the Traffic Sign Tile URL
     *
     * @return A URL (String)
     */
    public static String getTrafficSigns() {
      return basePointDetection() + "&layers=trafficsigns";
    }

    /**
     * Get the Object Detection Tile URL
     *
     * @return A URL (String)
     */
    public static String getObjectDetections() {
      return basePointDetection() + "&layers=points";
    }

    private static String basePointDetection() {
      // TODO Update to actual v4 API
      return baseUrl + "map_features?tile={z}/{x}/{y}&per_page=1000&client_id=" + CLIENT_ID;
    }

    /**
     * Get the Sequence/Image Tile URL
     *
     * @return A URL (String)
     */
    public static String getImages() {
      // TODO Update to actual v4 API
      // This currently does not require the CLIENT_ID (and will actually return an empty response, if the CLIENT_ID is
      // given)
      return "https://tiles3.mapillary.com/v0.1/{z}/{x}/{y}.mvt";
    }

    /**
     * Get more image information
     *
     * @param images The image(s) to get more information for
     * @return The URL for image information
     */
    public static URL getImageInformation(String... images) {
      return string2URL(baseUrl, "images", queryString(Collections.singletonMap("ids", String.join(",", images))));
    }

    /**
     * Get all image detections
     *
     * @param image The image to get detections for
     * @return The URL to get detections
     */
    public static String getImageDetections(String image) {
      return baseUrl + image + "/detections?client_id=" + CLIENT_ID;
    }

    /**
     * Get user information for the current user
     *
     * @return The URL to get user information (logged in user only)
     */
    public static URL getUserInformation() {
      return string2URL(baseUrl, "me", queryString(null));
    }

    /**
     * Get the user's organizations
     *
     * @return The URL to get user orgs from
     */
    public static URL getUserOrganizations() {
      return string2URL(baseUrl, "me", "organizations", queryString(null));
    }
  }

  public static final class APIv3 {
    static String baseUrl = "https://a.mapillary.com/v3/";

    private static final String IMAGES = "images";
    private static final String SEQUENCES = "sequences";

    private APIv3() {
      // Private constructor to avoid instantiation
    }

    /**
     * Retrieve images by sequence
     *
     * @param key The sequence keys
     * @return The URL to get images with
     */
    public static URL getImagesBySequences(String... key) {
      return string2URL(baseUrl, IMAGES, queryString(null), "&sequence_keys=", String.join(",", key));
    }

    /**
     * Retrieve specific sequences
     *
     * @param key The sequence key(s)
     * @return The URL for the sequence(s)
     */
    public static URL getSequence(String... key) {
      if (key.length == 1) {
        return string2URL(baseUrl, SEQUENCES + "/", key[0], queryString(null));
      } else if (key.length > 1) {
        return string2URL(baseUrl, SEQUENCES, queryString(null), "&sequence_keys=", String.join(",", key));
      }
      return null;
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
    StringBuilder ret = new StringBuilder("?client_id=").append(CLIENT_ID);
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
