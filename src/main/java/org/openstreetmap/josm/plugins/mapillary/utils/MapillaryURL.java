// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.tools.Logging;

public final class MapillaryURL {
  /** Base URL of the Mapillary API. */
  private static final String BASE_API_V2_URL = "https://a.mapillary.com/v2/";
  private static final String CLIENT_ID = "T1Fzd20xZjdtR0s1VDk5OFNIOXpYdzoxNDYyOGRkYzUyYTFiMzgz";
  private static final String TRAFFIC_SIGN_LAYER = "trafficsigns";


  public static final class APIv3 {
    static String baseUrl = "https://a.mapillary.com/v3/";

    private APIv3() {
      // Private constructor to avoid instantiation
    }

    public static URL getUser(String key) {
      return string2URL(baseUrl, "users/", key, MapillaryURL.queryString(null));
    }

    /**
     * @return the URL where you can create, get and approve changesets
     */
    public static URL submitChangeset() {
      return string2URL(baseUrl, "changesets", queryString(null));
    }

    public static URL searchDetections(Bounds bounds) {
      return string2URL(baseUrl, "image_detections", queryString(bounds, TRAFFIC_SIGN_LAYER));
    }

    public static URL searchImages(Bounds bounds) {
      return string2URL(baseUrl, "images", queryString(bounds));
    }

    public static URL searchMapObjects(final Bounds bounds) {
      return string2URL(baseUrl, "map_features", queryString(bounds, TRAFFIC_SIGN_LAYER));
    }

    public static URL searchSequences(final Bounds bounds) {
      return string2URL(baseUrl, "sequences", queryString(bounds));
    }

    /**
     * The APIv3 returns a Link header for each request. It contains a URL for requesting more results.
     * If you supply the value of the Link header, this method returns the next URL,
     * if such a URL is defined in the header.
     * @param value the value of the HTTP-header with key "Link"
     * @return the {@link URL} for the next result page, or <code>null</code> if no such URL could be found
     */
    public static URL parseNextFromLinkHeaderValue(String value) {
      if (value != null) {
        // Iterate over the different entries of the Link header
        for (String link : value.split(",", Integer.MAX_VALUE)) {
          boolean isNext = false;
          URL url = null;
          // Iterate over the parts of each entry (typically it's one `rel="‹linkType›"` and one like `<https://URL>`)
          for (String linkPart : link.split(";", Integer.MAX_VALUE)) {
            linkPart = linkPart.trim();
            isNext |= linkPart.matches("rel\\s*=\\s*\"next\"");
            if (linkPart.length() >= 1 && linkPart.charAt(0) == '<' && linkPart.endsWith(">")) {
              try {
                url = new URL(linkPart.substring(1, linkPart.length() - 1));
              } catch (MalformedURLException e) {
                Logging.log(Logging.LEVEL_WARN, "Mapillary API v3 returns a malformed URL in the Link header.", e);
              }
            }
          }
          // If both a URL and the rel=next attribute are present, return the URL. Otherwise null is returned
          if (url != null && isNext) {
            return url;
          }
        }
      }
      return null;
    }

    public static String queryString(final Bounds bounds) {
      if (bounds != null) {
        final Map<String, String> parts = new HashMap<>();
        parts.put("bbox", bounds.toBBox().toStringCSV(","));
        return MapillaryURL.queryString(parts);
      }
      return MapillaryURL.queryString(null);
    }

    public static String queryString(final Bounds bounds, final String layer) {
      if (layer == null) {
        throw new IllegalArgumentException("layer cannot be null");
      }
      final Map<String, String> parts = new HashMap<>();
      if (bounds != null) {
        parts.put("bbox", bounds.toBBox().toStringCSV(","));
      }
      parts.put("layers", layer);
      return MapillaryURL.queryString(parts);

    }

    /**
     * @return the URL where you'll find information about the user account as JSON
     */
    public static URL userURL() {
      return string2URL(baseUrl, "me", MapillaryURL.queryString(null));
    }
  }

  public static final class Cloudfront {
    private static final String BASE_URL = "https://d1cuyjsrcm0gby.cloudfront.net/";

    private Cloudfront() {
      // Private constructor to avoid instantiation
    }

    public static URL thumbnail(final String key, final boolean large) {
      return string2URL(BASE_URL, key, "/thumb-", large ? "2048" : "320", ".jpg");
    }
  }

  public static final class MainWebsite {
    static String baseUrl = "https://www.mapillary.com/";

    private MainWebsite() {
      // Private constructor to avoid instantiation
    }

    /**
     * Gives you the URL for the online viewer of a specific mapillary image.
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
      parts.put("scope", "user:read public:upload public:write");
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
   * @return the URL where you'll find the upload secrets as JSON
   */
  public static URL uploadSecretsURL() {
    return string2URL(BASE_API_V2_URL, "me/uploads/secrets", queryString(null));
  }

  /**
   * Builds a query string from it's parts that are supplied as a {@link Map}
   * @param parts the parts of the query string
   * @return the constructed query string (including a leading ?)
   */
  static String queryString(Map<String, String> parts) {
    StringBuilder ret = new StringBuilder("?client_id=").append(CLIENT_ID);
    if (parts != null) {
      for (Entry<String, String> entry : parts.entrySet()) {
        try {
          ret.append('&')
            .append(URLEncoder.encode(entry.getKey(), StandardCharsets.UTF_8.name()))
            .append('=')
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
      Logging.log(Logging.LEVEL_ERROR, String.format(
          "The class '%s' produces malformed URLs like '%s'!",
          MapillaryURL.class.getName(),
          builder
      ), e);
      return null;
    }
  }
}
