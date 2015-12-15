package org.openstreetmap.josm.plugins.mapillary.utils;

import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLEncoder;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.data.Bounds;

public final class MapillaryURL {
  private static final String CLIENT_ID = "T1Fzd20xZjdtR0s1VDk5OFNIOXpYdzoxNDYyOGRkYzUyYTFiMzgz";
  /** Base URL of the Mapillary API. */
  private static final String BASE_API_URL = "https://a.mapillary.com/v2/";
  private static final String BASE_WEBSITE_URL = "https://www.mapillary.com/";

  private MapillaryURL() {
    // Private constructor to avoid instantiation
  }

  public static URL browseEditURL(String key) {
    if (key == null || !key.matches("[a-zA-Z0-9\\-_]{22}")) {
      throw new IllegalArgumentException("Invalid image key");
    }
    return string2URL(BASE_WEBSITE_URL + "map/e/" + key);
  }

  public static URL browseImageURL(String key) {
    if (key == null || !key.matches("[a-zA-Z0-9\\-_]{22}")) {
      throw new IllegalArgumentException("Invalid image key");
    }
    return string2URL(BASE_WEBSITE_URL + "map/im/" + key);
  }

  public static URL browseUploadImageURL() {
    return string2URL(BASE_WEBSITE_URL + "map/upload/im");
  }

  public static URL connectURL(String redirectURI) {
    HashMap<String, String> parts = new HashMap<>();
    parts.put("redirect_uri", redirectURI);
    parts.put("response_type", "token");
    parts.put("scope", "user:read public:upload public:write");
    return string2URL(BASE_WEBSITE_URL + "connect/"+queryString(parts));
  }

  public static URL searchImageURL(Bounds bounds, int page) {
    HashMap<String, String> parts = new HashMap<>();
    putBoundsInQueryStringParts(parts, bounds);
    parts.put("page", Integer.toString(page));
    parts.put("limit", "20");
    return string2URL(BASE_API_URL + "search/im/" + queryString(parts));
  }

  public static URL searchSequenceURL(Bounds bounds, int page) {
    HashMap<String, String> parts = new HashMap<>();
    putBoundsInQueryStringParts(parts, bounds);
    parts.put("page", Integer.toString(page));
    parts.put("limit", "10");
    return string2URL(BASE_API_URL + "search/s/" + queryString(parts));
  }

  public static URL searchTrafficSignURL(Bounds bounds, int page) {
    HashMap<String, String> parts = new HashMap<>();
    putBoundsInQueryStringParts(parts, bounds);
    parts.put("page", Integer.toString(page));
    parts.put("limit", "20");
    return string2URL(BASE_API_URL + "search/im/or/" + queryString(parts));
  }

  public static URL uploadSecretsURL() {
    return string2URL(BASE_API_URL + "me/uploads/secrets/" + queryString(null));
  }

  public static URL userURL() {
    return string2URL(BASE_API_URL + "me/" + queryString(null));
  }

  private static void putBoundsInQueryStringParts(Map<String, String> parts, Bounds bounds) {
    parts.put("min_lat", String.format(Locale.UK, "%f", bounds.getMin().lat()));
    parts.put("max_lat", String.format(Locale.UK, "%f", bounds.getMax().lat()));
    parts.put("min_lon", String.format(Locale.UK, "%f", bounds.getMin().lon()));
    parts.put("max_lon", String.format(Locale.UK, "%f", bounds.getMax().lon()));
  }

  private static String queryString(Map<String, String> parts) {
    StringBuilder ret = new StringBuilder().append("?client_id=").append(CLIENT_ID);
    if (parts != null) {
      for (Entry<String, String> entry : parts.entrySet()) {
        try {
          ret.append('&')
            .append(URLEncoder.encode(entry.getKey(), "UTF-8"))
            .append('=')
            .append(URLEncoder.encode(entry.getValue(), "UTF-8"));
        } catch (UnsupportedEncodingException e) {
          Main.error(e); // This should not happen, as the encoding is hard-coded
        }
      }
    }
    return ret.toString();
  }

  private static URL string2URL(String string) {
    try {
      return new URL(string);
    } catch (MalformedURLException e) {
      Main.error("The MapillaryAPI class produces malformed URLs!", e);
      return null;
    }
  }
}
