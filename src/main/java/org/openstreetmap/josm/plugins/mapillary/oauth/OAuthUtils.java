// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.oauth;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLConnection;
import java.nio.charset.StandardCharsets;
import java.util.Collections;

import javax.json.Json;
import javax.json.JsonException;
import javax.json.JsonObject;
import javax.json.JsonReader;

import org.openstreetmap.josm.io.CachedFile;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.tools.HttpClient;

/**
 * A set of utilities related to OAuth.
 *
 * @author nokutu
 *
 */
public final class OAuthUtils {

  private OAuthUtils() {
    // Private constructor to avoid instantiation
  }

  /**
   * Returns a JsonObject containing the result of making a GET request with the
   * authorization header.
   *
   * @param url
   *          The {@link URL} where the request must be made.
   * @return A JsonObject containing the result of the GET request.
   * @throws IOException
   *           Errors relating to the connection.
   */
  public static JsonObject getWithHeader(URL url) throws IOException {
    HttpURLConnection con = (HttpURLConnection) url.openConnection();
    con.setRequestMethod("GET");
    addAuthenticationHeader(con);

    try (
      JsonReader reader = Json.createReader(new BufferedReader(new InputStreamReader(con.getInputStream(), StandardCharsets.UTF_8)))
    ) {
      return reader.readObject();
    } catch (JsonException e) {
      throw new IOException(e);
    }
  }

  /**
   * Returns a URLConnection with an authorization header for use when making user
   * specific API calls
   *
   * @param con The connection to add authentication headers to
   * @return The URLConnection for easy chaining
   */
  public static URLConnection addAuthenticationHeader(URLConnection con) {
    con.setRequestProperty("Authorization", "Bearer " + MapillaryProperties.ACCESS_TOKEN.get());
    return con;
  }

  /**
   * Returns a CachedFile with an authorization header for use when making user
   * specific API calls
   *
   * @param file The CachedFile to add authentication headers to
   * @return The CachedFile for easy chaining
   */
  public static CachedFile addAuthenticationHeader(CachedFile file) {
    if (MapillaryProperties.ACCESS_TOKEN.get() != null) {
      return file.setHttpHeaders(Collections.singletonMap("Authorization", "Bearer " + MapillaryProperties.ACCESS_TOKEN.get()));
    }
    return file;
  }

  /**
   * Returns a HttpClient with an authorization header for use when making user
   * specific API calls
   *
   * @param client The HttpClient to add authentication headers to
   * @return The HttpClient for easy chaining
   */
  public static HttpClient addAuthenticationHeader(HttpClient client) {
    return client.setHeader("Authorization", "Bearer " + MapillaryProperties.ACCESS_TOKEN.get());
  }
}
