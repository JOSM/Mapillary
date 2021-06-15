// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.oauth;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.net.URLConnection;
import java.util.Collections;

import javax.json.Json;
import javax.json.JsonException;
import javax.json.JsonObject;
import javax.json.JsonReader;
import javax.json.JsonStructure;

import org.apache.http.client.methods.HttpEntityEnclosingRequestBase;

import org.openstreetmap.josm.io.CachedFile;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL;
import org.openstreetmap.josm.tools.HttpClient;

/**
 * A set of utilities related to OAuth.
 *
 * @author nokutu
 */
public final class OAuthUtils {

  private static final String AUTHORIZATION = "Authorization";
  private static final String BEARER = "OAuth ";

  private OAuthUtils() {
    // Private constructor to avoid instantiation
  }

  /**
   * Returns a JsonObject containing the result of making a GET request with the
   * authorization header.
   *
   * @param url
   *        The {@link URL} where the request must be made.
   * @return A JsonObject containing the result of the GET request.
   * @throws IOException
   *         Errors relating to the connection.
   */
  public static JsonObject getWithHeader(URL url) throws IOException {
    HttpClient client = HttpClient.create(url, "GET");
    return getWithHeader(client);
  }

  /**
   * Returns a JsonObject containing the result of making a request with the
   * authorization header.
   *
   * @param client The HttpClient to get the data with
   * @return A JsonObject containing the result of the request.
   * @throws IOException
   *         Errors relating to the connection.
   */
  public static JsonObject getWithHeader(HttpClient client) throws IOException {
    addAuthenticationHeader(client);
    client.connect();
    try (InputStream inputStream = client.getResponse().getContent();
      JsonReader reader = Json.createReader(inputStream)) {
      JsonStructure structure = reader.read();
      return structure.asJsonObject();
    } catch (JsonException e) {
      throw new IOException(e);
    } finally {
      client.disconnect();
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
    con.setRequestProperty(AUTHORIZATION, getAuthorizationToken());
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
    return file.setHttpHeaders(Collections.singletonMap(AUTHORIZATION, getAuthorizationToken()));
  }

  /**
   * Returns a HttpClient with an authorization header for use when making user
   * specific API calls
   *
   * @param client The HttpClient to add authentication headers to
   * @return The HttpClient for easy chaining
   */
  public static HttpClient addAuthenticationHeader(HttpClient client) {
    return client.setHeader(AUTHORIZATION, getAuthorizationToken());
  }

  /**
   * Returns a HttpEntity with an authorization header for use when making user
   * specific API calls
   *
   * @param httpEntity The HttpEntity to add authentication headers to
   * @return The HttpEntity for easy chaining
   */
  public static HttpEntityEnclosingRequestBase addAuthenticationHeader(HttpEntityEnclosingRequestBase httpEntity) {
    httpEntity.addHeader(AUTHORIZATION, getAuthorizationToken());
    return httpEntity;
  }

  private static String getAuthorizationToken() {
    if (MapillaryProperties.ACCESS_TOKEN.isSet()) {
      return BEARER + MapillaryProperties.ACCESS_TOKEN.get();
    }
    return BEARER + MapillaryURL.APIv4.ACCESS_ID;
  }
}
