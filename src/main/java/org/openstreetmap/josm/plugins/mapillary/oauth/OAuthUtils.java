// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.oauth;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.net.URLConnection;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.Collections;

import javax.json.Json;
import javax.json.JsonException;
import javax.json.JsonObject;
import javax.json.JsonReader;
import javax.json.JsonStructure;
import javax.swing.JOptionPane;

import org.openstreetmap.josm.gui.Notification;
import org.openstreetmap.josm.gui.util.GuiHelper;
import org.openstreetmap.josm.io.CachedFile;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL;
import org.openstreetmap.josm.tools.HttpClient;
import org.openstreetmap.josm.tools.JosmRuntimeException;
import org.openstreetmap.josm.tools.Logging;
import org.openstreetmap.josm.tools.Utils;

import org.apache.http.client.methods.HttpEntityEnclosingRequestBase;

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
    final HttpClient.Response response = client.getResponse();
    final String appUsage = response.getHeaderField("x-app-usage");
    final int percentageUsed;
    if (appUsage != null && !Utils.isStripEmpty(appUsage)) {
      try (JsonReader reader = Json.createReader(new ByteArrayInputStream(appUsage.getBytes(StandardCharsets.UTF_8)))) {
        final JsonObject appUsageJson = reader.readObject();
        percentageUsed = appUsageJson.getInt("call_volume", appUsageJson.getInt("call_count", 0));
      }
    } else {
      percentageUsed = 0;
    }

    try (InputStream inputStream = response.getContent(); final JsonReader reader = Json.createReader(inputStream)) {
      final JsonStructure structure = reader.read();
      if (percentageUsed > 95) {
        throw new IOException("API Limits reached");
      }
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
    if (MapillaryProperties.ACCESS_TOKEN.isSet() && MapillaryProperties.ACCESS_TOKEN_EXPIRES_AT.isSet()
      && MapillaryProperties.ACCESS_TOKEN_REFRESH_IN.isSet()) {
      // Right now, the expires_in is ~60 days. If we refresh every 7 days, we should (hopefully) account for infrequent
      // users.
      if (Instant.now().isBefore(Instant.ofEpochSecond(MapillaryProperties.ACCESS_TOKEN_EXPIRES_AT.get())
        .minus(Duration.ofSeconds(MapillaryProperties.ACCESS_TOKEN_REFRESH_IN.get())))) {
        return BEARER + MapillaryProperties.ACCESS_TOKEN.get();
      } else {
        refreshAuthorization(MapillaryProperties.ACCESS_TOKEN.get());
        return getAuthorizationToken();
      }
    }
    return BEARER + MapillaryURL.APIv4.ACCESS_ID;
  }

  private static void refreshAuthorization(final String authorizationCode) {
    try {
      final HttpClient client = HttpClient.create(new URL("https://graph.mapillary.com/token"), "POST");
      client.setHeader(AUTHORIZATION, BEARER + MapillaryURL.APIv4.CLIENT_SECRET);
      client.setRequestBody(("grant_type=refresh_token&client_id=" + Long.toString(MapillaryURL.APIv4.CLIENT_ID)
        + "&refresh_token=" + authorizationCode).getBytes(StandardCharsets.UTF_8));

      final HttpClient.Response response = client.connect();
      try (JsonReader jsonReader = Json.createReader(response.getContentReader())) {
        final JsonObject jsonObject = jsonReader.readObject();
        OAuthUtils.updateAuthorization(jsonObject);
      }
    } catch (IOException e) {
      MapillaryUser.reset();
      GuiHelper.runInEDT(() -> {
        Notification notification = new Notification();
        notification.setIcon(JOptionPane.ERROR_MESSAGE);
        notification.setDuration(Notification.TIME_DEFAULT);
        notification.setContent(tr("Could not refresh Mapillary token. Logging out."));
        notification.show();
      });
    }
  }

  /**
   * Update authorization information from a JSON object
   *
   * @param jsonObject The json object to use
   * @return {@code true} if the update was successful
   */
  static boolean updateAuthorization(final JsonObject jsonObject) {
    // The actual access token we want to use
    final String accessToken = jsonObject.getString("access_token", null);
    // This is in seconds
    final int expiresIn = jsonObject.getInt("expires_in", Integer.MIN_VALUE);
    // Will probably always be bearer
    final String tokenType = jsonObject.getString("token_type", null);
    if (tokenType == null || accessToken == null || expiresIn == Integer.MIN_VALUE) {
      MapillaryUser.reset();
      GuiHelper.runInEDT(() -> {
        Notification notification = new Notification();
        notification.setContent(tr("Mapillary: Could not refresh login, logging out"));
        notification.setIcon(JOptionPane.ERROR_MESSAGE);
        notification.show();
      });
      return false;
    }
    if (!"bearer".equals(tokenType)) {
      MapillaryUser.reset();
      throw new JosmRuntimeException("Mapillary: Login failed due to unknown token type: " + tokenType);
    }

    MapillaryUser.setTokenValid(true);
    MapillaryProperties.ACCESS_TOKEN.put(accessToken);
    MapillaryProperties.ACCESS_TOKEN_EXPIRES_AT.put(Instant.now().getEpochSecond() + expiresIn);
    final int amount;
    final ChronoUnit expireUnit;
    if (expiresIn > Duration.of(30, ChronoUnit.DAYS).getSeconds()) {
      amount = 7;
      expireUnit = ChronoUnit.DAYS;
    } else if (expiresIn > Duration.of(7, ChronoUnit.DAYS).getSeconds()) {
      amount = 1;
      expireUnit = ChronoUnit.DAYS;
    } else if (expiresIn > Duration.of(1, ChronoUnit.DAYS).getSeconds()) {
      amount = 1;
      expireUnit = ChronoUnit.HALF_DAYS;
    } else if (expiresIn > Duration.of(2, ChronoUnit.HOURS).getSeconds()) {
      amount = 1;
      expireUnit = ChronoUnit.HOURS;
    } else {
      // Hopefully we never hit this code path in production...
      amount = 10;
      expireUnit = ChronoUnit.MINUTES;
    }
    MapillaryProperties.ACCESS_TOKEN_REFRESH_IN.put(Duration.of(amount, expireUnit).getSeconds());

    Logging.info("Successful authentication with Mapillary, the access token is {0} expiring in {1} seconds",
      accessToken, expiresIn);
    // Saves the access token in preferences.
    return true;
  }
}
