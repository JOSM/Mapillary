// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.oauth;

import static org.openstreetmap.josm.tools.I18n.marktr;
import static org.openstreetmap.josm.tools.I18n.tr;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URI;
import java.net.URL;
import java.net.URLConnection;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.Collections;

import javax.swing.JOptionPane;

import jakarta.annotation.Nullable;
import jakarta.json.Json;
import jakarta.json.JsonException;
import jakarta.json.JsonObject;
import jakarta.json.JsonReader;
import jakarta.json.JsonStructure;
import org.openstreetmap.josm.gui.Notification;
import org.openstreetmap.josm.gui.util.GuiHelper;
import org.openstreetmap.josm.io.CachedFile;
import org.openstreetmap.josm.plugins.mapillary.spi.preferences.IMapillaryUrls;
import org.openstreetmap.josm.plugins.mapillary.spi.preferences.MapillaryConfig;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.tools.HttpClient;
import org.openstreetmap.josm.tools.JosmRuntimeException;
import org.openstreetmap.josm.tools.Logging;
import org.openstreetmap.josm.tools.Utils;

/**
 * A set of utilities related to OAuth.
 *
 * @author nokutu
 */
public final class OAuthUtils {
    /**
     * A specific exception for when there is an issue with OAuth
     */
    private static final class OAuthUtilsException extends Exception {
        OAuthUtilsException(Throwable cause) {
            super(cause);
        }
    }

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
     * Returns a JsonObject containing the result of making a GET request with the
     * authorization header.
     *
     * @param uri
     *        The {@link URI} where the request must be made.
     * @return A JsonObject containing the result of the GET request.
     * @throws IOException
     *         Errors relating to the connection.
     */
    public static JsonObject getWithHeader(URI uri) throws IOException {
        return getWithHeader(uri.toURL());
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
            try (JsonReader reader = Json
                .createReader(new ByteArrayInputStream(appUsage.getBytes(StandardCharsets.UTF_8)))) {
                final JsonObject appUsageJson = reader.readObject();
                percentageUsed = appUsageJson.getInt("call_volume", appUsageJson.getInt("call_count", 0));
            }
        } else {
            percentageUsed = 0;
        }

        try (InputStream inputStream = response.getContent();
            final JsonReader reader = Json.createReader(inputStream)) {
            final JsonStructure structure = reader.read();
            if (percentageUsed > 95) {
                throw new IOException("API Limits reached");
            }
            validateJsonInformation(response, structure.toString());
            return structure.asJsonObject();
        } catch (JsonException e) {
            throw new IOException(e);
        } finally {
            client.disconnect();
        }
    }

    /**
     * Check the response, and reset the mapillary user if needed
     *
     * @param response The response with response information
     * @param optionalContent The content
     */
    private static void validateJsonInformation(@Nullable final HttpClient.Response response,
        @Nullable final String optionalContent) {
        if (response != null && response.getResponseCode() == HttpURLConnection.HTTP_UNAUTHORIZED
            && optionalContent != null && optionalContent.contains("The user has not authorized application")) {
            MapillaryUser.reset();
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
     * Get an authorization token
     *
     * @return The authorization token
     */
    private static String getAuthorizationToken() {
        if (MapillaryProperties.ACCESS_TOKEN.isSet() && MapillaryProperties.ACCESS_TOKEN_EXPIRES_AT.isSet()
            && MapillaryProperties.ACCESS_TOKEN_REFRESH_IN.isSet()) {
            // Right now, the expires_in is ~60 days. If we refresh every 7 days, we should (hopefully) account for
            // infrequent
            // users.
            if (Instant.now().isBefore(Instant.ofEpochSecond(MapillaryProperties.ACCESS_TOKEN_EXPIRES_AT.get())
                .minus(Duration.ofSeconds(MapillaryProperties.ACCESS_TOKEN_REFRESH_IN.get())))) {
                return BEARER + MapillaryProperties.ACCESS_TOKEN.get();
            } else {
                try {
                    refreshAuthorization(MapillaryProperties.ACCESS_TOKEN.get());
                    return getAuthorizationToken();
                } catch (OAuthUtilsException e) {
                    Logging.logWithStackTrace(Logging.LEVEL_TRACE, e);
                }
            }
        }
        return BEARER + MapillaryConfig.getUrls().getAccessId();
    }

    /**
     * Refresh authorization
     *
     * @param authorizationCode The code to use to refresh auth
     * @throws OAuthUtilsException if there was an {@link IOException} of some type
     */
    private static void refreshAuthorization(final String authorizationCode) throws OAuthUtilsException {
        HttpClient.Response response = null;
        String content = null;
        IMapillaryUrls urls = MapillaryConfig.getUrls();
        try {
            final HttpClient client = HttpClient.create(URI.create(urls.getTokenUrl()).toURL(), "POST");
            client.setHeader(AUTHORIZATION, BEARER + urls.getClientSecret());
            client.setRequestBody(
                ("grant_type=refresh_token&client_id=" + urls.getClientId() + "&refresh_token=" + authorizationCode)
                    .getBytes(StandardCharsets.UTF_8));

            response = client.connect();
            content = response.fetchContent();
            updateAuth(content);
        } catch (IOException e) {
            validateJsonInformation(response, content);
            final String notificationMessage = marktr("Could not refresh Mapillary token. HTTP Status Code {0}.");
            final HttpClient.Response response1 = response;
            GuiHelper.runInEDT(() -> {
                Notification notification = new Notification();
                notification.setIcon(JOptionPane.ERROR_MESSAGE);
                notification.setDuration(Notification.TIME_DEFAULT);
                notification.setContent(tr(notificationMessage,
                    response1 != null ? response1.getResponseCode() : HttpURLConnection.HTTP_NOT_ACCEPTABLE));
                notification.show();
            });
            throw new OAuthUtilsException(e);
        }
    }

    /**
     * Update auth information from a string
     *
     * @param content The content
     * @throws IOException If there is a {@link JsonException}
     */
    private static void updateAuth(final String content) throws IOException {
        try (JsonReader jsonReader = Json
            .createReader(new ByteArrayInputStream(content.getBytes(StandardCharsets.UTF_8)))) {
            final JsonObject jsonObject = jsonReader.readObject();
            OAuthUtils.updateAuthorization(jsonObject);
        } catch (JsonException e) {
            throw new IOException(content, e);
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
