// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.oauth;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import javax.json.JsonObject;
import javax.json.JsonString;
import javax.json.JsonValue;

import org.openstreetmap.josm.plugins.mapillary.data.mapillary.OrganizationRecord;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL;
import org.openstreetmap.josm.tools.HttpClient;
import org.openstreetmap.josm.tools.ListenerList;
import org.openstreetmap.josm.tools.Logging;

/**
 * Represents the current logged in user and stores its data.
 *
 * @author nokutu
 */
public final class MapillaryUser {

  private static final ListenerList<MapillaryLoginListener> LISTENERS = ListenerList.create();
  private static String username;
  private static JsonObject uploadSession;
  /** If the stored token is valid or not. */
  private static boolean isTokenValid = true;

  /** Various user information */
  private static Map<String, String> userInformation;

  /** User organization information */
  private static List<OrganizationRecord> organizations;

  private MapillaryUser() {
    // Private constructor to avoid instantiation
  }

  /**
   * Get the username of the current user
   *
   * @return The username of the logged in user.
   */
  public static synchronized String getUsername() {
    if (!isTokenValid) {
      return null;
    }
    Map<String, String> userInfo = getUserInformation();
    if (username == null && userInfo != null) {
      username = userInfo.get("username");
      LISTENERS.fireEvent(l -> l.onLogin(username));
    }
    return username;
  }

  /**
   * Get the information for the currently logged in user
   *
   * @return The user information of the logged in user.
   */
  public static synchronized Map<String, String> getUserInformation() {
    if (!isTokenValid) {
      return Collections.emptyMap();
    }
    if (userInformation == null) {
      try {
        userInformation = OAuthUtils.getWithHeader(MapillaryURL.APIv4.getUserInformation()).entrySet().parallelStream()
          .filter(e -> JsonValue.ValueType.STRING.equals(e.getValue().getValueType()))
          .collect(Collectors.toMap(Map.Entry::getKey, e -> ((JsonString) e.getValue()).getString()));
      } catch (IOException e) {
        Logging.log(Logging.LEVEL_WARN, "Invalid Mapillary token, resetting field", e);
        reset();
      }
    }
    return userInformation == null ? Collections.emptyMap() : Collections.unmodifiableMap(userInformation);
  }

  /**
   * Get the secrets needed to upload data
   *
   * @return A JsonObject containing secrets for upload
   */
  public static synchronized JsonObject getSecrets() {
    if (!isTokenValid)
      return null;
    JsonObject session = uploadSession;
    try {
      if (session == null) {
        final HttpClient client = HttpClient.create(MapillaryURL.APIv3.uploadSecretsURL(), "POST");
        client.setHeader("Content-Type", "application/json");
        // Currently, this is the only request body and it MUST be present
        client.setRequestBody("{\"type\": \"images/sequence\"}".getBytes(StandardCharsets.UTF_8));
        session = OAuthUtils.getWithHeader(client);
      }
    } catch (IOException e) {
      Logging.log(Logging.LEVEL_WARN, "Invalid Mapillary token, resetting field", e);
      reset();
    }
    return session;
  }

  /**
   * Finish a secret session (clear upload secrets).
   */
  public static synchronized void clearSecrets() {
    uploadSession = null;
  }

  /**
   * Resets the MapillaryUser to null values.
   */
  public static synchronized void reset() {
    username = null;
    userInformation = null;
    organizations = null;
    uploadSession = null;
    isTokenValid = false;
    MapillaryProperties.ACCESS_TOKEN.put(MapillaryProperties.ACCESS_TOKEN.getDefaultValue());
    LISTENERS.fireEvent(MapillaryLoginListener::onLogout);
  }

  public static synchronized void setTokenValid(boolean value) {
    isTokenValid = value;
  }

  public static void addListener(MapillaryLoginListener listener) {
    LISTENERS.addListener(listener);
  }

  public static void removeListener(MapillaryLoginListener listener) {
    LISTENERS.removeListener(listener);
  }
}
