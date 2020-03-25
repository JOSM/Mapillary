// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.oauth;

import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;

import javax.json.Json;
import javax.json.JsonReader;
import javax.json.JsonString;
import javax.json.JsonValue;

import org.openstreetmap.josm.plugins.mapillary.data.mapillary.OrganizationRecord;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL.APIv3;
import org.openstreetmap.josm.plugins.mapillary.utils.api.JsonOrganizationDecoderUtils;
import org.openstreetmap.josm.tools.HttpClient;
import org.openstreetmap.josm.tools.ListenerList;
import org.openstreetmap.josm.tools.Logging;

/**
 * Represents the current logged in user and stores its data.
 *
 * @author nokutu
 *
 */
public final class MapillaryUser {

  private static final ListenerList<MapillaryLoginListener> LISTENERS = ListenerList.create();
  private static String username;
  private static String imagesPolicy;
  private static String imagesHash;
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
   * @return The user information of the logged in user.
   */
  public static synchronized Map<String, String> getUserInformation() {
    if (!isTokenValid) {
      return Collections.emptyMap();
    }
    if (userInformation == null) {
      try {
        userInformation = OAuthUtils.getWithHeader(MapillaryURL.APIv3.userURL()).entrySet().parallelStream().filter(
          e -> JsonValue.ValueType.STRING.equals(e.getValue().getValueType())
        ).collect(Collectors.toMap(Entry::getKey, e -> ((JsonString) e.getValue()).getString()));
      } catch (IOException e) {
        Logging.log(Logging.LEVEL_WARN, "Invalid Mapillary token, resetting field", e);
        reset();
      }
    }
    return userInformation == null ? Collections.emptyMap() : Collections.unmodifiableMap(userInformation);
  }

  /**
   * @return A HashMap object containing the images_policy and images_hash
   *         strings.
   */
  public static synchronized Map<String, String> getSecrets() {
    if (!isTokenValid)
      return null;
    Map<String, String> hash = new HashMap<>();
    try {
      if (imagesHash == null)
        imagesHash = OAuthUtils
            .getWithHeader(MapillaryURL.uploadSecretsURL())
            .getString("images_hash", null);
      hash.put("images_hash", imagesHash);
      if (imagesPolicy == null)
        imagesPolicy = OAuthUtils
            .getWithHeader(MapillaryURL.uploadSecretsURL())
            .getString("images_policy");
    } catch (IOException e) {
      Logging.log(Logging.LEVEL_WARN, "Invalid Mapillary token, resetting field", e);
      reset();
    }
    hash.put("images_policy", imagesPolicy);
    return hash;
  }

  public static synchronized List<OrganizationRecord> getOrganizations() {
    if (isTokenValid && organizations == null) {
      HttpClient client = HttpClient.create(APIv3.retrieveOrganizationss(getUserInformation().getOrDefault("key", username)));
      OAuthUtils.addAuthenticationHeader(client);
      try {
        client.connect();
        try (JsonReader reader = Json.createReader(client.getResponse().getContentReader())) {
          organizations = JsonOrganizationDecoderUtils.decodeOrganizations(reader.readValue());
        }
      } catch (IOException e) {
        Logging.error(e);
      } finally {
        client.disconnect();
      }
    }
    return organizations == null ? Collections.emptyList() : Collections.unmodifiableList(organizations);
  }

  /**
   * Resets the MapillaryUser to null values.
   */
  public static synchronized void reset() {
    username = null;
    userInformation = null;
    organizations = null;
    imagesPolicy = null;
    imagesHash = null;
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
