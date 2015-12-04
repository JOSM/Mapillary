// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.oauth;

import java.io.IOException;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;

/**
 * Represents the current logged in user and stores its data.
 *
 * @author nokutu
 *
 */
public class MapillaryUser {

  private static String username;
  private static String imagesPolicy;
  private static String imagesHash;
  /** If the stored token is valid or not. */
  public static boolean isTokenValid = true;

  /**
   * Returns the username of the logged user.
   *
   * @return The username of the logged in user.
   */
  public static String getUsername() {
    if (!isTokenValid)
      return null;
    if (username == null)
      try {
        username = OAuthUtils
            .getWithHeader(
                new URL(
                    "https://a.mapillary.com/v2/me?client_id="+MapillaryPlugin.CLIENT_ID))
            .getString("username");
      } catch (IOException e) {
        Main.info("Invalid Mapillary token, reseting field");
        reset();
      }
    return username;
  }

  /**
   * @return A HashMap object containing the images_policy and images_hash
   *         strings.
   */
  public static Map<String, String> getSecrets() {
    if (!isTokenValid)
      return null;
    Map<String, String> hash = new HashMap<>();
    try {
      if (imagesHash == null)
        imagesHash = OAuthUtils
            .getWithHeader(
                new URL(
                    "https://a.mapillary.com/v2/me/uploads/secrets?client_id="+MapillaryPlugin.CLIENT_ID))
            .getString("images_hash");
      hash.put("images_hash", imagesHash);
      if (imagesPolicy == null)
        imagesPolicy = OAuthUtils
            .getWithHeader(
                new URL(
                    "https://a.mapillary.com/v2/me/uploads/secrets?client_id="+MapillaryPlugin.CLIENT_ID))
            .getString("images_policy");
    } catch (IOException e) {
      Main.info("Invalid Mapillary token, reseting field");
      reset();
    }
    hash.put("images_policy", imagesPolicy);
    return hash;
  }

  /**
   * Resets the MapillaryUser to null values.
   */
  public static void reset() {
    username = null;
    imagesPolicy = null;
    imagesHash = null;
    isTokenValid = false;
    Main.pref.put("mapillary.access-token", null);
  }
}
