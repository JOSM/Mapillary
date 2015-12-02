// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.oauth;

import java.io.IOException;
import java.net.URL;
import java.util.HashMap;

import org.openstreetmap.josm.Main;

/**
 * Represents the current logged in user and stores its data.
 *
 * @author nokutu
 *
 */
public class MapillaryUser {

  private static String username;
  private static String images_policy;
  private static String images_hash;
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
                    "https://a.mapillary.com/v2/me?client_id=T1Fzd20xZjdtR0s1VDk5OFNIOXpYdzoxNDYyOGRkYzUyYTFiMzgz"))
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
  public static HashMap<String, String> getSecrets() {
    if (!isTokenValid)
      return null;
    HashMap<String, String> hash = new HashMap<>();
    try {
      if (images_hash == null)
        images_hash = OAuthUtils
            .getWithHeader(
                new URL(
                    "https://a.mapillary.com/v2/me/uploads/secrets?client_id=T1Fzd20xZjdtR0s1VDk5OFNIOXpYdzoxNDYyOGRkYzUyYTFiMzgz"))
            .getString("images_hash");
      hash.put("images_hash", images_hash);
      if (images_policy == null)
        images_policy = OAuthUtils
            .getWithHeader(
                new URL(
                    "https://a.mapillary.com/v2/me/uploads/secrets?client_id=T1Fzd20xZjdtR0s1VDk5OFNIOXpYdzoxNDYyOGRkYzUyYTFiMzgz"))
            .getString("images_policy");
    } catch (IOException e) {
      Main.info("Invalid Mapillary token, reseting field");
      reset();
    }
    hash.put("images_policy", images_policy);
    return hash;
  }

  /**
   * Resets the MapillaryUser to null values.
   */
  public static void reset() {
    username = null;
    images_policy = null;
    images_hash = null;
    isTokenValid = false;
    Main.pref.put("mapillary.access-token", null);
  }
}
