package org.openstreetmap.josm.plugins.mapillary.oauth;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;

/**
 * @author nokutu
 *
 */
public class MapillaryUser {

  private static String username;
  private static String images_policy;
  private static String images_hash;

  /**
   * Returns the username of the logged user.
   *
   * @return The username of the logged in user.
   * @throws MalformedURLException
   * @throws IOException
   */
  public static String getUsername() throws MalformedURLException, IOException {
    if (username == null)
      username = OAuthUtils
          .getWithHeader(
              new URL(
                  "https://a.mapillary.com/v2/me?client_id=T1Fzd20xZjdtR0s1VDk5OFNIOXpYdzoxNDYyOGRkYzUyYTFiMzgz"))
          .getString("username");

    return username;
  }

  /**
   * @return A HashMap object containing the images_policy and images_hash
   *         strings.
   * @throws MalformedURLException
   * @throws IOException
   */
  public static HashMap<String, String> getSecrets()
      throws MalformedURLException, IOException {
    HashMap<String, String> hash = new HashMap<>();
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
  }

}
