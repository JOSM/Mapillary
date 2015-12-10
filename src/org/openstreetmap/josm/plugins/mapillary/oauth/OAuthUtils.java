// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.oauth;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;

import javax.json.Json;
import javax.json.JsonObject;

import org.openstreetmap.josm.Main;

/**
 * A set of utilities related to OAuth.
 *
 * @author nokutu
 *
 */
public class OAuthUtils {

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
    con.setRequestProperty("Authorization", "Bearer " + Main.pref.get("mapillary.access-token"));

    try (
      BufferedReader in = new BufferedReader(new InputStreamReader(con.getInputStream(), "UTF-8"))
    ) {
      return Json.createReader(in).readObject();
    }
  }
}
