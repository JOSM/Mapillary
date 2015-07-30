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

  /** URL where to upload the images. */
  public static final String MAPILLARY_UPLOAD_URL = "https://s3-eu-west-1.amazonaws.com/mapillary.uploads.manual.images";

  /**
   * Returns a JsonObject containing the result of making a GET request with the
   * authorization header.
   *
   * @param url
   * @return A JsonObject containing the result of the GET request.
   * @throws IOException
   */
  public static JsonObject getWithHeader(URL url) throws IOException {
    HttpURLConnection con = (HttpURLConnection) url.openConnection();
    con.setRequestMethod("GET");
    con.setRequestProperty("Authorization",
        "Bearer " + Main.pref.get("mapillary.access-token"));

    BufferedReader in = new BufferedReader(new InputStreamReader(
        con.getInputStream()));
    return Json.createReader(in).readObject();
  }
}
