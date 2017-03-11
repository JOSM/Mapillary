// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.io.download;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLConnection;
import java.time.Instant;
import java.time.format.DateTimeFormatter;
import java.util.concurrent.ExecutorService;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.JsonReader;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL;

/**
 * This thread downloads one of the images in a given area.
 *
 * @author nokutu
 * @see MapillarySquareDownloadManagerThread
 */
public class MapillaryImageInfoDownloadThread extends Thread {
  private final Bounds bounds;
  private final int page;
  private final ExecutorService ex;

  /**
   * Main constructor.
   *
   * @param ex {@link ExecutorService} object that is executing this thread.
   * @param bounds the bounds inside which the image info should be downloaded
   * @param page the pagenumber of the results that should be retrieved
   */
  public MapillaryImageInfoDownloadThread(ExecutorService ex, Bounds bounds, int page) {
    this.bounds = bounds;
    this.page = page;
    this.ex = ex;
  }

  @Override
  public void run() {
      URL imageInfoURL = MapillaryURL.searchImageInfoURL(bounds, page);
      try {
        URLConnection urlConnection = imageInfoURL.openConnection();
        BufferedReader br = new BufferedReader(new InputStreamReader(
                  urlConnection.getInputStream(), "UTF-8"
        ));

        JsonReader reader = Json.createReader(br);
        JsonObject jsonObj = reader.readObject();
        String headerFieldLink = urlConnection.getHeaderField("Link");
        if (headerFieldLink == null || !headerFieldLink.contains("next")) {
          this.ex.shutdown();
        }
        JsonArray features = jsonObj.getJsonArray("features");
        for (int i = 0; i < features.size(); i++) {
          final JsonObject imageFeature = features.getJsonObject(i);
          final JsonObject imageProperties = imageFeature.getJsonObject("properties");
          String key = imageProperties.getString("key");
          MapillaryLayer.getInstance().getData().getImages().stream().filter(image -> image instanceof MapillaryImage
            && ((MapillaryImage) image).getKey().equals(key)
            && ((MapillaryImage) image).getUser() == null).forEach(image -> {
            ((MapillaryImage) image).setUser(imageProperties.getString("user_key"));
              long epochMilli = Instant.from(DateTimeFormatter.ISO_INSTANT.parse(imageProperties.getString("captured_at"))).toEpochMilli();
              image.setCapturedAt(epochMilli);
          });
        }
    } catch (IOException e) {
      Main.error(e);
    }
  }
}
