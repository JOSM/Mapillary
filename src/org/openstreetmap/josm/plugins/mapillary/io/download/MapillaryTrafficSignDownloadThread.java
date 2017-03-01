// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.io.download;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLConnection;
import java.util.concurrent.ExecutorService;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.JsonReader;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.MapillarySign;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL.DETECTION_PACKAGE;

/**
 * Downloads the signs information in a given area.
 *
 * @author nokutu
 */
public class MapillaryTrafficSignDownloadThread extends Thread {
  private final Bounds bounds;
  private final int page;
  private final ExecutorService ex;

  /**
   * Main constructor.
   *
   * @param ex     {@link ExecutorService} object that is executing this thread.
   * @param bounds the bounds in which the traffic signs should be downloaded
   * @param page   the pagenumber of the results page that should be retrieved
   */
  public MapillaryTrafficSignDownloadThread(ExecutorService ex, Bounds bounds, int page) {
    this.bounds = bounds;
    this.page = page;
    this.ex = ex;
  }

  @Override
  public void run() {
      URL searchImageDetectionsURL = MapillaryURL.searchImageDetectionsURL(bounds, page, DETECTION_PACKAGE.TRAFFICSIGNS);

      try {
          URLConnection urlConnection = searchImageDetectionsURL.openConnection();
          BufferedReader br = new BufferedReader(new InputStreamReader(
                  urlConnection.getInputStream(), "UTF-8"
          ));

          JsonReader reader = Json.createReader(br);
          URLConnection connection = searchImageDetectionsURL.openConnection();
          JsonObject jsonObj = reader.readObject();
          String headerFieldLink = urlConnection.getHeaderField("Link");
          if (headerFieldLink == null || !headerFieldLink.contains("next")) {
            this.ex.shutdown();
          }
          JsonArray features = jsonObj.getJsonArray("features");
          for (int i = 0; i < features.size(); i++) {
              final JsonObject detectionFeature = features.getJsonObject(i);
              final JsonObject detectionProperties = detectionFeature.getJsonObject("properties");
              for (MapillaryAbstractImage image : MapillaryLayer.getInstance().getData().getImages()) {
                  if (image instanceof MapillaryImage && ((MapillaryImage) image).getKey().equals(detectionProperties.getString("image_key"))) {
                      ((MapillaryImage) image).addSign(new MapillarySign(
                              detectionProperties.getString("value"), detectionProperties.getString("key")));
                  }
              }
          }
    } catch (IOException e) {
      Main.error(e);
    }
  }
}
