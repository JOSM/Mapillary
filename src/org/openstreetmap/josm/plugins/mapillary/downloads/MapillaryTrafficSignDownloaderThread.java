package org.openstreetmap.josm.plugins.mapillary.downloads;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.concurrent.ExecutorService;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonObject;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;

/**
 * Downloads the signs information in a given area.
 *
 * @author nokutu
 *
 */
public class MapillaryTrafficSignDownloaderThread extends Thread {
  private static final String URL = MapillaryDownloader.BASE_URL
      + "search/im/or/";
  private final String queryString;
  private final ExecutorService ex;
  private final MapillaryLayer layer;

  /**
   * Main constructor.
   *
   * @param ex
   *          {@link ExecutorService} object that is executing this thread.
   * @param queryString
   *          A String containing the parameter for the download.
   * @param layer
   *          The layer to store the data.
   */
  public MapillaryTrafficSignDownloaderThread(ExecutorService ex,
      String queryString, MapillaryLayer layer) {
    this.ex = ex;
    this.queryString = queryString;
    this.layer = layer;
  }

  @Override
  public void run() {
    BufferedReader br;
    try {
      br = new BufferedReader(new InputStreamReader(
          new URL(URL + this.queryString).openStream(), "UTF-8"));
      JsonObject jsonobj = Json.createReader(br).readObject();
      if (!jsonobj.getBoolean("more")) {
        this.ex.shutdown();
      }
      JsonArray jsonarr = jsonobj.getJsonArray("ims");
      for (int i = 0; i < jsonarr.size(); i++) {
        JsonArray rects = jsonarr.getJsonObject(i).getJsonArray("rects");
        JsonArray rectversions = jsonarr.getJsonObject(i).getJsonArray(
            "rectversions");
        String key = jsonarr.getJsonObject(i).getString("key");
        if (rectversions != null) {
          for (int j = 0; j < rectversions.size(); j++) {
            rects = rectversions.getJsonObject(j).getJsonArray("rects");
            for (int k = 0; k < rects.size(); k++) {
              JsonObject data = rects.getJsonObject(k);
              for (MapillaryAbstractImage image : this.layer.getMapillaryData()
                  .getImages())
                if (image instanceof MapillaryImage
                    && ((MapillaryImage) image).getKey().equals(key))
                  ((MapillaryImage) image).addSign(data.getString("type"));
            }
          }
        }

        // Just one sign on the picture
        else if (rects != null) {
          for (int j = 0; j < rects.size(); j++) {
            JsonObject data = rects.getJsonObject(j);
            for (MapillaryAbstractImage image : this.layer.getMapillaryData()
                .getImages())
              if (image instanceof MapillaryImage
                  && ((MapillaryImage) image).getKey().equals(key))
                ((MapillaryImage) image).addSign(data.getString("type"));
          }
        }
      }
    } catch (MalformedURLException e) {
      Main.error(e);
    } catch (IOException e) {
      Main.error(e);
    }
  }
}
