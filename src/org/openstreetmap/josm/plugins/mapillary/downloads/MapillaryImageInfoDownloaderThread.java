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
 * This thread downloads one of the images in a given area.
 *
 * @author nokutu
 * @see MapillarySquareDownloadManagerThread
 */
public class MapillaryImageInfoDownloaderThread extends Thread {
  private static final String URL = MapillaryDownloader.BASE_URL + "search/im/";
  private final String queryString;
  private final ExecutorService ex;
  private final MapillaryLayer layer;

  /**
   * Main constructor.
   * 
   * @param ex
   *          {@link ExecutorService} object that is executing this thread.
   * @param queryString
   * @param layer
   */
  public MapillaryImageInfoDownloaderThread(ExecutorService ex,
      String queryString, MapillaryLayer layer) {
    this.ex = ex;
    this.queryString = queryString;
    this.layer = layer;
  }

  @Override
  public void run() {
    try {
      BufferedReader br = new BufferedReader(new InputStreamReader(new URL(URL
          + queryString).openStream(), "UTF-8"));
      JsonObject jsonobj = Json.createReader(br).readObject();
      if (!jsonobj.getBoolean("more"))
        ex.shutdown();
      JsonArray jsonarr = jsonobj.getJsonArray("ims");
      JsonObject data;
      for (int i = 0; i < jsonarr.size(); i++) {
        data = jsonarr.getJsonObject(i);
        String key = data.getString("key");
        for (MapillaryAbstractImage image : layer.getMapillaryData()
            .getImages()) {
          if (image instanceof MapillaryImage) {
            if (((MapillaryImage) image).getKey().equals(key)
                && ((MapillaryImage) image).getUser() == null) {
              ((MapillaryImage) image).setUser(data.getString("user"));
              ((MapillaryImage) image).setCapturedAt(data.getJsonNumber(
                  "captured_at").longValue());
            }
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
