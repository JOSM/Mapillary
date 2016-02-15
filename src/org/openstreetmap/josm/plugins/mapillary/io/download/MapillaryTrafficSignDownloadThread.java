// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.io.download;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.util.concurrent.ExecutorService;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonObject;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL.IMAGE_SELECTOR;

/**
 * Downloads the signs information in a given area.
 *
 * @author nokutu
 *
 */
public class MapillaryTrafficSignDownloadThread extends Thread {
  private final Bounds bounds;
  private final int page;
  private final ExecutorService ex;

  /**
   * Main constructor.
   *
   * @param ex {@link ExecutorService} object that is executing this thread.
   * @param bounds the bounds in which the traffic signs should be downloaded
   * @param page the pagenumber of the results page that should be retrieved
   */
  public MapillaryTrafficSignDownloadThread(ExecutorService ex, Bounds bounds, int page) {
    this.bounds = bounds;
    this.page = page;
    this.ex = ex;
  }

  @Override
  public void run() {

    try (
      BufferedReader br = new BufferedReader(new InputStreamReader(
        MapillaryURL.searchImageInfoURL(bounds, page, IMAGE_SELECTOR.OBJ_REC_ONLY).openStream(), "UTF-8"
      ));
    ) {
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
              for (MapillaryAbstractImage image : MapillaryLayer.getInstance().getData().getImages()) {
                if (image instanceof MapillaryImage && ((MapillaryImage) image).getKey().equals(key))
                  ((MapillaryImage) image).addSign(data.getString("type"));
              }
            }
          }
        }

        // Just one sign on the picture
        else if (rects != null) {
          for (int j = 0; j < rects.size(); j++) {
            JsonObject data = rects.getJsonObject(j);
            for (MapillaryAbstractImage image : MapillaryLayer.getInstance().getData().getImages()) {
              if (image instanceof MapillaryImage && ((MapillaryImage) image).getKey().equals(key)) {
                ((MapillaryImage) image).addSign(data.getString("type"));
              }
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
