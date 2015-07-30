package org.openstreetmap.josm.plugins.mapillary.downloads;

import java.io.BufferedReader;
import java.io.IOException;
import java.net.URL;
import java.io.InputStreamReader;

import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.Json;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.MapillarySequence;

/**
 * This thread downloads all the pictures in a given sequence and creates the
 * needed MapillaryImage and MapillarySequence objects. It just stores the ones
 * in the given area.
 *
 * @author nokutu
 * @see MapillarySquareDownloadManagerThread
 */
public class MapillarySequenceDownloadThread extends Thread {
  private static final String URL = MapillaryDownloader.BASE_URL + "search/s/";

  /** Lock to prevent multiple downloads to be imported at the same time. */
  private static final Lock LOCK = new ReentrantLock();

  private final String queryString;
  private final ExecutorService ex;
  private final MapillaryLayer layer;

  /**
   * Main constructor.
   *
   * @param ex
   * @param queryString
   */
  public MapillarySequenceDownloadThread(ExecutorService ex, String queryString) {
    this.queryString = queryString;
    this.ex = ex;
    this.layer = MapillaryLayer.getInstance();
  }

  @Override
  public void run() {
    try {
      BufferedReader br;
      br = new BufferedReader(new InputStreamReader(
          new URL(URL + queryString).openStream(), "UTF-8"));
      JsonObject jsonall = Json.createReader(br).readObject();

      if (!jsonall.getBoolean("more") && !ex.isShutdown())
        ex.shutdown();
      JsonArray jsonseq = jsonall.getJsonArray("ss");
      boolean isSequenceWrong = false;
      for (int i = 0; i < jsonseq.size(); i++) {
        JsonObject jsonobj = jsonseq.getJsonObject(i);
        JsonArray cas = jsonobj.getJsonArray("cas");
        JsonArray coords = jsonobj.getJsonArray("coords");
        JsonArray keys = jsonobj.getJsonArray("keys");
        ArrayList<MapillaryImage> images = new ArrayList<>();
        for (int j = 0; j < cas.size(); j++) {
          try {
            images.add(new MapillaryImage(keys.getString(j), coords
                .getJsonArray(j).getJsonNumber(1).doubleValue(), coords
                .getJsonArray(j).getJsonNumber(0).doubleValue(), cas
                .getJsonNumber(j).doubleValue()));
          } catch (IndexOutOfBoundsException e) {
            Main.warn("Mapillary bug at " + URL + queryString);
            isSequenceWrong = true;
          }
        }
        if (isSequenceWrong)
          break;
        MapillarySequence sequence = new MapillarySequence(
            jsonobj.getString("key"), jsonobj.getJsonNumber("captured_at")
                .longValue());
        List<MapillaryImage> finalImages = new ArrayList<>(images);
        // Here it gets only those images which are in the downloaded
        // area.
        for (MapillaryAbstractImage img : images) {
          if (!isInside(img))
            finalImages.remove(img);
        }

        LOCK.lock();
        MapillaryImage.LOCK.lock();
        try {
          for (MapillaryImage img : finalImages) {
            if (layer.getMapillaryData().getImages().contains(img)) {
              // The image in finalImages is substituted by the one in the
              // database, as they represent the same picture.
              img = (MapillaryImage) layer.getMapillaryData().getImages()
                  .get(layer.getMapillaryData().getImages().indexOf(img));
              sequence.add(img);
              ((MapillaryImage) layer.getMapillaryData().getImages()
                  .get(layer.getMapillaryData().getImages().indexOf(img)))
                  .setSequence(sequence);
              finalImages.set(finalImages.indexOf(img), img);
            } else {
              img.setSequence(sequence);
              sequence.add(img);
            }
          }
        } finally {
          MapillaryImage.LOCK.unlock();
          LOCK.unlock();
        }

        layer.getMapillaryData().addWithoutUpdate(
            new ArrayList<MapillaryAbstractImage>(finalImages));
      }
    } catch (IOException e) {
      Main.error("Error reading the url " + URL + queryString
          + " might be a Mapillary problem.");
    }
  }

  private boolean isInside(MapillaryAbstractImage image) {
    for (int i = 0; i < layer.bounds.size(); i++)
      if (layer.bounds.get(i).contains(image.getLatLon()))
        return true;
    return false;
  }
}
