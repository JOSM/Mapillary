package org.openstreetmap.josm.plugins.mapillary.downloads;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.Locale;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ConcurrentHashMap;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryFilterDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryMainDialog;

/**
 * This Class is needed to create an indeterminate amount of downloads, because
 * the Mapillary API has a parameter called page which is needed when the amount
 * of requested images is quite big.
 *
 * @author nokutu
 *
 * @see MapillaryDownloader
 */
public class MapillarySquareDownloadManagerThread extends Thread {

  private final String imageQueryString;
  private final String sequenceQueryString;
  private final String signQueryString;
  private final MapillaryLayer layer;
  /** Whether if new images have been added in the download or not. */
  public boolean imagesAdded = false;

  /**
   * Main constructor.
   *
   * @param queryStringParts
   * @param layer
   *
   */
  public MapillarySquareDownloadManagerThread(
      ConcurrentHashMap<String, Double> queryStringParts, MapillaryLayer layer) {
    this.imageQueryString = buildQueryString(queryStringParts);
    this.sequenceQueryString = buildQueryString(queryStringParts);
    this.signQueryString = buildQueryString(queryStringParts);

    // TODO: Move this line to the appropriate place, here's no GET-request
    Main.info("GET " + sequenceQueryString + " (Mapillary plugin)");

    this.layer = layer;
  }

  // TODO: Maybe move into a separate utility class?
  private String buildQueryString(ConcurrentHashMap<String, Double> hash) {
    StringBuilder ret = new StringBuilder("?client_id="
        + MapillaryDownloader.CLIENT_ID);
    for (String key : hash.keySet())
      if (key != null)
        try {
          ret.append("&" + URLEncoder.encode(key, "UTF-8")).append(
              "="
                  + URLEncoder.encode(
                      String.format(Locale.UK, "%f", hash.get(key)), "UTF-8"));
        } catch (UnsupportedEncodingException e) {
          // This should not happen, as the encoding is hard-coded
        }
    return ret.toString();
  }

  @Override
  public void run() {
    Main.map.statusLine.setHelpText(tr("Downloading images from Mapillary"));
    try {
      downloadSequences();
      if (imagesAdded) {
        Main.map.statusLine.setHelpText(tr("Downloading image's information"));
        completeImages();
        MapillaryMainDialog.getInstance().updateTitle();
        Main.map.statusLine.setHelpText(tr("Downloading traffic signs"));
        downloadSigns();
      }
    } catch (InterruptedException e) {
      Main.error(e);
    }
    layer.updateHelpText();
    layer.getMapillaryData().dataUpdated();
    MapillaryFilterDialog.getInstance().refresh();
    MapillaryMainDialog.getInstance().updateImage();
  }

  private void downloadSequences() throws InterruptedException {
    ThreadPoolExecutor ex = new ThreadPoolExecutor(3, 5, 25, TimeUnit.SECONDS,
        new ArrayBlockingQueue<Runnable>(5));
    int page = 0;
    while (!ex.isShutdown()) {
      ex.execute(new MapillarySequenceDownloadThread(ex, sequenceQueryString
          + "&page=" + page + "&limit=10", layer, this));
      while (ex.getQueue().remainingCapacity() == 0)
        Thread.sleep(500);
      page++;
    }
    ex.awaitTermination(15, TimeUnit.SECONDS);
    layer.getMapillaryData().dataUpdated();
  }

  private void completeImages() throws InterruptedException {
    ThreadPoolExecutor ex = new ThreadPoolExecutor(3, 5, 25, TimeUnit.SECONDS,
        new ArrayBlockingQueue<Runnable>(5));
    int page = 0;
    while (!ex.isShutdown()) {
      ex.execute(new MapillaryImageInfoDownloaderThread(ex, imageQueryString
          + "&page=" + page + "&limit=20", layer));
      while (ex.getQueue().remainingCapacity() == 0)
        Thread.sleep(100);
      page++;
    }
    ex.awaitTermination(15, TimeUnit.SECONDS);
  }

  private void downloadSigns() throws InterruptedException {
    ThreadPoolExecutor ex = new ThreadPoolExecutor(3, 5, 25, TimeUnit.SECONDS,
        new ArrayBlockingQueue<Runnable>(5));
    int page = 0;
    while (!ex.isShutdown()) {
      ex.execute(new MapillaryTrafficSignDownloaderThread(ex, signQueryString
          + "&page=" + page + "&limit=20", layer));
      while (ex.getQueue().remainingCapacity() == 0)
        Thread.sleep(100);
      page++;
    }
    ex.awaitTermination(15, TimeUnit.SECONDS);
  }
}
