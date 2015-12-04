// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.io.download;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.Locale;
import java.util.Map.Entry;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.plugins.mapillary.MapillaryData;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryFilterDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryMainDialog;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.PluginState;

/**
 * This Class is needed to create an indeterminate amount of downloads, because
 * the Mapillary API has a parameter called page which is needed when the amount
 * of requested images is quite big.
 *
 * @author nokutu
 *
 * @see MapillaryDownloader
 * @see MapillarySequenceDownloadThread
 * @see MapillaryImageInfoDownloadThread
 * @see MapillaryTrafficSignDownloadThread
 */
public class MapillarySquareDownloadManagerThread extends Thread {

  private final String imageQueryString;
  private final String sequenceQueryString;
  private final String signQueryString;

  private ThreadPoolExecutor downloadExecutor = new ThreadPoolExecutor(3, 5,
      25, TimeUnit.SECONDS, new ArrayBlockingQueue<Runnable>(5));
  private ThreadPoolExecutor completeExecutor = new ThreadPoolExecutor(3, 5,
      25, TimeUnit.SECONDS, new ArrayBlockingQueue<Runnable>(5));
  private ThreadPoolExecutor signsExecutor = new ThreadPoolExecutor(3, 5, 25,
      TimeUnit.SECONDS, new ArrayBlockingQueue<Runnable>(5));

  /**
   * Main constructor.
   *
   * @param queryStringParts
   *          The query data.
   *
   */
  public MapillarySquareDownloadManagerThread(ConcurrentMap<String, Double> queryStringParts) {
    this.imageQueryString = buildQueryString(queryStringParts);
    this.sequenceQueryString = buildQueryString(queryStringParts);
    this.signQueryString = buildQueryString(queryStringParts);

    // TODO: Move this line to the appropriate place, here's no GET-request
    Main.info("GET " + this.sequenceQueryString + " (Mapillary plugin)");
  }

  // TODO: Maybe move into a separate utility class?
  private static String buildQueryString(ConcurrentMap<String, Double> hash) {
    StringBuilder ret = new StringBuilder().append("?client_id=").append(MapillaryPlugin.CLIENT_ID);
    for (Entry<String, Double> entry : hash.entrySet())
      if (entry.getKey() != null)
        try {
          ret.append('&')
            .append(URLEncoder.encode(entry.getKey(), "UTF-8"))
            .append('=')
            .append(URLEncoder.encode(String.format(Locale.UK, "%f", entry.getValue()), "UTF-8"));
        } catch (UnsupportedEncodingException e) {
          // This should not happen, as the encoding is hard-coded
        }
    return ret.toString();
  }

  @Override
  public void run() {
    try {
      PluginState.startDownload();
      MapillaryUtils.updateHelpText();
      downloadSequences();
      completeImages();
      MapillaryMainDialog.getInstance().updateTitle();
      downloadSigns();
    } catch (InterruptedException e) {
      Main.error("Mapillary download interrupted (probably because of closing the layer).");
    } finally {
      PluginState.finishDownload();
    }
    MapillaryUtils.updateHelpText();
    MapillaryData.dataUpdated();
    MapillaryFilterDialog.getInstance().refresh();
    MapillaryMainDialog.getInstance().updateImage();
  }

  /**
   * Downloads the sequence positions, directions and keys.
   *
   * @throws InterruptedException
   *           if the thread is interrupted while running this method.
   */
  private void downloadSequences() throws InterruptedException {
    int page = 0;
    while (!this.downloadExecutor.isShutdown()) {
      this.downloadExecutor.execute(new MapillarySequenceDownloadThread(
          this.downloadExecutor, this.sequenceQueryString + "&page=" + page + "&limit=10"));
      while (this.downloadExecutor.getQueue().remainingCapacity() == 0)
        Thread.sleep(500);
      page++;
    }
    this.downloadExecutor.awaitTermination(15, TimeUnit.SECONDS);
    MapillaryData.dataUpdated();
  }

  /**
   * Downloads the image's author's username and the image's location.
   *
   * @throws InterruptedException
   *           if the thread is interrupted while running this method.
   */
  private void completeImages() throws InterruptedException {
    int page = 0;
    while (!this.completeExecutor.isShutdown()) {
      this.completeExecutor.execute(new MapillaryImageInfoDownloadThread(
          this.completeExecutor, this.imageQueryString + "&page=" + page
              + "&limit=20"));
      while (this.completeExecutor.getQueue().remainingCapacity() == 0)
        Thread.sleep(100);
      page++;
    }
    this.completeExecutor.awaitTermination(15, TimeUnit.SECONDS);
  }

  /**
   * Downloads the traffic signs in the images.
   *
   * @throws InterruptedException
   *           if the thread is interrupted while running this method.
   */
  private void downloadSigns() throws InterruptedException {
    int page = 0;
    while (!this.signsExecutor.isShutdown()) {
      this.signsExecutor.execute(new MapillaryTrafficSignDownloadThread(
          this.signsExecutor, this.signQueryString + "&page=" + page
              + "&limit=20"));
      while (this.signsExecutor.getQueue().remainingCapacity() == 0)
        Thread.sleep(100);
      page++;
    }
    this.signsExecutor.awaitTermination(15, TimeUnit.SECONDS);
  }

  @Override
  public void interrupt() {
    super.interrupt();
    this.downloadExecutor.shutdownNow();
    this.completeExecutor.shutdownNow();
    this.signsExecutor.shutdownNow();
    try {
      this.downloadExecutor.awaitTermination(15, TimeUnit.SECONDS);
      this.completeExecutor.awaitTermination(15, TimeUnit.SECONDS);
      this.signsExecutor.awaitTermination(15, TimeUnit.SECONDS);
    } catch (InterruptedException e) {
      Main.error(e);
    }
  }
}
