// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.io.download;

import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.plugins.mapillary.MapillaryData;
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

  private final Bounds bounds;

  private final ThreadPoolExecutor downloadExecutor = new ThreadPoolExecutor(3, 5,
      25, TimeUnit.SECONDS, new ArrayBlockingQueue<Runnable>(5));
  private final ThreadPoolExecutor completeExecutor = new ThreadPoolExecutor(3, 5,
      25, TimeUnit.SECONDS, new ArrayBlockingQueue<Runnable>(5));
  private final ThreadPoolExecutor signsExecutor = new ThreadPoolExecutor(3, 5, 25,
      TimeUnit.SECONDS, new ArrayBlockingQueue<Runnable>(5));

  /**
   * Main constructor.
   *
   * @param bounds the bounds of the area that should be downloaded
   *
   */
  public MapillarySquareDownloadManagerThread(Bounds bounds) {
    this.bounds = bounds;
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
   * @throws InterruptedException if the thread is interrupted while running this method.
   */
  private void downloadSequences() throws InterruptedException {
    int page = 0;
    while (!this.downloadExecutor.isShutdown()) {
      this.downloadExecutor.execute(new MapillarySequenceDownloadThread(this.downloadExecutor, bounds, page));
      while (this.downloadExecutor.getQueue().remainingCapacity() == 0) {
        Thread.sleep(500);
      }
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
      this.completeExecutor.execute(new MapillaryImageInfoDownloadThread(completeExecutor, bounds, page));
      while (this.completeExecutor.getQueue().remainingCapacity() == 0) {
        Thread.sleep(100);
      }
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
      this.signsExecutor.execute(new MapillaryTrafficSignDownloadThread(this.signsExecutor, bounds, page));
      while (this.signsExecutor.getQueue().remainingCapacity() == 0) {
        Thread.sleep(100);
      }
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
