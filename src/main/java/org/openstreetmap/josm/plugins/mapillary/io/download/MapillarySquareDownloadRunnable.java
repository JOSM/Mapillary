// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.io.download;

import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.TimeUnit;

import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryFilterDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryMainDialog;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.PluginState;
import org.openstreetmap.josm.tools.Logging;
import org.openstreetmap.josm.tools.Utils;

public class MapillarySquareDownloadRunnable implements Runnable {

  private final Bounds bounds;

  /**
   * Main constructor.
   *
   * @param bounds
   *          the bounds of the area that should be downloaded
   *
   */
  public MapillarySquareDownloadRunnable(Bounds bounds) {
    this.bounds = bounds;
  }

  @Override
  public void run() {
    PluginState.startDownload();
    MapillaryUtils.updateHelpText();

    // Download basic sequence data synchronously
    ForkJoinPool pool = Utils.newForkJoinPool("mapillary.forkjoinpool", "mapillary-downloader-%d", 4);
    pool.invoke(new SequenceDownloadRunnable(MapillaryLayer.getInstance().getData(), bounds));
    pool.awaitQuiescence(3600, TimeUnit.SECONDS);

    if (Thread.interrupted() || pool.isShutdown()) {
      pool.shutdown();
      return;
    }

    // Asynchronously load the rest of the image details
    pool.execute(new ImageDetailsDownloadRunnable(MapillaryLayer.getInstance().getData(), bounds));
    pool.execute(new DetectionsDownloadRunnable(MapillaryLayer.getInstance().getData(), bounds));

    ForkJoinPool.ManagedBlocker blocker = new ForkJoinPool.ManagedBlocker() {

      @Override
      public boolean isReleasable() {
        return (!pool.hasQueuedSubmissions() && pool.getActiveThreadCount() == 0)
          || MainApplication.getLayerManager().getLayersOfType(MapillaryLayer.class).isEmpty();
      }

      @Override
      public boolean block() throws InterruptedException {
        while (!this.isReleasable()) {
          synchronized (this) {
            wait(1000);
          }
        }
        return this.isReleasable();
      }
    };
    try {
      ForkJoinPool.managedBlock(blocker);
    } catch (InterruptedException e) {
      Thread.currentThread().interrupt();
      Logging.error(e);
    }

    pool.shutdownNow(); // Kill the threads -- the dataset doesn't really exist anymore
    PluginState.finishDownload();

    MapillaryUtils.updateHelpText();
    MapillaryLayer.invalidateInstance();
    MapillaryFilterDialog.getInstance().refresh();
    MapillaryMainDialog.getInstance().updateImage();
  }
}
