// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.io.download;

import java.lang.reflect.Constructor;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.plugins.mapillary.MapillaryData;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryFilterDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryMainDialog;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.PluginState;

public class MapillarySquareDownloadRunnable implements Runnable {

  private final Bounds bounds;

  private ThreadPoolExecutor completeExecutor;

  /**
   * Main constructor.
   *
   * @param bounds the bounds of the area that should be downloaded
   *
   */
  public MapillarySquareDownloadRunnable(Bounds bounds) {
    this.bounds = bounds;
  }

  @Override
  public void run() {
    completeExecutor = newThreadPoolExecutor();

    try {
      PluginState.startDownload();
      MapillaryUtils.updateHelpText();

      // Download basic sequence data synchronously
      new SequenceDownloadRunnable(MapillaryLayer.getInstance().getData(), bounds).run();

      completeImages();
      MapillaryMainDialog.getInstance().updateTitle();
      Thread detectionsThread = new Thread(new DetectionsDownloadRunnable(MapillaryLayer.getInstance().getData(), bounds));
      detectionsThread.start();

      detectionsThread.join();
    } catch (InterruptedException e) {
      Main.error(e, "Mapillary download interrupted (probably because of closing the layer).");
    } finally {
      PluginState.finishDownload();
    }
    MapillaryUtils.updateHelpText();
    MapillaryData.dataUpdated();
    MapillaryFilterDialog.getInstance().refresh();
    MapillaryMainDialog.getInstance().updateImage();
  }

  /**
   * Constructs a new {@code ThreadPoolExecutor}.
   *
   * @return new {@code ThreadPoolExecutor}
   */
  private static ThreadPoolExecutor newThreadPoolExecutor() {
    return new ThreadPoolExecutor(3, 5, 25, TimeUnit.SECONDS,
      new ArrayBlockingQueue<>(5), new ThreadPoolExecutor.DiscardPolicy());
  }

  /**
   * Downloads the image's author's username and the image's location.
   *
   * @throws InterruptedException
   *           if the thread is interrupted while running this method.
   */
  private void completeImages() throws InterruptedException {
    download(completeExecutor, MapillaryImageInfoDownloadThread.class);
  }

  /**
   * Common download method.
   *
   * @param <T> Thread class
   * @param threadpool Thread pool executor
   * @param klass Thread class, must have a constructor with three arguments:
   * <ol>
   * <li>ThreadPoolExecutor executor</li>
   * <li>Bounds bounds</li>
   * <li>int page</li>
   * </ol>
   *
   * @throws InterruptedException
   *           if the thread is interrupted while running this method.
   */
  private <T extends Thread> void download(ThreadPoolExecutor threadpool, Class<T> klass)
    throws InterruptedException {
    try {
      int page = 0;
      Constructor<T> constructor = klass.getConstructor(ExecutorService.class, Bounds.class, int.class);
      while (!threadpool.isShutdown()) {
        threadpool.execute(constructor.newInstance(threadpool, bounds, page));
        while (threadpool.getQueue().remainingCapacity() == 0) {
          Thread.sleep(100);
        }
        page++;
      }
      threadpool.awaitTermination(15, TimeUnit.SECONDS);
    } catch (ReflectiveOperationException e) {
      Main.error(e);
    }
  }
}
