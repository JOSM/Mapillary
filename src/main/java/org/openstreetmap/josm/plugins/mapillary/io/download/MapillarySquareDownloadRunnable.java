// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.io.download;

import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.TimeUnit;

import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.progress.ChildProgress;
import org.openstreetmap.josm.plugins.mapillary.gui.DownloadProgressMonitor;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryMainDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.dialog.MapillaryDownloadDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.dialog.MapillaryFilterDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.PluginState;
import org.openstreetmap.josm.tools.Logging;
import org.openstreetmap.josm.tools.Utils;

public class MapillarySquareDownloadRunnable implements Runnable {

  private final Bounds bounds;
  private final DownloadProgressMonitor monitor;
  private SequenceDownloadRunnable sqr;
  private ImageDetailsDownloadRunnable idr;
  private DetectionsDownloadRunnable ddr;
  public STATE state;
  private Thread thread;

  /**
   * Main constructor.
   *
   * @param bounds the bounds of the area that should be downloaded
   *
   */
  public MapillarySquareDownloadRunnable(Bounds bounds) {
    this.bounds = bounds;
    monitor = new DownloadProgressMonitor(this);
    monitor.beginTask("Download queued", 100);
    setState(STATE.QUEUED);
  }

  public DownloadProgressMonitor getMonitor() {
    return monitor;
  }

  public enum STATE {
    QUEUED,//Added for download in future.
    PAUSED,//Paused using {@link MapillaryDownloadDialog}.
    RUNNING,//Normal execution of task.
    STOPPED,//Stopped midway, may execute normally.
    FAILED//task failed during normal execution.
  }

  @Override
  public void run() {
    thread = Thread.currentThread();
    PluginState.startDownload();
    MapillaryUtils.updateHelpText();
    setState(STATE.RUNNING);
    MapillaryDownloadDialog.getInstance().downloadInfoChanged();

    // Download basic sequence data synchronously
    ForkJoinPool pool = Utils.newForkJoinPool("mapillary.forkjoinpool", "mapillary-downloader-%d", 4);

    sqr = new SequenceDownloadRunnable(MapillaryLayer.getInstance().getData(), bounds, (ChildProgress) monitor.createSubTaskMonitor(50, false));
    pool.invoke(sqr);
    pool.awaitQuiescence(3600, TimeUnit.SECONDS);

    if (Thread.interrupted() || pool.isShutdown()) {
      pool.shutdown();
      PluginState.finishDownload();
      MapillaryDownloader.removeHash(this);
      monitor.doFinishTask();
      return;
    }

    synchronized (this) {
      if (state == STATE.PAUSED) {
        try {
          this.wait();
        } catch (InterruptedException e) {
          Thread.currentThread().interrupt();
          Logging.error(e);
        }
      }
    }
    // Asynchronously load the rest of the image details
    idr = new ImageDetailsDownloadRunnable(MapillaryLayer.getInstance().getData(), bounds, (ChildProgress) monitor.createSubTaskMonitor(25, false));
    ddr = new DetectionsDownloadRunnable(MapillaryLayer.getInstance().getData(), bounds, (ChildProgress) monitor.createSubTaskMonitor(25, false));
    pool.execute(idr);
    pool.execute(ddr);

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
    if (isComplete()) {
      monitor.finishTask();
    } else if (state == STATE.STOPPED) {//Stopped midway and download didn't complete.
      MapillaryDownloader.removeHash(this);
    } else {//Failed normally, this can be then be reused to create a new download then.
      setState(STATE.FAILED);
    }

    MapillaryUtils.updateHelpText();
    MapillaryLayer.invalidateInstance();
    MapillaryFilterDialog.getInstance().refresh();
    MapillaryMainDialog.getInstance().updateImage();
  }

  public Boolean isComplete() {
    return sqr.completed && idr.completed && ddr.completed;
  }

  public Boolean isPausable() {
    return state == STATE.RUNNING;
  }

  public Boolean isResumable() {
    return state == STATE.PAUSED || state == STATE.STOPPED;
  }

  public Boolean isCancelable() {
    return state != STATE.STOPPED;
  }

  public int completeCount() {
    int c = 0;
    if (sqr != null && sqr.completed) {
      c++;
    }
    if (idr != null && idr.completed) {
      c++;
    }
    if (ddr != null && ddr.completed) {
      c++;
    }
    return c;
  }

  public Bounds getBounds() {
    return bounds;
  }

  //Cancel a download that hasn't started yet or is running, remove failed ones.
  public void cancel() {
    if (state == STATE.QUEUED) {
      MapillaryDownloader.removeDownload(this);
      monitor.finishTask();
    } else if (state == STATE.FAILED) {
      monitor.doFinishTask();
    } else {//Running
      finish();
    }
  }

  //Atempts to pause the download after {@code sqr} has completed download.
  public void pause() {
    setState(STATE.PAUSED);
  }

  //Resume after paused using {@link pause()} or redownload if failed.
  public void resume() {
    if (state == STATE.PAUSED) {
      synchronized (this) {
        this.notifyAll();
      }
    } else if (state == STATE.FAILED) {
      MapillaryDownloader.getImages(getBounds());
      monitor.doFinishTask();
    }
  }

  //Attempts to cancel a running download midway, it's hash is removed if download was not able to complete.
  public void finish() {
    state = STATE.STOPPED;
    thread.interrupt();
  }

  //Set state and update download status in {@link DownloadTableModel}
  public void setState(STATE state) {
    this.state = state;
    monitor.model.fireTableCellUpdated(monitor.model.getDownloadRow(this), monitor.model.findColumn("Status"));
  }
}
