package org.openstreetmap.josm.plugins.mapillary.actions;

import java.awt.image.BufferedImage;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryData;
import org.openstreetmap.josm.plugins.mapillary.MapillaryDataListener;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.cache.Utils;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryMainDialog;

/**
 * Thread containing the walk process.
 *
 * @author nokutu
 *
 */
public class WalkThread extends Thread implements MapillaryDataListener {
  private int interval;
  private MapillaryData data;
  private Lock lock = new ReentrantLock();
  private boolean end = false;
  private boolean waitForPicture;
  private BufferedImage lastImage;
  private volatile boolean paused = false;

  /**
   * Main constructor.
   *
   * @param interval
   * @param waitForPicture
   */
  public WalkThread(int interval, boolean waitForPicture) {
    this.interval = interval;
    this.waitForPicture = waitForPicture;
    data = MapillaryLayer.getInstance().getMapillaryData();
    data.addListener(this);
  }

  @Override
  public void run() {
    try {
      while (!end && data.getSelectedImage().next() != null) {
        MapillaryAbstractImage image = data.getSelectedImage();
        for (int i = 0; i < 5; i++) {
          if (image.next() == null)
            break;
          image = image.next();
          Utils.downloadPicture(image);
        }
        try {
          synchronized (this) {
            if (waitForPicture
                && data.getSelectedImage() instanceof MapillaryImage) {
              while (MapillaryMainDialog.getInstance().mapillaryImageDisplay
                  .getImage() == lastImage
                  || MapillaryMainDialog.getInstance().mapillaryImageDisplay
                      .getImage() == null
                  || MapillaryMainDialog.getInstance().mapillaryImageDisplay
                      .getImage().getWidth() < 2048)
                wait(100);
            } else {
              while (MapillaryMainDialog.getInstance().mapillaryImageDisplay
                  .getImage() == lastImage
                  || MapillaryMainDialog.getInstance().mapillaryImageDisplay
                      .getImage() == null
                  || MapillaryMainDialog.getInstance().mapillaryImageDisplay
                      .getImage().getWidth() < 320)
                wait(100);
            }
            while (paused)
              wait(100);
            wait(interval);
            while (paused)
              wait(100);
          }
          lastImage = MapillaryMainDialog.getInstance().mapillaryImageDisplay
              .getImage();
          synchronized (lock) {
            data.selectNext();
          }
        } catch (InterruptedException e) {
          return;
        }
      }
    } catch (NullPointerException e) {
      return;
    }
    end = true;
    data.removeListener(this);
    MapillaryMainDialog.getInstance().setMode(MapillaryMainDialog.Mode.NORMAL);
  }

  @Override
  public void interrupt() {
    end = true;
    data.removeListener(this);
    MapillaryMainDialog.getInstance().setMode(MapillaryMainDialog.Mode.NORMAL);
    super.interrupt();
  }

  @Override
  public void imagesAdded() {
    // Nothing
  }

  @Override
  public void selectedImageChanged(MapillaryAbstractImage oldImage,
      MapillaryAbstractImage newImage) {
    if (newImage != oldImage.next()) {
      synchronized (lock) {
        interrupt();
      }
    }
  }

  /**
   * Continues with the execution if paused.
   */
  public void play() {
    paused = false;
  }

  /**
   * Pauses the execution.
   */
  public void pause() {
    paused = true;
  }

  /**
   * Stops the execution.
   */
  public void stopWalk() {
    this.interrupt();
  }
}
