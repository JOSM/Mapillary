// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.actions;

import java.awt.image.BufferedImage;

import javax.swing.SwingUtilities;

import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryData;
import org.openstreetmap.josm.plugins.mapillary.MapillaryDataListener;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.cache.CacheUtils;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryMainDialog;

/**
 * Thread containing the walk process.
 *
 * @author nokutu
 *
 */
public class WalkThread extends Thread implements MapillaryDataListener {
  private final int interval;
  private final MapillaryData data;
  private boolean end;
  private final boolean waitForFullQuality;
  private final boolean followSelected;
  private final boolean goForward;
  private BufferedImage lastImage;
  private volatile boolean paused;

  /**
   * Main constructor.
   *
   * @param interval
   *          How often the images switch.
   * @param waitForPicture
   *          If it must wait for the full resolution picture or just the
   *          thumbnail.
   * @param followSelected
   *          Zoom to each image that is selected.
   * @param goForward
   *          true to go forward; false to go backwards.
   */
  public WalkThread(int interval, boolean waitForPicture,
      boolean followSelected, boolean goForward) {
    this.interval = interval;
    this.waitForFullQuality = waitForPicture;
    this.followSelected = followSelected;
    this.goForward = goForward;
    this.data = MapillaryLayer.getInstance().getData();
    this.data.addListener(this);
  }

  @Override
  public void run() {
    try {
      while (!this.end && this.data.getSelectedImage().next() != null) {
        MapillaryAbstractImage image = this.data.getSelectedImage();
        if (image != null && image.next() instanceof MapillaryImage) {
          // Predownload next 10 thumbnails.
          preDownloadImages((MapillaryImage) image.next(), 10, CacheUtils.PICTURE.THUMBNAIL);
          if (this.waitForFullQuality) {
            // Start downloading 3 next full images.
            preDownloadImages((MapillaryImage) image.next(), 3, CacheUtils.PICTURE.FULL_IMAGE);
          }
        }
        try {
          synchronized (this) {
            // Waits for full quality picture.
            final BufferedImage displayImage = MapillaryMainDialog.getInstance().mapillaryImageDisplay.getImage();
            if (this.waitForFullQuality && image instanceof MapillaryImage) {
              while ( displayImage == this.lastImage || displayImage == null || displayImage.getWidth() < 2048) {
                wait(100);
              }
            }
            // Waits for thumbnail.
            else {
              while (displayImage == this.lastImage || displayImage == null || displayImage.getWidth() < 320) {
                wait(100);
              }
            }
            while (this.paused) {
              wait(100);
            }
            wait(this.interval);
            while (this.paused) {
              wait(100);
            }
          }
          this.lastImage = MapillaryMainDialog.getInstance().mapillaryImageDisplay.getImage();
          synchronized (this) {
            if (this.goForward) {
              this.data.selectNext(this.followSelected);
            } else {
              this.data.selectPrevious(this.followSelected);
            }
          }
        } catch (InterruptedException e) {
          return;
        }
      }
    } catch (NullPointerException e) {
      // TODO: Avoid NPEs instead of waiting until they are thrown and then catching them
      return;
    }
    end();
  }

  /**
   * Downloads n images into the cache beginning from the supplied start-image (including the start-image itself).
   * @param startImage the image to start with (this and the next n-1 images in the same sequence are downloaded)
   * @param n the number of images to download
   * @param type the quality of the image (full or thumbnail)
   */
  private void preDownloadImages(MapillaryImage startImage, int n, CacheUtils.PICTURE type) {
    if (n >= 1 && startImage != null) {
      CacheUtils.downloadPicture(startImage, type);
      if (startImage.next() instanceof MapillaryImage && n >= 2) {
        preDownloadImages((MapillaryImage) startImage.next(), n - 1, type);
      }
    }
  }

  @Override
  public void imagesAdded() {
    // Nothing
  }

  @Override
  public void selectedImageChanged(MapillaryAbstractImage oldImage, MapillaryAbstractImage newImage) {
    if (newImage != oldImage.next()) {
      end();
      interrupt();
    }
  }

  /**
   * Continues with the execution if paused.
   */
  public void play() {
    this.paused = false;
  }

  /**
   * Pauses the execution.
   */
  public void pause() {
    this.paused = true;
  }

  /**
   * Stops the execution.
   */
  public void stopWalk() {
    if (SwingUtilities.isEventDispatchThread()) {
      end();
      this.interrupt();
    } else {
      SwingUtilities.invokeLater(new Runnable() {
        @Override
        public void run() {
          stopWalk();
        }
      });
    }
  }

  /**
   * Called when the walk stops by itself of forcefully.
   */
  public void end() {
    if (SwingUtilities.isEventDispatchThread()) {
      this.end = true;
      this.data.removeListener(this);
      MapillaryMainDialog.getInstance().setMode(MapillaryMainDialog.MODE.NORMAL);
    } else {
      SwingUtilities.invokeLater(new Runnable() {
        @Override
        public void run() {
          end();
        }
      });
    }
  }
}
