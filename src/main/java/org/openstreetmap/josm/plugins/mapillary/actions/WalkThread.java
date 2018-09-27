// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.actions;

import javax.swing.SwingUtilities;

import org.openstreetmap.josm.gui.Notification;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryData;
import org.openstreetmap.josm.plugins.mapillary.MapillaryDataListener;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.cache.CacheUtils;
import org.openstreetmap.josm.plugins.mapillary.cache.MapillaryCache;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryMainDialog;
import org.openstreetmap.josm.tools.I18n;
import org.openstreetmap.josm.tools.Logging;

/**
 * Thread containing the walk process.
 *
 * @author nokutu
 */
public class WalkThread extends Thread implements MapillaryDataListener {
  private final int interval;
  private final MapillaryData data;
  private boolean end;
  private final boolean waitForFullQuality;
  private final boolean followSelected;
  private final boolean goForward;
  private volatile boolean paused;

  /**
   * Main constructor.
   *
   * @param interval How often the images switch.
   * @param waitForPicture If it must wait for the full resolution picture or just the
   * thumbnail.
   * @param followSelected Zoom to each image that is selected.
   * @param goForward true to go forward; false to go backwards.
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
      MapillaryAbstractImage curSelection;
      MapillaryImage curImage;
      while (
          !this.end &&
          (curSelection = this.data.getSelectedImage().next()) != null &&
          (curImage = curSelection instanceof MapillaryImage ? (MapillaryImage) curSelection : null) != null
      ) {
        // Predownload next 10 thumbnails.
        preDownloadImages(curImage, 10, CacheUtils.PICTURE.THUMBNAIL, goForward);
        if (this.waitForFullQuality) {
          // Start downloading 3 next full images.
          preDownloadImages(curImage, 3, CacheUtils.PICTURE.FULL_IMAGE, goForward);
        }
        try {
          // Wait for picture for 1 minute.
          final MapillaryCache cache = new MapillaryCache(curImage.getKey(), waitForFullQuality ? MapillaryCache.Type.FULL_IMAGE : MapillaryCache.Type.THUMBNAIL);
          int limit = 240; // 240 * 250 = 60000 ms
          while (cache.get() == null) {
            Thread.sleep(250);
            if (limit-- < 0) {
              new Notification(I18n.tr("Walk mode: Waiting for next image takes too long! Exiting walk mode…"))
                  .setIcon(MapillaryPlugin.LOGO.get())
                  .show();
              end();
              return;
            }
          }
          while (this.paused) {
            Thread.sleep(100);
          }
          Thread.sleep(this.interval);
          while (this.paused) {
            Thread.sleep(100);
          }
          final MapillaryAbstractImage selectedImage = this.data.getSelectedImage();
          if (selectedImage != null) {
            final MapillaryAbstractImage nextSelectedImg = this.goForward ? selectedImage.next() : selectedImage.previous();
            if (nextSelectedImg != null && nextSelectedImg.isVisible()) {
              this.data.setSelectedImage(nextSelectedImg, followSelected);
            }
          }
        } catch (InterruptedException e) {
          end();
          return;
        }

      }
    } catch (NullPointerException e) {
      Logging.warn(e);
      end();
      // TODO: Avoid NPEs instead of waiting until they are thrown and then catching them
      return;
    }
    end();
  }

  /**
   * Downloads n images into the cache beginning from the supplied start-image (including the start-image itself).
   *
   * @param startImage the image to start with (this and the next n-1 images in the same sequence are downloaded)
   * @param n the number of images to download
   * @param type the quality of the image (full or thumbnail)
   * @param goForward true if the next images, false if the previous ones should be downloaded
   */
  private static void preDownloadImages(MapillaryImage startImage, int n, CacheUtils.PICTURE type, final boolean goForward) {
    if (n >= 1 && startImage != null) {
      CacheUtils.downloadPicture(startImage, type);
      final MapillaryAbstractImage nextImg = goForward ? startImage.next() : startImage.previous();
      if (nextImg instanceof MapillaryImage && n >= 2) {
        preDownloadImages((MapillaryImage) nextImg, n - 1, type, goForward);
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
      SwingUtilities.invokeLater(this::stopWalk);
    }
  }

  /**
   * Called when the walk stops by itself of forcefully.
   */
  private void end() {
    if (SwingUtilities.isEventDispatchThread()) {
      this.end = true;
      this.data.removeListener(this);
      MapillaryMainDialog.getInstance().setMode(MapillaryMainDialog.MODE.NORMAL);
    } else {
      SwingUtilities.invokeLater(this::end);
    }
  }
}
