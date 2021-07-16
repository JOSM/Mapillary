// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.actions;

import java.io.IOException;
import java.io.Serializable;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicBoolean;

import javax.annotation.Nonnull;
import javax.swing.SwingUtilities;

import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.data.osm.IWay;
import org.openstreetmap.josm.data.osm.OsmData;
import org.openstreetmap.josm.data.osm.event.IDataSelectionListener;
import org.openstreetmap.josm.data.vector.VectorDataSet;
import org.openstreetmap.josm.data.vector.VectorNode;
import org.openstreetmap.josm.data.vector.VectorPrimitive;
import org.openstreetmap.josm.data.vector.VectorRelation;
import org.openstreetmap.josm.data.vector.VectorWay;
import org.openstreetmap.josm.gui.Notification;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.cache.CacheUtils;
import org.openstreetmap.josm.plugins.mapillary.cache.MapillaryCache;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.VectorDataSelectionListener;
import org.openstreetmap.josm.plugins.mapillary.gui.DeveloperToggleAction;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryMainDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillarySequenceUtils;
import org.openstreetmap.josm.tools.I18n;
import org.openstreetmap.josm.tools.Logging;

/**
 * Thread containing the walk process.
 *
 * @author nokutu
 */
public class WalkThread extends Thread implements Serializable, VectorDataSelectionListener {
  private final int interval;
  private final OsmData<?, ?, ?, ?> data;
  private final MapillaryLayer layer;
  private boolean endWalk;
  private final boolean waitForPicture;
  private final MapillarySequenceUtils.NextOrPrevious goForward;
  private final AtomicBoolean paused = new AtomicBoolean();

  /**
   * Main constructor.
   *
   * @param interval How often the images switch.
   * @param waitForPicture If it must wait for the full resolution picture or just the
   *        thumbnail.
   * @param followSelected Zoom to each image that is selected.
   * @param goForward See {@link org.openstreetmap.josm.plugins.mapillary.utils.MapillarySequenceUtils.NextOrPrevious}
   */
  public WalkThread(int interval, boolean waitForPicture, boolean followSelected,
    MapillarySequenceUtils.NextOrPrevious goForward) {
    this.interval = interval;
    this.waitForPicture = waitForPicture;
    this.goForward = goForward;
    this.layer = MapillaryLayer.getInstance();
    this.data = this.layer.getData();
    // This currently causes a ConcurrentModificationException TODO fix
    // this.layer.getData().addSelectionListener(this);
  }

  @Override
  public void run() {
    try {
      INode curSelection;
      INode curImage;
      while (!this.endWalk && (curSelection = this.data.getSelectedNodes().stream().findFirst().orElse(null)) != null
        && (curImage = MapillaryImageUtils.getKey(curSelection) != null ? curSelection : null) != null) {
        // Predownload next 10 thumbnails.
        preDownloadImages(curImage, 10, CacheUtils.PICTURE.THUMBNAIL, goForward);
        if (this.waitForPicture) {
          // Start downloading 3 next full images.
          preDownloadImages(curImage, 3, CacheUtils.PICTURE.FULL_IMAGE, goForward);
        }
        // Wait for picture for 1 minute.
        awaitImageDownload(curImage);
        synchronized (this.paused) {
          while (this.paused.get()) {
            this.paused.wait();
          }
        }
        Thread.sleep(this.interval);

        final INode selectedImage = this.data.getSelectedNodes().stream().findFirst().orElse(null);
        if (!Objects.equals(selectedImage, curSelection)) {
          // This is just a filler until we the TODO fix ConcurrentModification error is fixed in the constructor
          return;
        }
        if (selectedImage != null
          && selectedImage.getReferrers().stream().filter(IWay.class::isInstance).count() == 1) {
          final INode nextSelectedImg = MapillarySequenceUtils.getNextOrPrevious(selectedImage, this.goForward);
          if (nextSelectedImg != null && nextSelectedImg.isVisible()) {
            this.layer.setSelected(nextSelectedImg);
          }
        }
      }
    } catch (InterruptedException interruptedException) {
      Logging.warn(interruptedException);
      Thread.currentThread().interrupt();
    } catch (IOException e) {
      Logging.warn(e);
    } catch (NullPointerException e) {
      Logging.warn(e);
      // TODO: Avoid NPEs instead of waiting until they are thrown and then catching them
      // Until then, rethrow if developer.
      if (DeveloperToggleAction.isDeveloper()) {
        throw e;
      }
    } finally {
      end();
    }
  }

  /**
   * Await the download of an image
   *
   * @param curImage The image waiting on download
   * @throws IOException See {@link MapillaryCache#submit(ICachedLoaderListener, boolean)}
   * @throws InterruptedException See {@link #wait(long)}
   */
  private static void awaitImageDownload(@Nonnull final INode curImage) throws IOException, InterruptedException {
    // Wait for picture for 1 minute.
    final MapillaryCache cache = new MapillaryCache(curImage);
    cache.submit((a, b, c) -> {
      synchronized (cache) {
        cache.notifyAll();
      }
    }, false);
    int limit = 600; // 600 * 100 = 60_000 ms
    synchronized (cache) {
      try {
        while (cache.get() == null) {
          cache.wait(100);
          limit--;
          if (limit < 0) {
            new Notification(I18n.tr("Walk mode: Waiting for next image takes too long! Exiting walk mode…"))
              .setIcon(MapillaryPlugin.LOGO.get()).show();
            return;
          }
        }
      } finally {
        cache.notifyAll();
      }
    }
  }

  /**
   * Downloads n images into the cache beginning from the supplied start-image (including the start-image itself).
   *
   * @param startImage the image to start with (this and the next n-1 images in the same sequence are downloaded)
   * @param n the number of images to download
   * @param type the quality of the image (full or thumbnail)
   * @param goForward
   *        {@link org.openstreetmap.josm.plugins.mapillary.utils.MapillarySequenceUtils.NextOrPrevious#NEXT} if the
   *        next images,
   *        {@link org.openstreetmap.josm.plugins.mapillary.utils.MapillarySequenceUtils.NextOrPrevious#PREVIOUS} if the
   *        previous images
   *        should be downloaded
   */
  private static void preDownloadImages(INode startImage, int n, CacheUtils.PICTURE type,
    final MapillarySequenceUtils.NextOrPrevious goForward) {
    if (n >= 1 && startImage != null) {
      CacheUtils.downloadPicture(startImage, type);
      final INode nextImg = MapillarySequenceUtils.getNextOrPrevious(startImage, goForward);
      if (MapillaryImageUtils.getKey(nextImg) != null && n > 1) {
        preDownloadImages(nextImg, n - 1, type, goForward);
      }
    }
  }

  @Override
  public void selectionChanged(
    final IDataSelectionListener.SelectionChangeEvent<VectorPrimitive, VectorNode, VectorWay, VectorRelation, VectorDataSet> event) {
    if (event.getSelection().size() > 1 || !event.isNop()) {
      end();
      interrupt();
    }
  }

  /**
   * Continues with the execution if paused.
   */
  public void play() {
    synchronized (this.paused) {
      this.paused.set(false);
      this.paused.notifyAll();
    }
  }

  /**
   * Pauses the execution.
   */
  public void pause() {
    synchronized (this.paused) {
      this.paused.set(true);
      this.paused.notifyAll();
    }
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
      this.endWalk = true;
      this.layer.getData().removeSelectionListener(this);
      MapillaryMainDialog.getInstance().setMode(MapillaryMainDialog.MODE.NORMAL);
    } else {
      SwingUtilities.invokeLater(this::end);
    }
  }
}
