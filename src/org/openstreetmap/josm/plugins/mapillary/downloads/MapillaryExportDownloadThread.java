package org.openstreetmap.josm.plugins.mapillary.downloads;

import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.concurrent.ArrayBlockingQueue;

import javax.imageio.ImageIO;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.data.cache.CacheEntry;
import org.openstreetmap.josm.data.cache.CacheEntryAttributes;
import org.openstreetmap.josm.data.cache.ICachedLoaderListener;
import org.openstreetmap.josm.gui.progress.ProgressMonitor;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.cache.MapillaryCache;

/**
 * This is the thread that downloads one of the images that are going to be
 * exported and writes them in a {@link ArrayBlockingQueue}.
 *
 * @author nokutu
 * @see MapillaryExportManager
 * @see MapillaryExportWriterThread
 */
public class MapillaryExportDownloadThread extends Thread implements
    ICachedLoaderListener {

  String url;
  ArrayBlockingQueue<BufferedImage> queue;
  ArrayBlockingQueue<MapillaryAbstractImage> queueImages;

  ProgressMonitor monitor;
  MapillaryImage image;

  /**
   * Main constructor.
   *
   * @param image
   *          Image to be downloaded.
   * @param queue
   *          Queue of {@link BufferedImage} objects for the
   *          {@link MapillaryExportWriterThread}.
   * @param queueImages
   *          Queue of {@link MapillaryAbstractImage} objects for the
   *          {@link MapillaryExportWriterThread}.
   */
  public MapillaryExportDownloadThread(MapillaryImage image,
      ArrayBlockingQueue<BufferedImage> queue,
      ArrayBlockingQueue<MapillaryAbstractImage> queueImages) {
    this.url = "https://d1cuyjsrcm0gby.cloudfront.net/" + image.getKey()
        + "/thumb-2048.jpg";
    this.queue = queue;
    this.image = image;
    this.queueImages = queueImages;
  }

  @Override
  public void run() {
    new MapillaryCache(this.image.getKey(), MapillaryCache.Type.FULL_IMAGE).submit(
        this, false);
  }

  @Override
  public void loadingFinished(CacheEntry data, CacheEntryAttributes attributes,
      LoadResult result) {
    try {
      this.queue.put(ImageIO.read(new ByteArrayInputStream(data.getContent())));
      this.queueImages.put(this.image);
    } catch (InterruptedException e) {
      Main.error(e);
    } catch (IOException e) {
      Main.error(e);
    }
  }
}
