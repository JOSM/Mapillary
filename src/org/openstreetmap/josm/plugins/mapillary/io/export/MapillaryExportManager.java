// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.io.export;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.image.BufferedImage;
import java.io.IOException;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import org.openstreetmap.josm.gui.PleaseWaitRunnable;
import org.openstreetmap.josm.gui.progress.swing.PleaseWaitProgressMonitor;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImportedImage;
import org.openstreetmap.josm.tools.Logging;

/**
 * Export main thread. Exportation works by creating a
 * {@link MapillaryExportWriterThread} and several
 * {@link MapillaryExportDownloadThread}. The second ones download every single
 * image that is going to be exported and stores them in an
 * {@link ArrayBlockingQueue}. Then it is picked by the first one and written on
 * the selected folder. Each image will be named by its key.
 *
 * @author nokutu
 * @see MapillaryExportWriterThread
 * @see MapillaryExportDownloadThread
 */
public class MapillaryExportManager extends PleaseWaitRunnable {

  private final ArrayBlockingQueue<BufferedImage> queue = new ArrayBlockingQueue<>(10);
  private final ArrayBlockingQueue<MapillaryAbstractImage> queueImages = new ArrayBlockingQueue<>(10);

  private int amount;
  private final Set<MapillaryAbstractImage> images;
  private final String path;

  private Thread writer;
  private ThreadPoolExecutor ex;

  /**
   * Main constructor.
   *
   * @param images Set of {@link MapillaryAbstractImage} objects to be exported.
   * @param path Export path.
   */
  public MapillaryExportManager(Set<MapillaryAbstractImage> images, String path) {
    super(
      tr("Downloading…"),
      new PleaseWaitProgressMonitor(tr("Exporting Mapillary Images…")),
      true
    );
    this.images = images == null ? new HashSet<>() : images;
    this.path = path;
    this.amount = this.images.size();
  }

  /**
   * Constructor used to rewrite imported images.
   *
   * @param images
   *          The set of {@link MapillaryImportedImage} object that is going to
   *          be rewritten.
   * @throws IOException
   *           If the file of one of the {@link MapillaryImportedImage} objects
   *           doesn't contain a picture.
   */
  public MapillaryExportManager(List<MapillaryImportedImage> images) throws IOException {
    this(null, null);
    for (MapillaryImportedImage image : images) {
      this.queue.add(image.getImage());
      this.queueImages.add(image);
    }
    this.amount = images.size();
  }

  @Override
  protected void cancel() {
    this.writer.interrupt();
    this.ex.shutdown();
  }

  @Override
  protected void realRun() throws IOException {
    // Starts a writer thread in order to write the pictures on the disk.
    this.writer = new MapillaryExportWriterThread(this.path, this.queue,
        this.queueImages, this.amount, this.getProgressMonitor());
    this.writer.start();
    if (this.path == null) {
      try {
        this.writer.join();
      } catch (InterruptedException e) {
        Logging.error(e);
      }
      return;
    }
    this.ex = new ThreadPoolExecutor(20, 35, 25, TimeUnit.SECONDS,
      new ArrayBlockingQueue<>(10));
    for (MapillaryAbstractImage image : this.images) {
      if (image instanceof MapillaryImage) {
        try {
          this.ex.execute(new MapillaryExportDownloadThread(
              (MapillaryImage) image, this.queue, this.queueImages));
        } catch (Exception e) {
          Logging.error(e);
        }
      } else if (image instanceof MapillaryImportedImage) {
        try {
          this.queue.put(((MapillaryImportedImage) image).getImage());
          this.queueImages.put(image);
        } catch (InterruptedException e) {
          Logging.error(e);
        }
      }
      try {
        // If the queue is full, waits for it to have more space
        // available before executing anything else.
        while (this.ex.getQueue().remainingCapacity() == 0) {
          Thread.sleep(100);
        }
      } catch (Exception e) {
        Logging.error(e);
      }
    }
    try {
      this.writer.join();
    } catch (InterruptedException e) {
      Logging.error(e);
    }
  }

  @Override
  protected void finish() {
  }
}
