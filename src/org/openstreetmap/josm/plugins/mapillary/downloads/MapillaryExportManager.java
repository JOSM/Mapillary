package org.openstreetmap.josm.plugins.mapillary.downloads;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.image.BufferedImage;
import java.io.IOException;
import java.util.List;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.gui.PleaseWaitRunnable;
import org.openstreetmap.josm.gui.progress.PleaseWaitProgressMonitor;
import org.openstreetmap.josm.io.OsmTransferException;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImportedImage;
import org.xml.sax.SAXException;

/**
 * Export main thread. Exportation works by creating a
 * {@link MapillaryExportWriterThread} and several
 * {@link MapillaryExportDownloadThread}. The second ones download every single
 * image that is going to be exported and stores them in an
 * {@link ArrayBlockingQueue}. Then it is picked by the first one and written on
 * the selected folder. Each image will be named by its key.
 *
 * @author nokutu
 *
 */
public class MapillaryExportManager extends PleaseWaitRunnable {

  ArrayBlockingQueue<BufferedImage> queue;
  ArrayBlockingQueue<MapillaryAbstractImage> queueImages;

  final int amount;
  List<MapillaryAbstractImage> images;
  String path;

  private Thread writer;
  private ThreadPoolExecutor ex;

  /**
   * Main constructor.
   *
   * @param images
   *          Set of {@link MapillaryAbstractImage} objects to be exported.
   * @param path
   *          Export path.
   */
  public MapillaryExportManager(List<MapillaryAbstractImage> images, String path) {
    super(tr("Downloading") + "...", new PleaseWaitProgressMonitor(
        "Exporting Mapillary Images"), true);
    this.queue = new ArrayBlockingQueue<>(10);
    this.queueImages = new ArrayBlockingQueue<>(10);

    this.images = images;
    this.amount = images.size();
    this.path = path;
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
  public MapillaryExportManager(List<MapillaryImportedImage> images)
      throws IOException {
    super(tr("Downloading") + "...", new PleaseWaitProgressMonitor(
        "Exporting Mapillary Images"), true);
    this.queue = new ArrayBlockingQueue<>(10);
    this.queueImages = new ArrayBlockingQueue<>(10);
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
  protected void realRun() throws SAXException, IOException,
      OsmTransferException {
    // Starts a writer thread in order to write the pictures on the disk.
    this.writer = new MapillaryExportWriterThread(this.path, this.queue,
        this.queueImages, this.amount, this.getProgressMonitor());
    this.writer.start();
    if (this.path == null) {
      try {
        this.writer.join();
      } catch (InterruptedException e) {
        Main.error(e);
      }
      return;
    }
    this.ex = new ThreadPoolExecutor(20, 35, 25, TimeUnit.SECONDS,
        new ArrayBlockingQueue<Runnable>(10));
    for (MapillaryAbstractImage image : this.images) {
      if (image instanceof MapillaryImage) {
        try {
          this.ex.execute(new MapillaryExportDownloadThread(
              (MapillaryImage) image, this.queue, this.queueImages));
        } catch (Exception e) {
          Main.error(e);
        }
      } else if (image instanceof MapillaryImportedImage) {
        try {
          this.queue.put(((MapillaryImportedImage) image).getImage());
          this.queueImages.put(image);
        } catch (InterruptedException e) {
          Main.error(e);
        }
      }
      try {
        // If the queue is full, waits for it to have more space
        // available before executing anything else.
        while (this.ex.getQueue().remainingCapacity() == 0)
          Thread.sleep(100);
      } catch (Exception e) {
        Main.error(e);
      }
    }
    try {
      this.writer.join();
    } catch (InterruptedException e) {
      Main.error(e);
    }

  }

  @Override
  protected void finish() {
    // TODO Auto-generated method stub
  }
}
