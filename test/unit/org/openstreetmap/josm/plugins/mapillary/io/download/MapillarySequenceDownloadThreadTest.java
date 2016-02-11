/**
 *
 */
package org.openstreetmap.josm.plugins.mapillary.io.download;

import static org.junit.Assert.assertTrue;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import org.junit.Test;
import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.plugins.mapillary.AbstractTest;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;

/**
 *
 */
public class MapillarySequenceDownloadThreadTest extends AbstractTest {

  /**
   * Test method for
   * {@link org.openstreetmap.josm.plugins.mapillary.io.download.MapillarySequenceDownloadThread#run()}
   * .
   *
   * This downloads a small area of mapillary-sequences where we know that
   * images already exist. When the download-thread finishes, we check if the
   * Mapillary-layer now contains one or more images.
   *
   * @throws InterruptedException
   */
  @Test
  public void testRun() throws InterruptedException {
    System.out.println("[JUnit] MapillarySequenceDownloadThreadTest.testRun()");
    // Area around image UjEbeXZYIoyAKOsE-remlg (59.32125452° N 18.06166856° E)
    LatLon minLatLon = new LatLon(59.3212545, 18.0616685);
    LatLon maxLatLon = new LatLon(59.3212546, 18.0616686);

    ExecutorService ex = Executors.newSingleThreadExecutor();
    Bounds bounds = new Bounds(minLatLon, maxLatLon);
    MapillaryLayer.getInstance().getData().getBounds().add(new Bounds(minLatLon,
        maxLatLon));

    int page = 1;
    while (!ex.isShutdown()
        && MapillaryLayer.getInstance().getData().getImages().size() <= 0
        && page < 50) {
      System.out.println("Sending sequence-request " + page
          + " to Mapillary-servers…");
      Thread downloadThread = new MapillarySequenceDownloadThread(ex, bounds, page);
      downloadThread.start();
      downloadThread.join();
      page++;
      Thread.sleep(500);
    }
    assertTrue(MapillaryLayer.getInstance().getData().getImages().size() >= 1);
    System.out.println("One or more images were added to the MapillaryLayer within the given bounds.");
  }

}
