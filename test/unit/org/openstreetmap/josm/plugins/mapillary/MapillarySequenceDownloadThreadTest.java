/**
 *
 */
package org.openstreetmap.josm.plugins.mapillary;

import static org.junit.Assert.assertTrue;

import java.util.Locale;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import org.junit.Before;
import org.junit.Test;
import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.plugins.mapillary.downloads.MapillaryDownloader;
import org.openstreetmap.josm.plugins.mapillary.downloads.MapillarySequenceDownloadThread;
import org.openstreetmap.josm.plugins.mapillary.util.TestUtil;

/**
 *
 */
public class MapillarySequenceDownloadThreadTest {

    /**
     * @throws java.lang.Exception
     */
    @Before
    public void setUp() {
        MapillaryData.TEST_MODE = true;
        TestUtil.initPlugin();
    }

    /**
     * Test method for {@link org.openstreetmap.josm.plugins.mapillary.downloads.MapillarySequenceDownloadThread#run()}.
     *
     * This downloads a small area of mapillary-sequences where we know that images already exist.
     * When the download-thread finishes, we check if the Mapillary-layer now contains one or more images.
     *
     * @throws InterruptedException
     */
    @Test
    public void testRun() throws InterruptedException {
	System.out.println("[JUnit] MapillarySequenceDownloadThreadTest.testRun()");
        //Area around image UjEbeXZYIoyAKOsE-remlg (59.32125452° N 18.06166856° E)
        LatLon minLatLon = new LatLon(59.3212545, 18.0616685);
        LatLon maxLatLon = new LatLon(59.3212546, 18.0616686);

        ExecutorService ex = Executors.newSingleThreadExecutor();
        String queryString = String.format(
            Locale.UK,
            "?max_lat=%.8f&max_lon=%.8f&min_lat=%.8f&min_lon=%.8f&limit=10&client_id=%s",
            maxLatLon.lat(),
            maxLatLon.lon(),
            minLatLon.lat(),
            minLatLon.lon(),
            MapillaryDownloader.CLIENT_ID
        );
        MapillaryLayer.getInstance().bounds.add(new Bounds(minLatLon, maxLatLon));

        int page = 1;
        while (!ex.isShutdown() && MapillaryLayer.getInstance().getMapillaryData().getImages().size() <= 0 && page < 50) {
            System.out.println("Sending sequence-request "+page+" to Mapillary-servers…");
            Thread downloadThread = new MapillarySequenceDownloadThread(ex, queryString+"&page="+page);
            downloadThread.start();
            downloadThread.join();
            page++;
            Thread.sleep(500);
        }
        assertTrue(MapillaryLayer.getInstance().getMapillaryData().getImages().size() >= 1);
        System.out.println("One or more images were added to the MapillaryLayer within the given bounds.");
    }

}
