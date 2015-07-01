package org.openstreetmap.josm.plugins.mapillary.downloads;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.ArrayBlockingQueue;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryFilterDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryToggleDialog;

/**
 * This Class is needed to create an indeterminate amount of downloads, because
 * the Mapillary API has a parameter called page which is needed when the
 * amount of requested images is quite big.
 * 
 * @author nokutu
 * 
 * @see MapillaryDownloader
 */
public class MapillarySquareDownloadManagerThread implements Runnable {

    private final String urlImages;
    private final String urlSequences;
    private final String urlSigns;
    private final MapillaryLayer layer;
    public boolean imagesAdded = false;

    public MapillarySquareDownloadManagerThread(String urlImages,
            String urlSequences, String urlSigns, MapillaryLayer layer) {
        this.urlImages = urlImages;
        this.urlSequences = urlSequences;
        this.urlSigns = urlSigns;
        this.layer = layer;
    }

    public void run() {
        Main.map.statusLine.setHelpText("Downloading images from Mapillary");
        try {
            downloadSequences();
            if (imagesAdded) {
                Main.map.statusLine
                        .setHelpText("Downloading image's information");
                completeImages();
                MapillaryToggleDialog.getInstance().updateTitle();
                Main.map.statusLine.setHelpText("Downloading signs");
                downloadSigns();
            }
        } catch (InterruptedException e) {
            Main.error(e);
        }
        if (layer.data.getImages().size() > 0)
            Main.map.statusLine.setHelpText(tr("Total images: ")
                    + layer.data.getImages().size());
        else
            Main.map.statusLine.setHelpText(tr("No images found"));
        layer.data.dataUpdated();
        MapillaryFilterDialog.getInstance().refresh();
        MapillaryToggleDialog.getInstance().updateImage();
    }

    private void downloadSequences() throws InterruptedException {
        ThreadPoolExecutor ex = new ThreadPoolExecutor(3, 5, 25,
                TimeUnit.SECONDS, new ArrayBlockingQueue<Runnable>(5));
        int page = 0;
        while (!ex.isShutdown()) {
            ex.execute(new MapillarySequenceDownloadThread(ex, urlSequences
                    + "&page=" + page + "&limit=1", layer, this));
            while (ex.getQueue().remainingCapacity() == 0)
                Thread.sleep(100);
            page++;
        }
        ex.awaitTermination(15, TimeUnit.SECONDS);
        layer.data.dataUpdated();
    }

    private void completeImages() throws InterruptedException {
        ThreadPoolExecutor ex = new ThreadPoolExecutor(3, 5, 25,
                TimeUnit.SECONDS, new ArrayBlockingQueue<Runnable>(5));
        int page = 0;
        while (!ex.isShutdown()) {
            ex.execute(new MapillaryImageInfoDownloaderThread(ex, urlImages
                    + "&page=" + page + "&limit=20", layer));
            while (ex.getQueue().remainingCapacity() == 0)
                Thread.sleep(100);
            page++;
        }
        ex.awaitTermination(15, TimeUnit.SECONDS);
    }

    private void downloadSigns() throws InterruptedException {
        ThreadPoolExecutor ex = new ThreadPoolExecutor(3, 5, 25,
                TimeUnit.SECONDS, new ArrayBlockingQueue<Runnable>(5));
        int page = 0;
        while (!ex.isShutdown()) {
            ex.execute(new MapillarySignDownloaderThread(ex, urlSigns
                    + "&page=" + page + "&limit=20", layer));
            while (ex.getQueue().remainingCapacity() == 0)
                Thread.sleep(100);
            page++;
        }
        ex.awaitTermination(15, TimeUnit.SECONDS);
    }
}
