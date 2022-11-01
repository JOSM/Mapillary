// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.io.export;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.Collection;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

import javax.imageio.ImageIO;

import org.openstreetmap.josm.data.cache.BufferedImageCacheEntry;
import org.openstreetmap.josm.data.cache.CacheEntry;
import org.openstreetmap.josm.data.cache.CacheEntryAttributes;
import org.openstreetmap.josm.data.cache.ICachedLoaderListener;
import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.plugins.mapillary.cache.CacheUtils;
import org.openstreetmap.josm.plugins.mapillary.cache.MapillaryCache;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.MapillaryDownloader;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.tools.Logging;

/**
 * This is the thread that downloads one of the images that are going to be
 * exported and writes them in a {@link ArrayBlockingQueue}.
 *
 * @author nokutu
 * @see MapillaryExportManager
 * @see MapillaryExportWriterThread
 */
public class MapillaryExportDownloadThread implements Runnable, ICachedLoaderListener {
    private static final AtomicInteger THREAD_COUNT = new AtomicInteger();

    private final ArrayBlockingQueue<BufferedImage> queue;
    private final ArrayBlockingQueue<INode> queueImages;

    private final INode image;
    private final MapillaryExportWriterThread exportWriterThread;

    /**
     * Main constructor.
     *
     * @param exportWriterThread
     *        The thread to notify of failures ({@code queue}/{@code queueImages} should both be in the thread)
     * @param image
     *        Image to be downloaded.
     * @param queue
     *        Queue of {@link BufferedImage} objects for the
     *        {@link MapillaryExportWriterThread}.
     * @param queueImages
     *        Queue of {@link INode} objects for the
     *        {@link MapillaryExportWriterThread}.
     */
    public MapillaryExportDownloadThread(MapillaryExportWriterThread exportWriterThread, INode image,
        ArrayBlockingQueue<BufferedImage> queue, ArrayBlockingQueue<INode> queueImages) {
        this.queue = queue;
        this.image = image;
        this.queueImages = queueImages;
        this.exportWriterThread = exportWriterThread;
    }

    @Override
    public void run() {
        if (MapillaryImageUtils.getKey(this.image) != 0) {
            final int threadCount = MapillaryCache.THREAD_LIMIT.get();
            synchronized (THREAD_COUNT) {
                while (THREAD_COUNT.get() > threadCount - 1) {
                    try {
                        THREAD_COUNT.wait(100);
                    } catch (InterruptedException e) {
                        Thread.currentThread().interrupt();
                        return;
                    }
                }
                THREAD_COUNT.incrementAndGet();
            }
            final INode toDownload;
            if (!MapillaryImageUtils.isDownloadable(this.image)) {
                toDownload = MapillaryDownloader.downloadImages(MapillaryImageUtils.getKey(this.image)).values()
                    .stream().flatMap(Collection::stream).findFirst().orElseThrow(() -> new IllegalArgumentException(
                        "Cannot download Mapillary image " + MapillaryImageUtils.getKey(this.image)));
            } else {
                toDownload = this.image;
            }
            CacheUtils.submit(toDownload, MapillaryCache.Type.ORIGINAL, false, this);
        } else {
            throw new UnsupportedOperationException(tr("We cannot export {0}",
                image.getInterestingTags().entrySet().stream()
                    .map(entry -> String.join("=", entry.getKey(), entry.getValue()))
                    .collect(Collectors.joining(", "))));
        }
    }

    @Override
    public synchronized void loadingFinished(CacheEntry data, CacheEntryAttributes attributes, LoadResult result) {
        THREAD_COUNT.decrementAndGet();
        synchronized (THREAD_COUNT) {
            THREAD_COUNT.notifyAll();
        }
        if (result != LoadResult.SUCCESS) {
            this.exportWriterThread.decrementSize();
            return;
        }
        try {
            final BufferedImage bufferedImage;
            if (data instanceof BufferedImageCacheEntry) {
                bufferedImage = ((BufferedImageCacheEntry) data).getImage();
            } else {
                bufferedImage = ImageIO.read(new ByteArrayInputStream(data.getContent()));
            }
            this.queue.put(bufferedImage);
            this.queueImages.put(this.image);
            synchronized (this.queue) {
                this.queue.notifyAll();
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            Logging.error(e);
        } catch (IOException e) {
            Logging.error(e);
        }
    }
}
