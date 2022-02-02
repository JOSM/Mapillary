// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.io.export;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.stream.Collectors;

import javax.imageio.ImageIO;

import org.openstreetmap.josm.data.cache.CacheEntry;
import org.openstreetmap.josm.data.cache.CacheEntryAttributes;
import org.openstreetmap.josm.data.cache.ICachedLoaderListener;
import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.plugins.mapillary.cache.CacheUtils;
import org.openstreetmap.josm.plugins.mapillary.cache.MapillaryCache;
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

    private final ArrayBlockingQueue<BufferedImage> queue;
    private final ArrayBlockingQueue<INode> queueImages;

    private final INode image;

    /**
     * Main constructor.
     *
     * @param image
     *        Image to be downloaded.
     * @param queue
     *        Queue of {@link BufferedImage} objects for the
     *        {@link MapillaryExportWriterThread}.
     * @param queueImages
     *        Queue of {@link INode} objects for the
     *        {@link MapillaryExportWriterThread}.
     */
    public MapillaryExportDownloadThread(INode image, ArrayBlockingQueue<BufferedImage> queue,
        ArrayBlockingQueue<INode> queueImages) {
        this.queue = queue;
        this.image = image;
        this.queueImages = queueImages;
    }

    @Override
    public void run() {
        if (MapillaryImageUtils.getKey(this.image) != 0) {
            CacheUtils.submit(this.image, MapillaryCache.Type.ORIGINAL, this);
        } else {
            throw new UnsupportedOperationException(tr("We cannot export {0}",
                image.getInterestingTags().entrySet().stream()
                    .map(entry -> String.join("=", entry.getKey(), entry.getValue()))
                    .collect(Collectors.joining(", "))));
        }
    }

    @Override
    public synchronized void loadingFinished(CacheEntry data, CacheEntryAttributes attributes, LoadResult result) {
        try {
            this.queue.put(ImageIO.read(new ByteArrayInputStream(data.getContent())));
            this.queueImages.put(this.image);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            Logging.error(e);
        } catch (IOException e) {
            Logging.error(e);
        }
    }
}
