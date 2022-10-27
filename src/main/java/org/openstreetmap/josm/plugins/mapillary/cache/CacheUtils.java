// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.cache;

import java.io.IOException;
import java.util.stream.Stream;

import org.openstreetmap.josm.data.cache.CacheEntry;
import org.openstreetmap.josm.data.cache.CacheEntryAttributes;
import org.openstreetmap.josm.data.cache.ICachedLoaderListener;
import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.plugins.mapillary.gui.workers.MapillaryNodeDownloader;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.tools.Logging;

/**
 * Utility methods for working with cache.
 *
 * @author nokutu
 */
public final class CacheUtils {

    /**
     * Get the default number of keys in a Mapillary image node.
     */
    public static final byte MAPILLARY_DEFAULT_KEY_LENGTH = (byte) Stream
        .of("captured_at", "id", "sequence_id", "organization_id", "is_pano").count();
    private static final IgnoreDownload IGNORE_DOWNLOAD = new IgnoreDownload();

    private CacheUtils() {
        // Private constructor to avoid instantiation
    }

    /**
     * Downloads the picture of the given image. Does nothing when it is already
     * in cache.
     *
     * @param img
     *        The image to be downloaded.
     * @param type
     *        The picture type to be downloaded (full quality, thumbnail or
     *        both.)
     */
    public static void downloadPicture(INode img, MapillaryCache.Type type) {
        if (img.getNumKeys() <= MAPILLARY_DEFAULT_KEY_LENGTH) {
            new MapillaryNodeDownloader(img, n -> {
                if (n != null && n.getNumKeys() > MAPILLARY_DEFAULT_KEY_LENGTH) {
                    downloadPicture(n, type);
                }
            }).execute();
            return;
        }
        if (new MapillaryCache(img, type).get() == null) {
            submit(img, type, IGNORE_DOWNLOAD);
        }
    }

    /**
     * Requests the picture with the given key and quality and uses the given
     * listener.
     *
     * @param image
     *        The picture to be requested.
     * @param lis
     *        The listener that is going to receive the picture.
     */
    public static void submit(INode image, MapillaryCache.Type type, ICachedLoaderListener lis) {
        submit(image, type, true, lis);
    }

    /**
     * Requests the picture with the given key and quality and uses the given
     * listener.
     *
     * @param image
     *        The picture to be requested.
     * @param lis
     *        The listener that is going to receive the picture.
     */
    public static void submit(INode image, MapillaryCache.Type type, boolean removeCurrent, ICachedLoaderListener lis) {
        try {
            final MapillaryCache cache = new MapillaryCache(image, type);
            if (cache.getUrl() != null) {
                cache.submit(lis != null ? lis : IGNORE_DOWNLOAD, false, removeCurrent);
            } else {
                Logging.error("Mapillary: {0} has no url. Maybe API limits have been reached?",
                    MapillaryImageUtils.getKey(image));
            }
        } catch (IOException e) {
            Logging.error(e);
        }
    }

    static class IgnoreDownload implements ICachedLoaderListener {

        @Override
        public void loadingFinished(CacheEntry arg0, CacheEntryAttributes arg1, LoadResult arg2) {
            // Ignore download
        }
    }
}
