// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.cache;

import java.io.IOException;
import java.util.stream.Stream;

import org.openstreetmap.josm.data.cache.CacheEntry;
import org.openstreetmap.josm.data.cache.CacheEntryAttributes;
import org.openstreetmap.josm.data.cache.ICachedLoaderListener;
import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.plugins.mapillary.io.download.MapillaryDownloader;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;
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

    /** Picture quality */
    public enum PICTURE {
        /** Thumbnail quality picture (320 p) */
        THUMBNAIL,
        /** Full quality picture (2048 p) */
        FULL_IMAGE,
        /** Both of them */
        BOTH
    }

    private CacheUtils() {
        // Private constructor to avoid instantiation
    }

    /**
     * Downloads the the thumbnail and the full resolution picture of the given
     * image. Does nothing if it is already in cache.
     *
     * @param img
     *        The image whose picture is going to be downloaded.
     */
    public static void downloadPicture(INode img) {
        downloadPicture(img, PICTURE.BOTH);
    }

    /**
     * Downloads the picture of the given image. Does nothing when it is already
     * in cache.
     *
     * @param img
     *        The image to be downloaded.
     * @param pic
     *        The picture type to be downloaded (full quality, thumbnail or
     *        both.)
     */
    public static void downloadPicture(INode img, PICTURE pic) {
        if (img.getNumKeys() <= MAPILLARY_DEFAULT_KEY_LENGTH) {
            MapillaryDownloader.downloadImages(MapillaryImageUtils.getKey(img));
            if (img.getNumKeys() <= MAPILLARY_DEFAULT_KEY_LENGTH) {
                return;
            }
            downloadPicture(img, pic);
            return;
        }
        if (new MapillaryCache(img).get() == null)
            submit(img, IGNORE_DOWNLOAD);
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
    public static void submit(INode image, ICachedLoaderListener lis) {
        // Run the initial cache creation in a separate thread -- it may attempt downloads
        MapillaryUtils.getForkJoinPool().execute(() -> {
            try {
                final MapillaryCache cache = new MapillaryCache(image);
                if (cache.getUrl() != null) {
                    cache.submit(lis != null ? lis : IGNORE_DOWNLOAD, false);
                } else {
                    Logging.error("Mapillary: {0} has no url. Maybe API limits have been reached?",
                        MapillaryImageUtils.getKey(image));
                }
            } catch (IOException e) {
                Logging.error(e);
            }
        });
    }

    static class IgnoreDownload implements ICachedLoaderListener {

        @Override
        public void loadingFinished(CacheEntry arg0, CacheEntryAttributes arg1, LoadResult arg2) {
            // Ignore download
        }
    }
}
