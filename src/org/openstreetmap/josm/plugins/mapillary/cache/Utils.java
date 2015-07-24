package org.openstreetmap.josm.plugins.mapillary.cache;

import org.openstreetmap.josm.data.cache.CacheEntry;
import org.openstreetmap.josm.data.cache.CacheEntryAttributes;
import org.openstreetmap.josm.data.cache.ICachedLoaderListener;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;

/**
 * Utility methods for working with cache.
 *
 * @author nokutu
 *
 */
public class Utils {

  private static IgnoreDownload IGNORE_DOWNLOAD = new IgnoreDownload();

  /**
   * Downloads the picture of the given image and does nothing when it is downloaded.
   *
   * @param img
   */
  public static void downloadPicture(MapillaryAbstractImage img) {
    if (!(img instanceof MapillaryImage))
      throw new IllegalArgumentException();
    new MapillaryCache(((MapillaryImage) img).getKey(), MapillaryCache.Type.THUMBNAIL).submit(
        IGNORE_DOWNLOAD, false);
    new MapillaryCache(((MapillaryImage) img).getKey(), MapillaryCache.Type.FULL_IMAGE).submit(
        IGNORE_DOWNLOAD, false);
  }

  private static class IgnoreDownload implements ICachedLoaderListener {

    @Override
    public void loadingFinished(CacheEntry arg0, CacheEntryAttributes arg1,
        LoadResult arg2) {
      // Nothing
    }
  }
}
