package org.openstreetmap.josm.plugins.mapillary.cache;

import org.openstreetmap.josm.data.cache.CacheEntry;
import org.openstreetmap.josm.data.cache.CacheEntryAttributes;
import org.openstreetmap.josm.data.cache.ICachedLoaderListener;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;

/**
 * Utility methods for working with cache.
 *
 * @author nokutu
 *
 */
public class Utils {

  private static IgnoreDownload IGNORE_DOWNLOAD = new IgnoreDownload();

  /** Picture quality */
  public enum PICTURE {
    /** Thumbnail quality picture () */
    THUMBNAIL,
    /** Full quality picture () */
    FULL,
    /** Both of them */
    BOTH;
  }

  /**
   * Downloads the picture of the given image and does nothing when it is
   * downloaded.
   *
   * @param img
   */
  public static void downloadPicture(MapillaryImage img) {
    downloadPicture(img, PICTURE.BOTH);
  }

  /**
   * Downloads the picture of the given image and does nothing when it is
   * downloaded.
   *
   * @param img
   * @param pic
   *          The picture type to be downloaded (full quality, thumbnail or
   *          both.)
   */
  public static void downloadPicture(MapillaryImage img, PICTURE pic) {
    if (pic == PICTURE.BOTH) {
      if (new MapillaryCache(img.getKey(), MapillaryCache.Type.THUMBNAIL).get() == null)
        new MapillaryCache(img.getKey(), MapillaryCache.Type.THUMBNAIL).submit(
            IGNORE_DOWNLOAD, false);
      if (new MapillaryCache(img.getKey(), MapillaryCache.Type.FULL_IMAGE)
          .get() == null)
        new MapillaryCache(img.getKey(), MapillaryCache.Type.FULL_IMAGE)
            .submit(IGNORE_DOWNLOAD, false);
    } else if (pic == PICTURE.THUMBNAIL
        && new MapillaryCache(img.getKey(), MapillaryCache.Type.THUMBNAIL)
            .get() == null) {
      new MapillaryCache(img.getKey(), MapillaryCache.Type.THUMBNAIL).submit(
          IGNORE_DOWNLOAD, false);
    } else if (pic == PICTURE.FULL
        && new MapillaryCache(img.getKey(), MapillaryCache.Type.FULL_IMAGE)
            .get() == null) {
      new MapillaryCache(img.getKey(), MapillaryCache.Type.FULL_IMAGE).submit(
          IGNORE_DOWNLOAD, false);
    }
  }

  private static class IgnoreDownload implements ICachedLoaderListener {

    @Override
    public void loadingFinished(CacheEntry arg0, CacheEntryAttributes arg1,
        LoadResult arg2) {
      // Nothing
    }
  }
}
