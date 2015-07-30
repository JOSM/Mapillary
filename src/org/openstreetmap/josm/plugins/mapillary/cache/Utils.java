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
    /** Thumbnail quality picture (320 p) */
    THUMBNAIL,
    /** Full quality picture (2048 p) */
    FULL_IMAGE,
    /** Both of them */
    BOTH;
  }

  /**
   * Downloads the the thumbnail and the full resolution picture of the given
   * image. Does nothing if it is already in cache.
   *
   * @param img
   *          The image whose picture is going to be downloaded.
   */
  public static void downloadPicture(MapillaryImage img) {
    downloadPicture(img, PICTURE.BOTH);
  }

  /**
   * Downloads the picture of the given image. Does nothing when it is already
   * in cache.
   *
   * @param img
   * @param pic
   *          The picture type to be downloaded (full quality, thumbnail or
   *          both.)
   */
  public static void downloadPicture(MapillaryImage img, PICTURE pic) {
    switch (pic) {
      case BOTH:
        if (new MapillaryCache(img.getKey(), MapillaryCache.Type.THUMBNAIL)
            .get() == null)
          new MapillaryCache(img.getKey(), MapillaryCache.Type.THUMBNAIL)
              .submit(IGNORE_DOWNLOAD, false);
        if (new MapillaryCache(img.getKey(), MapillaryCache.Type.FULL_IMAGE)
            .get() == null)
          new MapillaryCache(img.getKey(), MapillaryCache.Type.FULL_IMAGE)
              .submit(IGNORE_DOWNLOAD, false);
        break;
      case THUMBNAIL:
        new MapillaryCache(img.getKey(), MapillaryCache.Type.THUMBNAIL).submit(
            IGNORE_DOWNLOAD, false);
        break;
      case FULL_IMAGE:
        new MapillaryCache(img.getKey(), MapillaryCache.Type.FULL_IMAGE)
            .submit(IGNORE_DOWNLOAD, false);
        break;
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
