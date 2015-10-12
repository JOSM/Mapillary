package org.openstreetmap.josm.plugins.mapillary.cache;

import java.io.IOException;

import org.openstreetmap.josm.Main;
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
public class CacheUtils {

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
   *          The image to be downloaded.
   * @param pic
   *          The picture type to be downloaded (full quality, thumbnail or
   *          both.)
   */
  public static void downloadPicture(MapillaryImage img, PICTURE pic) {
    switch (pic) {
      case BOTH:
        if (new MapillaryCache(img.getKey(), MapillaryCache.Type.THUMBNAIL)
            .get() == null)
          submit(img.getKey(), MapillaryCache.Type.THUMBNAIL, IGNORE_DOWNLOAD);
        if (new MapillaryCache(img.getKey(), MapillaryCache.Type.FULL_IMAGE)
            .get() == null)
          submit(img.getKey(), MapillaryCache.Type.FULL_IMAGE, IGNORE_DOWNLOAD);
        break;
      case THUMBNAIL:
        submit(img.getKey(), MapillaryCache.Type.THUMBNAIL, IGNORE_DOWNLOAD);
        break;
      case FULL_IMAGE:
        submit(img.getKey(), MapillaryCache.Type.FULL_IMAGE, IGNORE_DOWNLOAD);
        break;
    }
  }

  /**
   * Requests the picture with the given key and quality and uses the given
   * listener.
   *
   * @param key
   *          The key of the picture to be requested.
   * @param type
   *          The quality of the picture to be requested.
   * @param lis
   *          The listener that is going to receive the picture.
   */
  public static void submit(String key, MapillaryCache.Type type,
      ICachedLoaderListener lis) {
    try {
      new MapillaryCache(key, type).submit(lis, false);
    } catch (IOException e) {
      Main.error(e);
    }
  }

  private static class IgnoreDownload implements ICachedLoaderListener {

    @Override
    public void loadingFinished(CacheEntry arg0, CacheEntryAttributes arg1,
        LoadResult arg2) {
    }
  }
}
