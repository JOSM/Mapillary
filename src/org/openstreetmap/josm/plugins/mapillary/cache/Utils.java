package org.openstreetmap.josm.plugins.mapillary.cache;

import org.openstreetmap.josm.data.cache.CacheEntry;
import org.openstreetmap.josm.data.cache.CacheEntryAttributes;
import org.openstreetmap.josm.data.cache.ICachedLoaderListener;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;

/**
 * Downloads and stores pictures in cache.
 *
 * @author nokutu
 *
 */
public class Utils implements ICachedLoaderListener {

  static Utils INSTANCE = new Utils();


  /**
   * Downloads the picture of the given image.
   *
   * @param img
   */
  public static void downloadPicture(MapillaryAbstractImage img) {
    if (!(img instanceof MapillaryImage))
      throw new IllegalArgumentException();
    new MapillaryCache(((MapillaryImage) img).getKey(), MapillaryCache.Type.THUMBNAIL).submit(
        INSTANCE, false);
    new MapillaryCache(((MapillaryImage) img).getKey(), MapillaryCache.Type.FULL_IMAGE).submit(
        INSTANCE, false);
  }

  @Override
  public void loadingFinished(CacheEntry arg0, CacheEntryAttributes arg1,
      LoadResult arg2) {
    // Nothing
  }

}
