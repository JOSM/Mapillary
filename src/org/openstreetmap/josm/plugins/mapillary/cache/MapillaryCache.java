package org.openstreetmap.josm.plugins.mapillary.cache;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.data.cache.BufferedImageCacheEntry;
import org.openstreetmap.josm.data.cache.JCSCachedTileLoaderJob;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;

/**
 * Stores the downloaded pictures locally.
 *
 * @author nokutu
 *
 */
public class MapillaryCache extends
    JCSCachedTileLoaderJob<String, BufferedImageCacheEntry> {

  private volatile URL url;
  private volatile String key;

  /**
   * Types of images.
   *
   * @author nokutu
   */
  public static enum Type {
    /** Full quality image */
    FULL_IMAGE,
    /** Low quality image */
    THUMBNAIL
  }

  /**
   * Main constructor.
   *
   * @param key
   *          The key of the image.
   * @param type
   *          The type of image that must be downloaded (THUMBNAIL or
   *          FULL_IMAGE).
   */
  public MapillaryCache(String key, Type type) {
    super(MapillaryPlugin.CACHE, 50000, 50000, new HashMap<String, String>());
    this.key = key;
    try {
      switch (type) {
        case FULL_IMAGE:
          this.url = new URL("https://d1cuyjsrcm0gby.cloudfront.net/" + key
              + "/thumb-2048.jpg");
          this.key += ".FULL_IMAGE";
          break;
        case THUMBNAIL:
          this.url = new URL("https://d1cuyjsrcm0gby.cloudfront.net/" + key
              + "/thumb-320.jpg");
          this.key += ".THUMBNAIL";
          break;
      }
    } catch (MalformedURLException e) {
      Main.error(e);
    }
  }

  @Override
  public String getCacheKey() {
    return this.key;
  }

  @Override
  public URL getUrl() {
    return this.url;
  }

  @Override
  protected BufferedImageCacheEntry createCacheEntry(byte[] content) {
    return new BufferedImageCacheEntry(content);
  }

  @Override
  protected boolean isObjectLoadable() {
    if (this.cacheData == null)
      return false;
    byte[] content = this.cacheData.getContent();
    return content != null && content.length > 0;
  }
}
