package org.openstreetmap.josm.plugins.mapillary.cache;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.data.cache.BufferedImageCacheEntry;
import org.openstreetmap.josm.data.cache.JCSCachedTileLoaderJob;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;

/**
 * Sotres the
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
    FULL_IMAGE, THUMBNAIL
  }

  /**
   * Main constructor.
   *
   * @param key
   * @param type
   */
  public MapillaryCache(String key, Type type) {
    super(MapillaryPlugin.CACHE, 50000, 50000, new HashMap<String, String>());
    this.key = key;
    try {
      if (type == Type.FULL_IMAGE) {
        url = new URL("https://d1cuyjsrcm0gby.cloudfront.net/" + key
            + "/thumb-2048.jpg");
        this.key += ".FULL_IMAGE";

      } else if (type == Type.THUMBNAIL) {
        url = new URL("https://d1cuyjsrcm0gby.cloudfront.net/" + key
            + "/thumb-320.jpg");
        this.key += ".THUMBNAIL";
      }
    } catch (MalformedURLException e) {
      Main.error(e);
    }
  }

  @Override
  public String getCacheKey() {
    return key;
  }

  @Override
  public URL getUrl() {
    return url;
  }

  @Override
  protected BufferedImageCacheEntry createCacheEntry(byte[] content) {
    return new BufferedImageCacheEntry(content);
  }

  @Override
  protected boolean isObjectLoadable() {
    if (cacheData == null)
      return false;
    byte[] content = cacheData.getContent();
    return content != null && content.length > 0;
  }

  // @Override
  protected boolean handleNotFound() {
    return false;
  }
}
