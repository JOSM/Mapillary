// License: GPL. For details, see LICENSE file.
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
public class MapillaryCache extends JCSCachedTileLoaderJob<String, BufferedImageCacheEntry> {

  private final URL url;
  private final String key;

  /**
   * Types of images.
   *
   * @author nokutu
   */
  public enum Type {
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
    String k = null;
    URL u = null;
    if (key != null && type != null) {
      try {
        switch (type) {
          case FULL_IMAGE:
            k = key + ".FULL_IMAGE";
            u = new URL("https://d1cuyjsrcm0gby.cloudfront.net/" + key + "/thumb-2048.jpg");
            break;
          case THUMBNAIL:
          default:
            k = key + ".THUMBNAIL";
            u = new URL("https://d1cuyjsrcm0gby.cloudfront.net/" + key + "/thumb-320.jpg");
            break;
        }
      } catch (MalformedURLException e) {
        // TODO: Throw exception, so that a MapillaryCache with malformed URL can't be instantiated.
        Main.error(e);
      }
    }
    this.key = k;
    this.url = u;
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
