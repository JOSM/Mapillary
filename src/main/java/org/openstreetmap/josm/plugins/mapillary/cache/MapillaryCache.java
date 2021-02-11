// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.cache;

import java.net.URL;
import java.util.HashMap;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import org.openstreetmap.josm.data.cache.BufferedImageCacheEntry;
import org.openstreetmap.josm.data.cache.JCSCachedTileLoaderJob;
import org.openstreetmap.josm.data.imagery.TMSCachedTileLoader;
import org.openstreetmap.josm.data.imagery.TileJobOptions;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL.Cloudfront;

/**
 * Stores the downloaded pictures locally.
 *
 * @author nokutu
 */
public class MapillaryCache extends JCSCachedTileLoaderJob<String, BufferedImageCacheEntry> {

  private final URL url;
  private final String key;

  private static final ThreadPoolExecutor DEFAULT_JOB_EXECUTOR = TMSCachedTileLoader
    .getNewThreadPoolExecutor("Mapillary-image-downloader-%d", THREAD_LIMIT.get(), THREAD_LIMIT.get());

  /**
   * Types of images.
   *
   * @author nokutu
   */
  public enum Type {
    /** Full quality image */
    FULL_IMAGE(2048),
    /** Low quality image */
    THUMBNAIL(320);

    private final int width;

    Type(int dimension) {
      this.width = dimension;
    }

    /** Get the anticipated width for the image */
    public int getWidth() {
      return width;
    }

    /** Get the anticipated height for the image */
    public int getHeight() {
      return width;
    }
  }

  /**
   * Main constructor.
   *
   * @param key
   *        The key of the image.
   * @param type
   *        The type of image that must be downloaded (THUMBNAIL or
   *        FULL_IMAGE).
   */
  public MapillaryCache(final String key, final Type type) {
    super(Caches.ImageCache.getInstance().getCache(type),
      new TileJobOptions(50_000, 50_000, new HashMap<>(), TimeUnit.HOURS.toSeconds(4)), DEFAULT_JOB_EXECUTOR);
    if (key == null || type == null) {
      this.key = null;
      this.url = null;
    } else {
      this.key = key + (type == Type.FULL_IMAGE ? ".FULL_IMAGE" : ".THUMBNAIL");
      this.url = Cloudfront.thumbnail(key, type == Type.FULL_IMAGE);
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
    if (this.cacheData == null) {
      return false;
    }
    byte[] content = this.cacheData.getContent();
    return content != null && content.length > 0;
  }
}
