// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.cache;

import java.net.URL;
import java.util.HashMap;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.stream.Stream;

import org.apache.commons.jcs3.access.CacheAccess;
import org.openstreetmap.josm.data.cache.BufferedImageCacheEntry;
import org.openstreetmap.josm.data.cache.JCSCachedTileLoaderJob;
import org.openstreetmap.josm.data.imagery.TMSCachedTileLoader;
import org.openstreetmap.josm.data.imagery.TileJobOptions;
import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillarySequenceUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL.Cloudfront;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;

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

    /**
     * Get the anticipated width for the image
     *
     * @return The width of the image (pixels)
     */
    public int getWidth() {
      return width;
    }

    /**
     * Get the anticipated height for the image
     *
     * @return The height of the image (pixels)
     */
    public int getHeight() {
      return width;
    }
  }

  /**
   * Cache images. The caching function is run in a separate thread.
   *
   * @param currentImage The image to cache around
   */
  public static void cacheSurroundingImages(INode currentImage) {
    MapillaryUtils.getForkJoinPool(MapillaryCache.class).execute(() -> runnableCacheSurroundingImages(currentImage));
  }

  private static void runnableCacheSurroundingImages(INode currentImage) {
    final ForkJoinPool pool = MapillaryUtils.getForkJoinPool(MapillaryCache.class);
    final int prefetchCount = MapillaryProperties.PRE_FETCH_IMAGE_COUNT.get();
    final long freeMemory = Runtime.getRuntime().freeMemory();
    final CacheAccess<String, BufferedImageCacheEntry> imageCache = Caches.ImageCache.getCache(Type.THUMBNAIL);
    // 3 bytes for RGB (jpg doesn't support the Alpha channel). I'm using 4 bytes instead of 3 for a buffer.
    long estimatedImageSize = Stream.of(MapillaryCache.Type.values())
      .mapToLong(v -> (long) v.getHeight() * v.getWidth() * 4).sum();

    INode nextImage = MapillarySequenceUtils.getNextOrPrevious(currentImage,
      MapillarySequenceUtils.NextOrPrevious.NEXT);
    INode prevImage = MapillarySequenceUtils.getNextOrPrevious(currentImage,
      MapillarySequenceUtils.NextOrPrevious.PREVIOUS);
    for (int i = 0; i < prefetchCount; i++) {
      if (freeMemory - estimatedImageSize < 0) {
        break; // It doesn't make sense to try to cache images that won't be kept.
      }
      if (nextImage != null) {
        if (nextImage.hasKey(MapillaryImageUtils.KEY)
          && (imageCache.get(MapillaryImageUtils.getKey(nextImage)) == null)) {
          INode current = nextImage;
          pool.execute(() -> CacheUtils.downloadPicture(current));
        }
        nextImage = MapillarySequenceUtils.getNextOrPrevious(nextImage, MapillarySequenceUtils.NextOrPrevious.NEXT);
      }
      if (prevImage != null) {
        if (prevImage.hasKey(MapillaryImageUtils.KEY)
          && (imageCache.get(MapillaryImageUtils.getKey(prevImage)) == null)) {
          INode current = prevImage;
          pool.execute(() -> CacheUtils.downloadPicture(current));
        }
        prevImage = MapillarySequenceUtils.getNextOrPrevious(prevImage, MapillarySequenceUtils.NextOrPrevious.PREVIOUS);
      }
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
    super(Caches.ImageCache.getCache(type),
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
