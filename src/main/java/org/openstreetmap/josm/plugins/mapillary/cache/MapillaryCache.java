// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.cache;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import org.openstreetmap.josm.data.cache.BufferedImageCacheEntry;
import org.openstreetmap.josm.data.cache.JCSCachedTileLoaderJob;
import org.openstreetmap.josm.data.imagery.TMSCachedTileLoader;
import org.openstreetmap.josm.data.imagery.TileJobOptions;
import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.data.vector.VectorNode;
import org.openstreetmap.josm.plugins.mapillary.io.download.MapillaryDownloader;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillarySequenceUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;
import org.openstreetmap.josm.tools.JosmRuntimeException;
import org.openstreetmap.josm.tools.Logging;

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
    FULL_IMAGE(MapillaryImageUtils.ImageProperties.BEST_IMAGE),
    /** Low quality image */
    THUMBNAIL(MapillaryImageUtils.ImageProperties.WORST_IMAGE);

    private final int width;
    private final String imageUrl;

    Type(final MapillaryImageUtils.ImageProperties properties) {
      this.imageUrl = properties.name().toLowerCase(Locale.ROOT);
      final Pattern pattern = MapillaryImageUtils.BASE_IMAGE_KEY;
      final Matcher matcher = pattern.matcher(this.imageUrl);
      if (matcher.matches()) {
        this.width = Integer.parseInt(matcher.group(1));
      } else {
        throw new IllegalArgumentException("Mapillary: " + this.imageUrl + " is not a valid image type");
      }
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

    /**
     * Get the key for this image type
     *
     * @return The key to look for in the image node
     */
    public String getKey() {
      return this.imageUrl;
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
        if (MapillaryImageUtils.getKey(nextImage) != null) {
          INode current = nextImage;
          pool.execute(() -> CacheUtils.downloadPicture(current, CacheUtils.PICTURE.THUMBNAIL));
        }
        nextImage = MapillarySequenceUtils.getNextOrPrevious(nextImage, MapillarySequenceUtils.NextOrPrevious.NEXT);
      }
      if (prevImage != null) {
        if (MapillaryImageUtils.getKey(prevImage) != null) {
          INode current = prevImage;
          pool.execute(() -> CacheUtils.downloadPicture(current, CacheUtils.PICTURE.THUMBNAIL));
        }
        prevImage = MapillarySequenceUtils.getNextOrPrevious(prevImage, MapillarySequenceUtils.NextOrPrevious.PREVIOUS);
      }
    }
    prefetchImageDetails(2 * prefetchCount, currentImage);
  }

  /**
   * Prefetch image details forward and behind
   *
   * @param prefetchCount The number of images to prefetch ahead and behind
   * @param currentImage The current image
   */
  private static void prefetchImageDetails(final int prefetchCount, final INode currentImage) {
    // Prefetch
    final ForkJoinPool pool = MapillaryUtils.getForkJoinPool();
    INode nextImage = MapillarySequenceUtils.getNextOrPrevious(currentImage,
      MapillarySequenceUtils.NextOrPrevious.NEXT);
    INode prevImage = MapillarySequenceUtils.getNextOrPrevious(currentImage,
      MapillarySequenceUtils.NextOrPrevious.PREVIOUS);
    for (int i = 0; i < prefetchCount; i++) {
      if (nextImage != null) {
        if (MapillaryImageUtils.getKey(nextImage) != null) {
          INode current = nextImage;
          pool.execute(() -> MapillaryDownloader.downloadImages(MapillaryImageUtils.getKey(current)));
        }
        nextImage = MapillarySequenceUtils.getNextOrPrevious(nextImage, MapillarySequenceUtils.NextOrPrevious.NEXT);
      }
      if (prevImage != null) {
        if (MapillaryImageUtils.getKey(prevImage) != null) {
          INode current = prevImage;
          pool.execute(() -> MapillaryDownloader.downloadImages(MapillaryImageUtils.getKey(current)));
        }
        prevImage = MapillarySequenceUtils.getNextOrPrevious(prevImage, MapillarySequenceUtils.NextOrPrevious.PREVIOUS);
      }
    }
  }

  /**
   * Main constructor.
   *
   * @param image
   *        The image.
   */
  public MapillaryCache(final INode image) {
    super(Caches.FULL_IMAGE_CACHE.getICacheAccess(),
      new TileJobOptions(50_000, 50_000, new HashMap<>(), TimeUnit.HOURS.toSeconds(4)), DEFAULT_JOB_EXECUTOR);
    if (image instanceof VectorNode) {
      MapillaryDownloader.downloadImages((VectorNode) image);
    } else {
      MapillaryDownloader.downloadImages(MapillaryImageUtils.getKey(image));
    }
    final Type type = Type.FULL_IMAGE;
    try {
      if (image == null) {
        this.key = null;
        this.url = null;
      } else if (image.hasKey(type.getKey())) {
        this.key = MapillaryImageUtils.getKey(image) + '.' + type.width;
        this.url = new URL(image.get(type.getKey()));
      } else {
        // Iterate through the keys, maybe there is another image url?
        String tKey = null;
        URL tUrl = null;
        for (Map.Entry<String, String> entry : image.getKeys().entrySet()) {
          if (MapillaryImageUtils.BASE_IMAGE_KEY.matcher(entry.getKey()).matches()) {
            String width = MapillaryImageUtils.BASE_IMAGE_KEY.matcher(entry.getKey()).group(1);
            tKey = MapillaryImageUtils.getKey(image) + '.' + width;
            tUrl = new URL(entry.getValue());
            break;
          }
        }
        this.key = tKey;
        this.url = tUrl;
      }
    } catch (MalformedURLException e) {
      Logging.error(e);
      throw new JosmRuntimeException(e);
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
