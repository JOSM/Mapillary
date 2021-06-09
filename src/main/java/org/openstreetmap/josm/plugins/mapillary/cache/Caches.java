// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.cache;

import java.io.File;

import javax.swing.ImageIcon;

import org.apache.commons.jcs3.access.CacheAccess;
import org.apache.commons.jcs3.engine.behavior.IElementAttributes;

import org.openstreetmap.josm.data.cache.BufferedImageCacheEntry;
import org.openstreetmap.josm.data.cache.JCSCacheManager;
import org.openstreetmap.josm.plugins.mapillary.model.UserProfile;
import org.openstreetmap.josm.spi.preferences.Config;

public final class Caches {

  private Caches() {
    // Private constructor to avoid instantiation
  }

  public static File getCacheDirectory() {
    final File f = new File(Config.getDirs().getCacheDirectory(true) + "/Mapillary");
    if (!f.exists()) {
      f.mkdirs();
    }
    return f;
  }

  /**
   * Caches for images
   */
  public static class ImageCache {
    private static final int MAX_DISK_IMAGES_SIZE = 100_000; // kb, ~500 full size images (average ~200 kb/image)
    private static final byte MAX_MEMORY_OBJECTS = 4;
    private static final CacheAccess<String, BufferedImageCacheEntry> THUMBNAIL_IMAGE_CACHE = JCSCacheManager.getCache(
      "mapillary:image:thumbnailImage", MAX_MEMORY_OBJECTS * 10, MAX_DISK_IMAGES_SIZE, getCacheDirectory().getPath());
    private static final CacheAccess<String, BufferedImageCacheEntry> FULL_IMAGE_CACHE = JCSCacheManager
      .getCache("mapillary:image:fullImage", MAX_MEMORY_OBJECTS, MAX_DISK_IMAGES_SIZE, getCacheDirectory().getPath());

    /**
     * Get the cache for the image type
     *
     * @param type The image type
     * @return The cache
     */
    public static CacheAccess<String, BufferedImageCacheEntry> getCache(MapillaryCache.Type type) {
      if (MapillaryCache.Type.THUMBNAIL.equals(type)) {
        return THUMBNAIL_IMAGE_CACHE;
      }
      return FULL_IMAGE_CACHE;
    }

    private ImageCache() {
      // No-op
    }
  }

  /**
   * The cache for map object icons
   */
  public static final CacheAccess<String, ImageIcon> mapObjectIconCache = JCSCacheManager
    .getCache("mapillary:objectIcons", 100, 1000, getCacheDirectory().getPath());
  /** The cache for user profiles */
  public static final CacheAccess<String, UserProfile> userProfileCache = JCSCacheManager
    .getCache("mapillary:userProfile", 100, 1000, getCacheDirectory().getPath());
  /** The cache for metadata objects */
  public static final CacheAccess<String, String> metaDataCache = JCSCacheManager.getCache("mapillary:metadata", 100,
    100000, getCacheDirectory().getPath());
  static {
    final IElementAttributes userProfileCacheAttributes = userProfileCache.getDefaultElementAttributes();
    userProfileCacheAttributes.setMaxLife(604_800_000);
    userProfileCache.setDefaultElementAttributes(userProfileCacheAttributes);
    metaDataCache.setDefaultElementAttributes(userProfileCacheAttributes);
  }
}
