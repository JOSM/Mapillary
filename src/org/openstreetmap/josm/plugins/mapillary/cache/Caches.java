// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.cache;

import java.io.File;
import java.io.Serializable;

import javax.swing.ImageIcon;

import org.apache.commons.jcs.access.CacheAccess;
import org.apache.commons.jcs.engine.behavior.IElementAttributes;

import org.openstreetmap.josm.data.Preferences;
import org.openstreetmap.josm.data.cache.BufferedImageCacheEntry;
import org.openstreetmap.josm.data.cache.JCSCacheManager;
import org.openstreetmap.josm.plugins.mapillary.model.UserProfile;

public final class Caches {

  private Caches() {
    // Private constructor to avoid instantiation
  }

  public static File getCacheDirectory() {
    final File f = new File(Preferences.main().getPluginsDirectory().getPath() + "/Mapillary/cache");
    if (!f.exists()) {
      f.mkdirs();
    }
    return f;
  }

  public abstract static class CacheProxy<K, V extends Serializable> {
    private final CacheAccess<K, V> cache;

    public CacheProxy() {
      cache = createNewCache();
    }

    protected abstract CacheAccess<K, V> createNewCache();

    public V get(final K key) {
      return cache == null ? null : cache.get(key);
    }

    public void put(final K key, final V value) {
      if (cache != null) {
        cache.put(key, value);
      }
    }
  }

  public static class ImageCache {
    private static ImageCache instance;
    private final CacheAccess<String, BufferedImageCacheEntry> cache =
        JCSCacheManager.getCache("mapillary", 10, 10000, getCacheDirectory().getPath());

    public CacheAccess<String, BufferedImageCacheEntry> getCache() {
      return cache;
    }

    public static ImageCache getInstance() {
      synchronized (ImageCache.class) {
        if (instance == null) {
          instance = new ImageCache();
        }
        return instance;
      }
    }
  }

  public static class MapObjectIconCache extends CacheProxy<String, ImageIcon> {
    private static CacheProxy<String, ImageIcon> instance;

    public static CacheProxy<String, ImageIcon> getInstance() {
      synchronized (MapObjectIconCache.class) {
        if (instance == null) {
          instance = new MapObjectIconCache();
        }
        return instance;
      }
    }

    @Override
    protected CacheAccess<String, ImageIcon> createNewCache() {
      return JCSCacheManager.getCache("mapillaryObjectIcons", 100, 1000, getCacheDirectory().getPath());
    }
  }

  public static class UserProfileCache extends CacheProxy<String, UserProfile> {
    private static CacheProxy<String, UserProfile> instance;

    public static CacheProxy<String, UserProfile> getInstance() {
      synchronized (UserProfileCache.class) {
        if (instance == null) {
          instance = new UserProfileCache();
        }
        return instance;
      }
    }

    @Override
    protected CacheAccess<String, UserProfile> createNewCache() {
      CacheAccess<String, UserProfile> cache =
        JCSCacheManager.getCache("userProfile", 100, 1000, getCacheDirectory().getPath());
      IElementAttributes atts = cache.getDefaultElementAttributes();
      atts.setMaxLife(604_800_000); // Sets lifetime to 7 days (604800000=1000*60*60*24*7)
      cache.setDefaultElementAttributes(atts);
      return cache;
    }
  }
}
