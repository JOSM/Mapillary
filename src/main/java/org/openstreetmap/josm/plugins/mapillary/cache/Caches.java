// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.cache;

import static org.openstreetmap.josm.tools.I18n.marktr;
import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.Future;
import java.util.function.Predicate;
import java.util.function.Supplier;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import javax.json.Json;
import javax.json.JsonObject;
import javax.json.JsonReader;
import javax.json.JsonValue;
import javax.swing.JOptionPane;

import org.openstreetmap.josm.data.cache.BufferedImageCacheEntry;
import org.openstreetmap.josm.data.cache.JCSCacheManager;
import org.openstreetmap.josm.gui.Notification;
import org.openstreetmap.josm.gui.util.GuiHelper;
import org.openstreetmap.josm.plugins.mapillary.model.UserProfile;
import org.openstreetmap.josm.spi.preferences.Config;
import org.openstreetmap.josm.tools.JosmRuntimeException;
import org.openstreetmap.josm.tools.Logging;

import org.apache.commons.jcs3.access.CacheAccess;
import org.apache.commons.jcs3.access.behavior.ICacheAccess;
import org.apache.commons.jcs3.engine.behavior.IElementAttributes;

public final class Caches {
  private static final String ERROR = "error";
  /** The cache for user profiles */
  public static final MapillaryCacheAccess<UserProfile> USER_PROFILE_CACHE = new MapillaryCacheAccess<>(
    JCSCacheManager.getCache("mapillary:userProfile", 100, 1000, getCacheDirectory().getPath()));

  /** The cache for non-street level images, AKA metadata images for users/organizations/etc. */
  public static final MapillaryCacheAccess<BufferedImage> META_IMAGES = new MapillaryCacheAccess<>(
    JCSCacheManager.getCache("mapillary:metaImages", 5, 1000, getCacheDirectory().getPath()));

  /** The cache for metadata objects */
  public static final MapillaryCacheAccess<String> META_DATA_CACHE = new MapillaryCacheAccess<>(
    JCSCacheManager.getCache("mapillary:metadata", 100, 100000, getCacheDirectory().getPath()), string -> {
      try (JsonReader reader = Json.createReader(new ByteArrayInputStream(string.getBytes(StandardCharsets.UTF_8)))) {
        JsonObject object = reader.readObject();
        if (object.containsKey(ERROR) || object.containsKey("call_volume") || object.containsKey("call_count")) {
          Logging.error(object.toString());
          return false;
        }
        return true;
      }
    });

  private static final int MAX_DISK_IMAGES_SIZE = 100_000; // kb, ~500 full size images (average ~200 kb/image)
  private static final byte MAX_MEMORY_OBJECTS = 4;

  /** The cache for full size images */
  public static final MapillaryCacheAccess<BufferedImageCacheEntry> FULL_IMAGE_CACHE = new MapillaryCacheAccess<>(
    JCSCacheManager.getCache("mapillary:image:fullImage", MAX_MEMORY_OBJECTS, MAX_DISK_IMAGES_SIZE,
      getCacheDirectory().getPath()));
  static {
    final IElementAttributes userProfileCacheAttributes = USER_PROFILE_CACHE.getDefaultElementAttributes();
    userProfileCacheAttributes.setMaxLife(604_800_000);
    USER_PROFILE_CACHE.setDefaultElementAttributes(userProfileCacheAttributes);
    META_DATA_CACHE.setDefaultElementAttributes(userProfileCacheAttributes);
  }

  public static File getCacheDirectory() {
    final File f = new File(Config.getDirs().getCacheDirectory(true) + "/Mapillary");
    if (!f.exists() && !f.mkdirs()) {
      throw new JosmRuntimeException(
        new IOException("Mapillary: Could not create cache directory: " + f.getAbsolutePath()));
    }
    return f;
  }

  private Caches() {
    // Private constructor to avoid instantiation
  }

  /**
   * A wrapper to avoid saving bad returns
   *
   * @param <V> The value type
   */
  public static class MapillaryCacheAccess<V> {
    private static final String UNKNOWN_MAPILLARY_EXCEPTION = marktr("Unknown Mapillary exception.\n{0}");
    @Nonnull
    private final CacheAccess<String, V> cacheAccess;
    @Nonnull
    private final List<Predicate<V>> validators;
    @Nullable
    private Supplier<V> defaultSupplier;

    private boolean rateLimited;

    @SafeVarargs
    public MapillaryCacheAccess(@Nonnull CacheAccess<String, V> cacheAccess, @Nullable Predicate<V>... validators) {
      this.cacheAccess = cacheAccess;
      if (validators != null) {
        this.validators = Arrays.asList(validators);
      } else {
        this.validators = Collections.emptyList();
      }
    }

    /**
     * Set the default supplier for a URL
     *
     * @param defaultSupplier The default supplier
     */
    public void setDefaultSupplier(@Nullable Supplier<V> defaultSupplier) {
      this.defaultSupplier = defaultSupplier;
    }

    /**
     * @see CacheAccess#getDefaultElementAttributes()
     * @return The default element attributes
     */
    public IElementAttributes getDefaultElementAttributes() {
      return this.cacheAccess.getDefaultElementAttributes();
    }

    /**
     * @see CacheAccess#setDefaultElementAttributes(IElementAttributes)
     * @param iElementAttributes The default element attributes to set
     */
    public void setDefaultElementAttributes(IElementAttributes iElementAttributes) {
      this.cacheAccess.setDefaultElementAttributes(iElementAttributes);
    }

    /**
     * Given a URL, get the information (if a default supplier is set, this fetches the information)
     *
     * @param url The URL to get
     * @return The information
     */
    @Nullable
    public V get(@Nonnull String url) {
      if (this.defaultSupplier != null) {
        return this.get(url, this.defaultSupplier);
      } else {
        return this.get(url, () -> null);
      }
    }

    /**
     * Given a URL, get the information (if a default supplier is set, this fetches the information).
     * This is non-blocking.
     *
     * @param url The URL to get
     * @return The information
     */
    @Nullable
    public Future<V> get(@Nonnull String url, @Nonnull ForkJoinPool pool) {
      if (this.defaultSupplier != null) {
        return this.get(url, pool, this.defaultSupplier);
      } else {
        return this.get(url, pool, () -> null);
      }
    }

    /**
     * Given a URL, get and cache the response if not already present.
     *
     * @param url The URL to get
     * @param supplier The method to get the response
     * @return The type the supplier returns
     */
    @Nullable
    public V get(@Nonnull String url, @Nonnull Supplier<V> supplier) {
      if (this.cacheAccess.get(url) != null) {
        return this.cacheAccess.get(url);
      }
      if (rateLimited) {
        return null;
      }
      final V returnObject;
      synchronized (this) {
        returnObject = this.cacheAccess.get(url) == null ? supplier.get() : this.cacheAccess.get(url);
        if (returnObject != null && this.validators.stream().allMatch(p -> p.test(returnObject))) {
          this.cacheAccess.put(url, returnObject);
        } else if (returnObject != null) {
          final String message = checkReturnObject(returnObject);
          GuiHelper.runInEDT(() -> {
            Notification notification = new Notification();
            notification.setContent(tr(message, returnObject));
            notification.setDuration(Notification.TIME_LONG);
            notification.setIcon(JOptionPane.ERROR_MESSAGE);
            notification.show();
          });
        }
      }
      return returnObject;
    }

    /**
     * Check an object for issues
     *
     * @param returnObject The object to check
     * @return The error message
     */
    private String checkReturnObject(V returnObject) {
      if (returnObject instanceof String) {
        try (JsonReader reader = Json
          .createReader(new ByteArrayInputStream(((String) returnObject).getBytes(StandardCharsets.UTF_8)))) {
          final JsonValue jsonValue = reader.readValue();
          if (jsonValue.getValueType() == JsonValue.ValueType.OBJECT && jsonValue.asJsonObject().containsKey(ERROR)
            && jsonValue.asJsonObject().get(ERROR).getValueType() == JsonValue.ValueType.OBJECT
            && jsonValue.asJsonObject().getJsonObject(ERROR).containsKey("message")
            && "Application request limit reached"
              .equals(jsonValue.asJsonObject().getJsonObject(ERROR).getString("message"))) {
            this.rateLimited = true;
            return marktr(
              "We have reached the Mapillary API limit. Disabling Mapillary networking until JOSM restart. Sorry.\n"
                + "Logging in after the rate limit subsides may help prevent this in the future.\n{0}");
          }
        }
      }
      return UNKNOWN_MAPILLARY_EXCEPTION;
    }

    /**
     * Given a URL, get and cache the response if not already present in a non-blocking manner
     *
     * @param url The url to get
     * @param pool The ForkJoinPool to use
     * @param supplier The supplier to get the object with
     * @return A future with the object, when it completes.
     */
    public Future<V> get(@Nonnull String url, @Nonnull ForkJoinPool pool, @Nonnull Supplier<V> supplier) {
      if (this.cacheAccess.get(url) != null) {
        return CompletableFuture.completedFuture(this.cacheAccess.get(url));
      }
      return pool.submit(() -> get(url, supplier));
    }

    /**
     * Get the underlying cache
     *
     * @return The actual cache object
     */
    public ICacheAccess<String, V> getICacheAccess() {
      return this.cacheAccess;
    }
  }
}
