package org.openstreetmap.josm.plugins.mapillary.utils;

import org.openstreetmap.josm.plugins.mapillary.cache.Caches;
import org.openstreetmap.josm.plugins.mapillary.cache.MapillaryCache;

/**
 * This class exists to help initialize some items that fail when a security manager is installed
 *
 * @author Taylor Smock
 */
public class SecurityManagerUtils {
  private SecurityManagerUtils() {
    // Don't allow instantiation
  }

  /**
   * Do initializations that cannot occur in a ForkJoinPool (due to WebStart's security manager)
   */
  public static void doInitializations() {
    if (System.getSecurityManager() != null) {
      // Ensure that we aren't initializing the caches in a secure context (specifically, ForkJoin pools)
      // -- this fails, and throws exceptions. See JOSM #20951.
      // Ensure that the image caches are initialized
      Caches.FULL_IMAGE_CACHE.hashCode();
      // Ensure that the ForkJoin pools are already initialized
      MapillaryUtils.getForkJoinPool();
      MapillaryUtils.getForkJoinPool(MapillaryCache.class);
    }
  }
}
