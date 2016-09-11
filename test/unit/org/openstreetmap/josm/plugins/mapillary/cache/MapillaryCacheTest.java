// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.cache;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;

import java.io.File;
import java.io.IOException;

import org.apache.commons.jcs.access.CacheAccess;
import org.junit.Before;
import org.junit.Test;
import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.data.cache.BufferedImageCacheEntry;
import org.openstreetmap.josm.data.cache.JCSCacheManager;
import org.openstreetmap.josm.plugins.mapillary.AbstractTest;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.cache.MapillaryCache.Type;

public class MapillaryCacheTest extends AbstractTest {
  
  @Test
  public void test() throws IOException {
    CacheAccess<String, BufferedImageCacheEntry> cacheAccess = JCSCacheManager.getCache("mapillary", 10, 10000,
      new File(Main.pref.getPluginsDirectory(), "mapillary").getPath() + "/cache/");

    MapillaryCache cache = new MapillaryCache(cacheAccess, "00000", Type.FULL_IMAGE);
    assertNotEquals(null, cache.getUrl());
    assertNotEquals(null, cache.getCacheKey());

    assertFalse(cache.isObjectLoadable());

    cache = new MapillaryCache(cacheAccess, "00000", Type.THUMBNAIL);
    assertNotEquals(null, cache.getCacheKey());
    assertNotEquals(null, cache.getUrl());

    cache = new MapillaryCache(cacheAccess, null, null);
    assertEquals(null, cache.getCacheKey());
    assertEquals(null, cache.getUrl());
  }
}
