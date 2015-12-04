package org.openstreetmap.josm.plugins.mapillary.cache;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;

import org.junit.Test;
import org.openstreetmap.josm.plugins.mapillary.AbstractTest;
import org.openstreetmap.josm.plugins.mapillary.cache.MapillaryCache.Type;

public class MapillaryCacheTest extends AbstractTest {

  @Test
  public void test() {
    MapillaryCache cache = new MapillaryCache("00000", Type.FULL_IMAGE);
    assertNotEquals(null, cache.getUrl());
    assertNotEquals(null, cache.getCacheKey());

    assertFalse(cache.isObjectLoadable());

    cache = new MapillaryCache("00000", Type.THUMBNAIL);
    assertNotEquals(null, cache.getCacheKey());
    assertNotEquals(null, cache.getUrl());

    cache = new MapillaryCache(null, null);
    assertEquals(null, cache.getCacheKey());
    assertEquals(null, cache.getUrl());
  }

}
