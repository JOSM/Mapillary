// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.cache;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import org.junit.Test;

import org.openstreetmap.josm.plugins.mapillary.AbstractTest;
import org.openstreetmap.josm.plugins.mapillary.cache.MapillaryCache.Type;

public class MapillaryCacheTest extends AbstractTest {

  @Test
  public void test() {
    MapillaryCache cache = new MapillaryCache("00000", Type.FULL_IMAGE);
    assertNotNull(cache.getUrl());
    assertNotNull(cache.getCacheKey());

    assertFalse(cache.isObjectLoadable());

    cache = new MapillaryCache("00000", Type.THUMBNAIL);
    assertNotNull(cache.getCacheKey());
    assertNotNull(cache.getUrl());

    cache = new MapillaryCache(null, null);
    assertNull(cache.getCacheKey());
    assertNull(cache.getUrl());
  }
}
