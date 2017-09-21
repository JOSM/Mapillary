// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.cache;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import org.junit.Rule;
import org.junit.Test;

import org.openstreetmap.josm.plugins.mapillary.cache.MapillaryCache.Type;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil.MapillaryTestRules;
import org.openstreetmap.josm.testutils.JOSMTestRules;

public class MapillaryCacheTest {

  @Rule
  public JOSMTestRules rules = new MapillaryTestRules().preferences();

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
