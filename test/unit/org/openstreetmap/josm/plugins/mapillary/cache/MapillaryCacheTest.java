// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.cache;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.RegisterExtension;

import org.openstreetmap.josm.plugins.mapillary.cache.MapillaryCache.Type;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil.MapillaryTestRules;
import org.openstreetmap.josm.testutils.JOSMTestRules;

class MapillaryCacheTest {

  @RegisterExtension
  public JOSMTestRules rules = new MapillaryTestRules().preferences();

  @Test
  void test() {
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
