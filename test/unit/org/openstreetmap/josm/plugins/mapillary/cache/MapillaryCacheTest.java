// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.cache;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.RegisterExtension;

import org.openstreetmap.josm.TestUtils;
import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.data.osm.Node;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryTestRules;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL;
import org.openstreetmap.josm.testutils.JOSMTestRules;

class MapillaryCacheTest {

  @RegisterExtension
  static JOSMTestRules rules = new MapillaryTestRules().preferences();

  @Test
  void test() {
    INode image = new Node();
    image.put(MapillaryURL.APIv4.ImageProperties.ID.toString(), "00000");
    MapillaryCache cache = new MapillaryCache(image);
    assertNotNull(cache.getUrl());
    assertNotNull(cache.getCacheKey());

    assertFalse(cache.isObjectLoadable());

    cache = new MapillaryCache(null);
    assertNull(cache.getCacheKey());
    assertNull(cache.getUrl());
  }

  @Test
  void testMapillaryCacheTypes() {
    TestUtils.superficialEnumCodeCoverage(MapillaryCache.Type.class);
  }
}
