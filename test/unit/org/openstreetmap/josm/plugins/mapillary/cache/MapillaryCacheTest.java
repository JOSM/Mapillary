// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.cache;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.RegisterExtension;

import org.openstreetmap.josm.TestUtils;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.data.vector.VectorDataSet;
import org.openstreetmap.josm.data.vector.VectorNode;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.MapillaryURLWireMock;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryTestRules;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL;
import org.openstreetmap.josm.testutils.JOSMTestRules;

@MapillaryURLWireMock
class MapillaryCacheTest {

  @RegisterExtension
  static JOSMTestRules rules = new MapillaryTestRules().preferences().main();

  @Test
  void test() {
    VectorNode image = new VectorNode("mapillary-images");
    image.setCoor(new LatLon(39.068354972222, -108.57081597222));
    VectorDataSet vectorDataSet = new VectorDataSet();
    vectorDataSet.addPrimitive(image);
    // Use 135511895288847 since that is an image we have real information for
    image.put(MapillaryURL.APIv4.ImageProperties.ID.toString(), "135511895288847");
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
