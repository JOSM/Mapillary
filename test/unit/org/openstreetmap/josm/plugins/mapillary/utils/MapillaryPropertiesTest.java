// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.RegisterExtension;

import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil.MapillaryTestRules;
import org.openstreetmap.josm.testutils.JOSMTestRules;

class MapillaryPropertiesTest {

  @RegisterExtension
  JOSMTestRules rules = new MapillaryTestRules();

  @Test
  void test() {
    TestUtil.testUtilityClass(MapillaryProperties.class);
  }

}
