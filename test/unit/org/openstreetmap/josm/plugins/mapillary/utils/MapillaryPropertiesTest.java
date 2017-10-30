// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import org.junit.Rule;
import org.junit.Test;

import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil.MapillaryTestRules;
import org.openstreetmap.josm.testutils.JOSMTestRules;

public class MapillaryPropertiesTest {

  @Rule
  public JOSMTestRules rules = new MapillaryTestRules();

  @Test
  public void test() {
    TestUtil.testUtilityClass(MapillaryProperties.class);
  }

}
