// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.cache;

import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil;
import org.openstreetmap.josm.testutils.JOSMTestRules;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.RegisterExtension;

class CachesTest {
  // Preferences are needed to get the cache directories
  @RegisterExtension
  JOSMTestRules rule = new JOSMTestRules().preferences();

  @Test
  void testUtilityClass() {
    TestUtil.testUtilityClass(Caches.class);
  }
}
