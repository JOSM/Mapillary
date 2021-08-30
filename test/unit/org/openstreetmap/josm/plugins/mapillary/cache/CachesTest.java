// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.cache;

import org.junit.jupiter.api.Test;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil;
import org.openstreetmap.josm.testutils.annotations.BasicPreferences;

/**
 * Test class for {@link Caches}
 */
// Preferences are needed to get the cache directories
@BasicPreferences
class CachesTest {
    @Test
    void testUtilityClass() {
        TestUtil.testUtilityClass(Caches.class);
    }
}
