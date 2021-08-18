// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;

import javax.swing.JComponent;

import org.junit.jupiter.api.Test;

class MapillaryColorSchemeTest {

    @Test
    void testUtilityClass() {
        TestUtil.testUtilityClass(MapillaryColorScheme.class);
    }

    @Test
    void testStyleAsDefaultPanel() {
        assertDoesNotThrow(() -> {
            MapillaryColorScheme.styleAsDefaultPanel();
            MapillaryColorScheme.styleAsDefaultPanel((JComponent[]) null);
        });
    }
}
