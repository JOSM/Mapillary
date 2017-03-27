// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import javax.swing.JComponent;

import org.junit.Test;

public class MapillaryColorSchemeTest {

  @Test
  public void testUtilityClass() {
    TestUtil.testUtilityClass(MapillaryColorScheme.class);
  }

  @Test
  public void testStyleAsDefaultPanel() {
    MapillaryColorScheme.styleAsDefaultPanel();
    MapillaryColorScheme.styleAsDefaultPanel((JComponent[]) null);
  }
}
