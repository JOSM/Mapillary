// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.actions;

import static org.junit.Assert.assertEquals;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;

import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil.MapillaryTestRules;
import org.openstreetmap.josm.testutils.JOSMTestRules;

public class MapObjectLayerActionTest {

  @Rule
  public JOSMTestRules rules = new MapillaryTestRules();

  @Before
  public void resetLayers() {
    MainApplication.getLayerManager().getLayers().parallelStream().forEach(l -> MainApplication.getLayerManager().removeLayer(l));
  }

  @Test
  public void testAction() {
    assertEquals(0, MainApplication.getLayerManager().getLayers().size());
    new MapObjectLayerAction().actionPerformed(null);
    assertEquals(1, MainApplication.getLayerManager().getLayers().size());
    new MapObjectLayerAction().actionPerformed(null);
    assertEquals(1, MainApplication.getLayerManager().getLayers().size());
  }
}
