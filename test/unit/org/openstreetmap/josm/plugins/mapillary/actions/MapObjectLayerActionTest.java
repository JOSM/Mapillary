// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.actions;

import static org.junit.Assert.assertEquals;

import org.junit.Before;
import org.junit.Test;

import org.openstreetmap.josm.gui.MainApplication;

public class MapObjectLayerActionTest {

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
