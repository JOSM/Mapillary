// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.actions;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

import org.openstreetmap.josm.Main;

public class MapObjectLayerActionTest {
  @Test
  public void testAction() {
    Main.getLayerManager().resetState();
    assertEquals(0, Main.getLayerManager().getLayers().size());
    new MapObjectLayerAction().actionPerformed(null);
    assertEquals(1, Main.getLayerManager().getLayers().size());
    new MapObjectLayerAction().actionPerformed(null);
    assertEquals(1, Main.getLayerManager().getLayers().size());
  }
}
