// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.actions;

import static org.junit.Assert.assertEquals;

import org.junit.Rule;
import org.junit.Test;

import org.openstreetmap.josm.data.osm.DataSet;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.layer.OsmDataLayer;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.PointObjectLayer;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil.MapillaryTestRules;
import org.openstreetmap.josm.testutils.JOSMTestRules;

public class MapObjectLayerActionTest {

  @Rule
  public JOSMTestRules rules = new MapillaryTestRules().main().projection();

  @Test
  public void testAction() {
    assertEquals(0, MainApplication.getLayerManager().getLayers().size());
    new MapObjectLayerAction().actionPerformed(null);
    assertEquals(0, MainApplication.getLayerManager().getLayers().size());
    MainApplication.getLayerManager().addLayer(new OsmDataLayer(new DataSet(), "Test", null));
    new MapObjectLayerAction().actionPerformed(null);
    assertEquals(2, MainApplication.getLayerManager().getLayers().size());
    new MapObjectLayerAction().actionPerformed(null);
    assertEquals(2, MainApplication.getLayerManager().getLayers().size());
    MainApplication.getLayerManager()
      .setActiveLayer(MainApplication.getLayerManager().getLayersOfType(PointObjectLayer.class).get(0));
    new MapObjectLayerAction().actionPerformed(null);
    assertEquals(2, MainApplication.getLayerManager().getLayers().size());
  }
}
