// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.actions;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.RegisterExtension;
import org.openstreetmap.josm.data.osm.DataSet;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.layer.OsmDataLayer;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.PointObjectLayer;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.MapillaryURLWireMock;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.MapillaryURLWireMockErrors;
import org.openstreetmap.josm.testutils.JOSMTestRules;
import org.openstreetmap.josm.testutils.annotations.BasicPreferences;

@BasicPreferences
@MapillaryURLWireMock
class MapObjectLayerActionTest {

    @RegisterExtension
    static JOSMTestRules rules = new JOSMTestRules().main().projection();

    @Test
    @MapillaryURLWireMockErrors
    void testAction() {
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
