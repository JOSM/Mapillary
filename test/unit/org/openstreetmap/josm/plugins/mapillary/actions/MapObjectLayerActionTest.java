// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.actions;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.openstreetmap.josm.data.osm.DataSet;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.layer.OsmDataLayer;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.PointObjectLayer;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.MapillaryURLWireMock;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.MapillaryURLWireMockErrors;
import org.openstreetmap.josm.testutils.annotations.BasicPreferences;
import org.openstreetmap.josm.testutils.annotations.Main;
import org.openstreetmap.josm.testutils.annotations.Projection;

@BasicPreferences
@Main
@MapillaryURLWireMock
@Projection
class MapObjectLayerActionTest {
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
