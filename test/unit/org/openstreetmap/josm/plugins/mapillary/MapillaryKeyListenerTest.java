package org.openstreetmap.josm.plugins.mapillary;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.lang.reflect.Field;

import javax.swing.JPopupMenu;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.RegisterExtension;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.data.osm.DataSet;
import org.openstreetmap.josm.data.osm.Node;
import org.openstreetmap.josm.data.vector.VectorNode;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.dialogs.properties.PropertiesDialog;
import org.openstreetmap.josm.gui.layer.OsmDataLayer;
import org.openstreetmap.josm.gui.util.GuiHelper;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.PointObjectLayer;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.MapillaryLayerAnnotation;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.MapillaryURLWireMock;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.MapillaryURLWireMockErrors;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryKeys;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryMapFeatureUtils;
import org.openstreetmap.josm.testutils.JOSMTestRules;
import org.openstreetmap.josm.testutils.annotations.BasicPreferences;
import org.openstreetmap.josm.testutils.annotations.BasicWiremock;

/**
 * Test class for {@link MapillaryKeyListener}
 *
 * @author Taylor Smock
 */
@BasicPreferences
@BasicWiremock // TODO remove when it is inheritable (see #21139)
@MapillaryURLWireMock
@MapillaryLayerAnnotation
class MapillaryKeyListenerTest {
    /** This is the JOSM standard component count. Update as needed. */
    private static final int TAG_MENU_COMPONENT_COUNT = 15;
    @RegisterExtension
    static JOSMTestRules josmTestRules = new JOSMTestRules().main().projection();

    @Test
    void testListenerNoSpuriousActions() throws ReflectiveOperationException {
        final DataSet osmData = new DataSet();
        MainApplication.getLayerManager().addLayer(new OsmDataLayer(osmData, "testListenerNoSpuriousActions", null));

        final PropertiesDialog properties = MainApplication.getMap().propertiesDialog;
        final Field tagMenuField = PropertiesDialog.class.getDeclaredField("tagMenu");
        tagMenuField.setAccessible(true);
        final JPopupMenu tagMenu = (JPopupMenu) tagMenuField.get(properties);

        new MapillaryKeyListener(properties, tagMenu);

        assertEquals(TAG_MENU_COMPONENT_COUNT, tagMenu.getComponentCount());
        GuiHelper.runInEDTAndWait(() -> tagMenu.setVisible(true));
        assertEquals(TAG_MENU_COMPONENT_COUNT, tagMenu.getComponentCount());
        GuiHelper.runInEDTAndWait(() -> tagMenu.setVisible(false));
        assertEquals(TAG_MENU_COMPONENT_COUNT, tagMenu.getComponentCount());

        Node node1 = new Node(LatLon.ZERO);
        osmData.addPrimitive(node1);
        osmData.setSelected(node1);
        GuiHelper.runInEDTAndWait(() -> tagMenu.setVisible(true));
        assertEquals(TAG_MENU_COMPONENT_COUNT, tagMenu.getComponentCount());
        GuiHelper.runInEDTAndWait(() -> tagMenu.setVisible(false));
        assertEquals(TAG_MENU_COMPONENT_COUNT, tagMenu.getComponentCount());

        osmData.clearSelection();
        assertEquals(TAG_MENU_COMPONENT_COUNT, tagMenu.getComponentCount());
        GuiHelper.runInEDTAndWait(() -> tagMenu.setVisible(true));
        assertEquals(TAG_MENU_COMPONENT_COUNT, tagMenu.getComponentCount());
        GuiHelper.runInEDTAndWait(() -> tagMenu.setVisible(false));
        assertEquals(TAG_MENU_COMPONENT_COUNT, tagMenu.getComponentCount());
    }

    // We don't care about the requests, so just give back errors
    @MapillaryURLWireMockErrors(MapillaryURLWireMockErrors.Type.BAD_REQUEST)
    @ParameterizedTest
    @ValueSource(strings = { "mapillary", "mapillary:image", "mapillary:map_feature" })
    void testWithMapillaryKey(final String key) throws ReflectiveOperationException {
        final DataSet osmData = new DataSet();
        MainApplication.getLayerManager().addLayer(new OsmDataLayer(osmData, "testListenerNoSpuriousActions", null));

        // We need to have a Mapillary layer. We don't necessarily need the image, but this avoids a potential network
        // call.
        // (mapillary, mapillary:image)
        final VectorNode image = new VectorNode("test");
        image.setCoor(LatLon.ZERO);
        image.put(MapillaryImageUtils.ImageProperties.ID.toString(), "135511895288847");
        MapillaryLayer.getInstance().getData().addPrimitive(image);

        // For this test, we do want a point object layer (mapillary:map_feature)
        final VectorNode detection = new VectorNode("test");
        detection.setCoor(LatLon.ZERO);
        detection.put(MapillaryMapFeatureUtils.MapFeatureProperties.ID.toString(), "138571594982877");
        final PointObjectLayer pointObjectLayer = new PointObjectLayer(MapillaryKeys.MAPILLARY_POINT_OBJECTS);
        pointObjectLayer.getData().addPrimitive(detection);
        MainApplication.getLayerManager().addLayer(pointObjectLayer);

        final PropertiesDialog properties = MainApplication.getMap().propertiesDialog;
        final Field tagMenuField = PropertiesDialog.class.getDeclaredField("tagMenu");
        tagMenuField.setAccessible(true);
        final JPopupMenu tagMenu = (JPopupMenu) tagMenuField.get(properties);

        new MapillaryKeyListener(properties, tagMenu);

        Node node1 = new Node(LatLon.ZERO);
        if (!key.contains("map_feature")) {
            node1.put(key, "135511895288847");
        } else {
            node1.put(key, "138571594982877");
        }
        osmData.addPrimitive(node1);
        osmData.setSelected(node1);
        GuiHelper.runInEDTAndWait(() -> tagMenu.setVisible(true));
        assertEquals(TAG_MENU_COMPONENT_COUNT + 1, tagMenu.getComponentCount());
        GuiHelper.runInEDTAndWait(() -> tagMenu.setVisible(false));
        assertEquals(TAG_MENU_COMPONENT_COUNT, tagMenu.getComponentCount());

        osmData.clearSelection();
        GuiHelper.runInEDTAndWait(() -> tagMenu.setVisible(true));
        assertEquals(TAG_MENU_COMPONENT_COUNT, tagMenu.getComponentCount());
        GuiHelper.runInEDTAndWait(() -> tagMenu.setVisible(false));
        assertEquals(TAG_MENU_COMPONENT_COUNT, tagMenu.getComponentCount());
    }
}
