// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.layer;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.text.MessageFormat;
import java.util.Arrays;
import java.util.Collections;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.openstreetmap.gui.jmapviewer.interfaces.TileSource;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.data.imagery.ImageryInfo;
import org.openstreetmap.josm.data.imagery.vectortile.mapbox.MVTTile;
import org.openstreetmap.josm.data.osm.DataSet;
import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.data.osm.Node;
import org.openstreetmap.josm.data.vector.VectorNode;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.layer.ImageryLayer;
import org.openstreetmap.josm.gui.layer.Layer;
import org.openstreetmap.josm.gui.layer.OsmDataLayer;
import org.openstreetmap.josm.gui.layer.imagery.MVTLayer;
import org.openstreetmap.josm.plugins.mapillary.gui.dialog.MapillaryFilterDialog;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.MapillaryLayerAnnotation;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.MapillaryURLWireMock;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.MapillaryURLWireMockErrors;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.api.JsonImageDetailsDecoderTest;
import org.openstreetmap.josm.testutils.annotations.BasicPreferences;
import org.openstreetmap.josm.testutils.annotations.Main;
import org.openstreetmap.josm.testutils.annotations.Projection;

@BasicPreferences
@Main
@MapillaryLayerAnnotation
@MapillaryURLWireMock
@MapillaryURLWireMockErrors
@Projection
class MapillaryLayerTest {
    private static Layer getDummyLayer() {
        return ImageryLayer.create(new ImageryInfo("dummy", "https://example.org"));
    }

    @BeforeEach
    void setUp() {
        if (MapillaryLayer.hasInstance()) {
            try {
                MapillaryLayer.getInstance().destroy();
                if (MapillaryFilterDialog.hasInstance()) {
                    MapillaryFilterDialog.getInstance().reset();
                }
            } catch (IllegalArgumentException illegalArgumentException) {
                // Some other test pollutes MapillaryLayer.getInstance, and LayerChangeAdaptor is cleaned up.
                // This causes destroy() to fail on the first test.
                if (!illegalArgumentException.getMessage().contains("Listener was not registered before")) {
                    throw illegalArgumentException;
                }
            }
        }
    }

    @AfterEach
    void tearDown() {
        this.setUp();
    }

    @Test
    void testGetIcon() {
        assertNotNull(MapillaryLayer.getInstance().getIcon());
    }

    @Test
    void testIsMergable() {
        assertFalse(MapillaryLayer.getInstance().isMergable(getDummyLayer()));
    }

    @Test
    void testMergeFrom() {
        Layer dummyLayer = getDummyLayer();
        MapillaryLayer instance = MapillaryLayer.getInstance();
        assertThrows(UnsupportedOperationException.class, () -> instance.mergeFrom(dummyLayer));
    }

    @Test
    void testSetVisible() {
        MapillaryLayer.getInstance().getData()
            .addPrimitive(JsonImageDetailsDecoderTest.createDownloadedImage(1, new LatLon(0.0, 0.0), 0.0, false));
        MapillaryLayer.getInstance().getData()
            .addPrimitive(JsonImageDetailsDecoderTest.createDownloadedImage(2, new LatLon(0.0, 0.0), 0.0, false));
        VectorNode invisibleImage = JsonImageDetailsDecoderTest.createDownloadedImage(3, new LatLon(0.0, 0.0), 0.0,
            false);
        invisibleImage.setVisible(false);
        MapillaryLayer.getInstance().getData().addPrimitive(invisibleImage);

        MapillaryLayer.getInstance().setVisible(false);
        for (INode img : MapillaryLayer.getInstance().getData().getNodes()) {
            if (MapillaryImageUtils.isImage(img)) {
                assertFalse(img.isVisible(),
                    MessageFormat.format("Failed on image {0}", MapillaryImageUtils.getKey(img)));
            }
        }

        MapillaryLayer.getInstance().setVisible(true);
        for (INode img : MapillaryLayer.getInstance().getData().getNodes()) {
            if (MapillaryImageUtils.isImage(img)) {
                assertTrue(img.isVisible());
            }
        }
    }

    @Test
    void testGetInfoComponent() {
        Object comp = MapillaryLayer.getInstance().getInfoComponent();
        assertTrue(comp instanceof String);
        assertTrue(((String) comp).length() >= 9);
    }

    @Test
    void testSetImageViewed() {
        INode image = JsonImageDetailsDecoderTest.createDownloadedImage("1", LatLon.ZERO, 0, false);
        assertFalse(MapillaryLayer.getInstance().setImageViewed(null),
            "An image should not be set as viewed if there is no image or dataset");
        assertFalse(MapillaryLayer.getInstance().setImageViewed(image),
            "An image should not be set as viewed if there is no dataset to edit");
        MainApplication.getLayerManager().addLayer(new OsmDataLayer(new DataSet(), "Test Layer", null));
        assertFalse(MapillaryLayer.getInstance().setImageViewed(null),
            "An image should not be set as viewed if there is no image (i.e., image is null)");
        assertTrue(MapillaryLayer.getInstance().setImageViewed(image),
            "An image should be set as viewed if there is an image and a dataset");
    }

    @Test
    void testGetChangesetSourceTag() {
        String actualChangesetSourceTag = MapillaryLayer.getInstance().getChangesetSourceTag();
        assertNull(actualChangesetSourceTag,
            "OpenStreetmap changeset source for Mapillary layer should be 'null' when there is no dataset");
        DataSet ds = new DataSet();
        Node node = new Node(LatLon.ZERO);
        ds.addPrimitive(node);
        node.setModified(true);
        INode image = JsonImageDetailsDecoderTest.createDownloadedImage("1", LatLon.ZERO, 0, false);
        MainApplication.getLayerManager().addLayer(new OsmDataLayer(ds, "Test Layer", null));
        MapillaryLayer.getInstance().setImageViewed(image);
        actualChangesetSourceTag = MapillaryLayer.getInstance().getChangesetSourceTag();
        assertEquals("Mapillary", actualChangesetSourceTag,
            "OpenStreetmap changeset source for Mapillary layer should be 'Mapillary'");
        node.setCoor(LatLon.NORTH_POLE);
        actualChangesetSourceTag = MapillaryLayer.getInstance().getChangesetSourceTag();
        assertNull(actualChangesetSourceTag,
            "OpenStreetmap changeset source for Mapillary layer should be 'null' when the viewed images are very far from the modified objects");
        node.setCoor(new LatLon(0.0049, 0.0049));
        actualChangesetSourceTag = MapillaryLayer.getInstance().getChangesetSourceTag();
        assertNull(actualChangesetSourceTag,
            "OpenStreetmap changeset source for Mapillary layer should be 'null' when the viewed images are more than 30.0m away (default)");
    }

    @Test
    void testClearInstance() {
        MapillaryLayer.getInstance();
        assertTrue(MapillaryLayer.hasInstance());
        MainApplication.getLayerManager().removeLayer(MapillaryLayer.getInstance());
        assertFalse(MapillaryLayer.hasInstance());
        MapillaryLayer.getInstance();
        assertTrue(MapillaryLayer.hasInstance());
    }

    /**
     * Non-regression test for JOSM #21329: IAE when setting osm id
     */
    @Test
    void testNonRegression21329() {
        final MapillaryLayer layer = MapillaryLayer.getInstance();
        TileSource tileSource;
        MVTTile mvtTile = null;
        try {
            final Method method = MVTLayer.class.getDeclaredMethod("getTileSource");
            method.setAccessible(true);
            tileSource = (TileSource) method.invoke(layer);

            mvtTile = new MVTTile(tileSource, 3251, 6258, 14);
            final Field field = MVTTile.class.getDeclaredField("layers");
            field.setAccessible(true);
            field.set(mvtTile, Collections.emptyList());
        } catch (ReflectiveOperationException e) {
            fail(e);
        }
        final VectorNode vectorNodeZero = new VectorNode("test");
        final VectorNode vectorNodeOne = new VectorNode("test");
        vectorNodeZero.setCoor(LatLon.ZERO);
        vectorNodeOne.setCoor(LatLon.ZERO);
        vectorNodeZero.put(MapillaryImageUtils.ImageProperties.ID.toString(), "0");
        vectorNodeOne.put(MapillaryImageUtils.ImageProperties.ID.toString(), "100");
        for (VectorNode node : Arrays.asList(vectorNodeZero, vectorNodeOne)) {
            mvtTile.getData().getAllPrimitives().add(node);
            mvtTile.getData().getPrimitivesMap().put(node.getPrimitiveId(), node);
            mvtTile.getData().getStore().addPrimitive(node);
        }
        final MVTTile finalTile = mvtTile;
        assertDoesNotThrow(() -> layer.finishedLoading(finalTile));
        assertAll(() -> assertEquals(0, vectorNodeZero.getOsmId()), () -> assertEquals(100, vectorNodeOne.getOsmId()));
    }
}
