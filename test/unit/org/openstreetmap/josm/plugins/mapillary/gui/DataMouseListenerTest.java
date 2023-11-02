// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.event.MouseEvent;
import java.awt.image.BufferedImage;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import org.awaitility.Awaitility;
import org.awaitility.Durations;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.data.imagery.ImageryInfo;
import org.openstreetmap.josm.data.imagery.vectortile.mapbox.MVTTile;
import org.openstreetmap.josm.data.imagery.vectortile.mapbox.MapboxVectorTileSource;
import org.openstreetmap.josm.data.osm.DataSet;
import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.data.osm.OsmPrimitiveType;
import org.openstreetmap.josm.data.osm.SimplePrimitiveId;
import org.openstreetmap.josm.data.projection.ProjectionRegistry;
import org.openstreetmap.josm.data.vector.VectorDataStore;
import org.openstreetmap.josm.data.vector.VectorNode;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.MapView;
import org.openstreetmap.josm.gui.layer.Layer;
import org.openstreetmap.josm.gui.layer.OsmDataLayer;
import org.openstreetmap.josm.gui.layer.imagery.MVTLayer;
import org.openstreetmap.josm.gui.util.GuiHelper;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.PointObjectLayer;
import org.openstreetmap.josm.plugins.mapillary.gui.workers.MapillaryNodesDownloader;
import org.openstreetmap.josm.plugins.mapillary.spi.preferences.MapillaryConfig;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.GuiWorkersStopper;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.MapillaryURLWireMock;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryKeys;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryMapFeatureUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.testutils.annotations.Main;
import org.openstreetmap.josm.testutils.annotations.Projection;

/**
 * Test class for {@link DataMouseListener}
 */
@Main
@MapillaryURLWireMock
@Projection
class DataMouseListenerTest {

    private DataMouseListener dataMouseListener;
    private OsmDataLayer osmDataLayer;

    @BeforeEach
    void setUp() {
        MapillaryProperties.USE_COMPUTED_LOCATIONS.put(false);
        osmDataLayer = new OsmDataLayer(new DataSet(), "DataMouseListenerTest", null);
        MainApplication.getLayerManager().addLayer(osmDataLayer);
        dataMouseListener = new DataMouseListener();
        // Zoom to test location
        MainApplication.getMap().mapView.setSize(800, 600);
        MainApplication.getMap().mapView.zoomTo(new Bounds(39.064115, -108.566401, 39.0656228, -108.5624528));
    }

    @AfterEach
    void tearDown() {
        dataMouseListener.destroy();
    }

    @ParameterizedTest
    @ValueSource(doubles = { 0.5, 1.5 })
    void testNoLayers(double zoom) {
        zoomToMetersPerPixelApproximate(zoom, false);
        simulateMove(0, 0);
        simulateClick(0, 0);
        List<Layer> layers = MainApplication.getLayerManager().getLayers();
        assertEquals(1, layers.size(),
            layers.stream().map(Layer::getClass).map(Class::getName).collect(Collectors.joining(", ")));
        assertSame(osmDataLayer, layers.get(0));
    }

    @ParameterizedTest
    @ValueSource(doubles = { 0.5, 1.5 })
    void testNonMapillaryMVTLayers(double zoom) {
        zoomToMetersPerPixelApproximate(zoom, false);
        MVTLayer mvtLayer = new MVTLayer(new ImageryInfo("dummy", MapillaryConfig.getUrls().getBaseTileUrl()));
        MainApplication.getLayerManager().addLayer(mvtLayer);
        simulateMove(0, 0);
        simulateClick(0, 0);
        assertEquals(2, MainApplication.getLayerManager().getLayers().size());
        assertSame(osmDataLayer, MainApplication.getLayerManager().getLayers().get(0));
        assertSame(mvtLayer, MainApplication.getLayerManager().getLayers().get(1));
    }

    @Test
    @GuiWorkersStopper
    void testSelect148137757289079() {
        MapillaryLayer layer = MapillaryLayer.getInstance();
        TestVectorDataStore vds = new TestVectorDataStore();
        // We don't have "real" tiles in the repo. This downloads the image and then selects it.
        new MapillaryNodesDownloader(i -> i.forEach(vds::addPrimitive), 148_137_757_289_079L).execute();
        Awaitility.await().atMost(Durations.FIVE_SECONDS).until(
            () -> vds.getPrimitivesMap().get(new SimplePrimitiveId(148137757289079L, OsmPrimitiveType.NODE)) != null);
        layer.finishedLoading(
            new TestTile((MVTTile) layer.createTile(new MapboxVectorTileSource(layer.getInfo()), 3251, 6257, 14), vds));
        VectorNode toSelect = (VectorNode) layer.getData().getPrimitiveById(148137757289079L, OsmPrimitiveType.NODE);
        testCommonSelection(layer, toSelect);
        // Check deselection
        MouseEvent clickEvent = new MouseEvent(MainApplication.getMap(), MouseEvent.MOUSE_CLICKED,
            System.currentTimeMillis(), 0, 0, 0, MapillaryProperties.DESELECT_CLICK_COUNT.get(), false,
            MouseEvent.BUTTON1);
        assertDoesNotThrow(() -> dataMouseListener.mouseClicked(clickEvent));
        assertFalse(layer.getData().getSelected().contains(toSelect));
    }

    @Test
    @GuiWorkersStopper
    void testSelect496980935069177() {
        PointObjectLayer layer = new PointObjectLayer(MapillaryKeys.MAPILLARY_TRAFFIC_SIGNS);
        MainApplication.getLayerManager().addLayer(layer);
        TestVectorDataStore vds = new TestVectorDataStore();
        // We don't have "real" tiles in the repo. This downloads the image and then selects it.
        VectorNode tNode = new VectorNode("object-detections");
        tNode.setOsmId(49_698_093_5069_177L, 1);
        tNode.setCoor(new LatLon(41.341166490122, -83.417193328459));
        MapillaryMapFeatureUtils.getImageIds(tNode);
        vds.addPrimitive(tNode);
        layer.finishedLoading(
            new TestTile((MVTTile) layer.createTile(new MapboxVectorTileSource(layer.getInfo()), 3251, 6257, 14), vds));
        VectorNode toSelect = (VectorNode) layer.getData().getPrimitiveById(49_698_093_5069_177L,
            OsmPrimitiveType.NODE);
        testCommonSelection(layer, toSelect);
        // Check deselection
        simulateClick(0, 0);
        assertFalse(layer.getData().getSelected().contains(toSelect));
    }

    private void testCommonSelection(MVTLayer layer, VectorNode toSelect) {
        final BufferedImage bi = new BufferedImage(800, 600, BufferedImage.TYPE_INT_ARGB);
        final Graphics2D graphics2D = bi.createGraphics();
        graphics2D.setClip(0, 0, 800, 600);
        MainApplication.getMap().mapView.paint(graphics2D);
        Point point = MainApplication.getMap().mapView.getPoint(toSelect);
        simulateMove(point.x, point.y);
        assertTrue(layer.getData().getSelected().isEmpty());
        assertFalse(layer.getData().getHighlighted().isEmpty());
        assertTrue(toSelect.isHighlighted());
        simulateClick(point.x, point.y);
        assertTrue(layer.getData().getSelected().contains(toSelect));
    }

    private static class TestVectorDataStore extends VectorDataStore {
        public void addPrimitive(INode mapillaryNode) {
            VectorNode node = new VectorNode("mapillary-image");
            node.setCoor(mapillaryNode.getCoor());
            mapillaryNode.getKeys().forEach(node::put);
            // Don't allow sequence downloads if it is selected.
            node.remove(MapillaryImageUtils.ImageProperties.SEQUENCE.toString());
            node.setOsmId(mapillaryNode.getOsmId(), mapillaryNode.getVersion());
            super.addPrimitive(node);
        }
    }

    private static class TestTile extends MVTTile {
        private final VectorDataStore data;

        public TestTile(MVTTile other, VectorDataStore data) {
            super(other.getSource(), other.getXtile(), other.getYtile(), other.getZoom());
            this.data = data;
        }

        @Override
        public VectorDataStore getData() {
            return this.data;
        }

        @Override
        public Collection<org.openstreetmap.josm.data.imagery.vectortile.mapbox.Layer> getLayers() {
            return Collections.emptyList();
        }
    }

    private void simulateClick(int x, int y) {
        MouseEvent clickEvent = new MouseEvent(MainApplication.getMap(), MouseEvent.MOUSE_CLICKED,
            System.currentTimeMillis(), 0, x, y, 1, false, MouseEvent.BUTTON1);
        assertDoesNotThrow(
            () -> GuiHelper.runInEDTAndWaitWithException(() -> dataMouseListener.mouseClicked(clickEvent)));
    }

    private void simulateMove(int x, int y) {
        MouseEvent moveEvent = new MouseEvent(MainApplication.getMap(), MouseEvent.MOUSE_MOVED,
            System.currentTimeMillis(), 0, x, y, 1, false, MouseEvent.NOBUTTON);
        assertDoesNotThrow(() -> GuiHelper.runInEDTAndWaitWithException(() -> dataMouseListener.mouseMoved(moveEvent)));
    }

    /**
     * Zoom to a target magnification
     *
     * @param target The pixels to meter to target (will _not_ be exact)
     * @param roundUp {@code true} to round up (e.g., if bracketing zooms are 9 and 11, and
     *        desired zoom is 10, use the zoom for that is 11 px/meter)
     */
    private void zoomToMetersPerPixelApproximate(double target, boolean roundUp) {
        MapView mapView = MainApplication.getMap().mapView;
        while (getScaleMetersPerPixel(mapView) > target) {
            mapView.zoomIn();
        }

        while (getScaleMetersPerPixel(mapView) < target) {
            mapView.zoomOut();
        }

        if (!roundUp && getScaleMetersPerPixel(mapView) > target) {
            mapView.zoomIn();
        }
    }

    private double getScaleMetersPerPixel(MapView mapView) {
        final double scaleInEastNorthUnitsPerPixel = mapView.getScale();
        return ProjectionRegistry.getProjection().getMetersPerUnit() * scaleInEastNorthUnitsPerPixel;
    }
}
