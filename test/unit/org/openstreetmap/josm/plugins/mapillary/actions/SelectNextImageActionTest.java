package org.openstreetmap.josm.plugins.mapillary.actions;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.util.stream.Stream;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.extension.RegisterExtension;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.openstreetmap.josm.data.osm.OsmPrimitiveType;
import org.openstreetmap.josm.data.osm.SimplePrimitiveId;
import org.openstreetmap.josm.data.vector.VectorDataSet;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.util.GuiHelper;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryMainDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.io.download.MapillaryDownloader;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.MapillaryCaches;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.MapillaryLayerAnnotation;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.MapillaryURLWireMock;
import org.openstreetmap.josm.testutils.JOSMTestRules;
import org.openstreetmap.josm.testutils.annotations.BasicPreferences;

/**
 * Test class for {@link SelectNextImageAction}
 *
 * @author Taylor Smock
 */
@MapillaryLayerAnnotation
@MapillaryURLWireMock
@MapillaryCaches
@BasicPreferences
class SelectNextImageActionTest {
    // Needed to set HTTP factory and shortcuts(main) and projection (for layers)
    @RegisterExtension
    static JOSMTestRules josmTestRules = new JOSMTestRules().main().projection();

    static Stream<Arguments> selectNextActionObjects() {
        return Stream.of(
            Arguments.of(SelectNextImageAction.NEXT_ACTION, 4235112816526838L, 464249047982277L, 308609047601518L),
            Arguments.of(SelectNextImageAction.PREVIOUS_ACTION, 4235112816526838L, 338231874314914L, 311799370533334L),
            Arguments.of(SelectNextImageAction.LAST_ACTION, 4235112816526838L, 311681117131457L, 4235112816526838L),
            Arguments.of(SelectNextImageAction.FIRST_ACTION, 4235112816526838L, 148137757289079L, 4235112816526838L));
    }

    @BeforeEach
    void beforeEach() {
        // Ensure that the main dialog is initialized
        MapillaryLayer.getInstance().getData().addSelectionListener(MapillaryMainDialog.getInstance());
        if (MainApplication.getLayerManager().getLayersOfType(MapillaryLayer.class).isEmpty()) {
            MainApplication.getLayerManager().addLayer(MapillaryLayer.getInstance());
        }
        // Get some sample (pre-downloaded) data.
        MapillaryDownloader.downloadSequences("7nfcwfvjdtphz7yj6zat6a")
            .forEach(MapillaryLayer.getInstance().getData()::addPrimitive);
    }

    @AfterEach
    void afterEach() {
        if (MapillaryMainDialog.hasInstance()) {
            MapillaryMainDialog.getInstance().destroy();
        }
    }

    /**
     * Tests for various {@link SelectNextImageAction}
     *
     * @param selectNextImageAction The action to use
     * @param initialId The initial node
     * @param nextId The next node (on action performed)
     * @param nextNextId The final node (on second action performed)
     */
    @ParameterizedTest
    @MethodSource("selectNextActionObjects")
    void actionPerformed(final SelectNextImageAction selectNextImageAction, final long initialId, final long nextId,
        final long nextNextId) {
        MapillaryLayer.getInstance().getData().setSelected(new SimplePrimitiveId(initialId, OsmPrimitiveType.NODE));
        assertEquals(initialId, MapillaryLayer.getInstance().getData().getSelected().iterator().next().getId());
        selectNextImageAction.actionPerformed(null);
        assertEquals(nextId, MapillaryLayer.getInstance().getData().getSelected().iterator().next().getId());
        selectNextImageAction.actionPerformed(null);
        assertEquals(nextNextId, MapillaryLayer.getInstance().getData().getSelected().iterator().next().getId());
        // No matter how many times the action is performed, it should never error out
        assertEquals(1, MapillaryLayer.getInstance().getData().getWays().size());
        for (int i = 0; i < MapillaryLayer.getInstance().getData().getWays().iterator().next().getNodesCount()
            + 10; i++) {
            selectNextImageAction.actionPerformed(null);
        }
    }

    static Stream<Arguments> updateEnabled() {
        return Stream.of(Arguments.of(SelectNextImageAction.NEXT_ACTION, true, true, false),
            Arguments.of(SelectNextImageAction.PREVIOUS_ACTION, false, true, true),
            Arguments.of(SelectNextImageAction.LAST_ACTION, true, true, false),
            Arguments.of(SelectNextImageAction.FIRST_ACTION, false, true, true));
    }

    @ParameterizedTest
    @MethodSource
    void updateEnabled(SelectNextImageAction action, boolean startState, boolean middleState, boolean endState) {
        final long start = 148137757289079L;
        final long middle = 4235112816526838L;
        final long end = 311681117131457L;
        VectorDataSet vectorDataSet = MapillaryLayer.getInstance().getData();

        vectorDataSet.clearSelection();
        action.updateEnabledState();
        assertFalse(action.isEnabled());

        vectorDataSet.setSelected(new SimplePrimitiveId(start, OsmPrimitiveType.NODE));
        assertEquals(start, vectorDataSet.getSelected().iterator().next().getId());
        action.updateEnabledState();
        assertEquals(startState, action.isEnabled());

        vectorDataSet.setSelected(new SimplePrimitiveId(middle, OsmPrimitiveType.NODE));
        assertEquals(middle, vectorDataSet.getSelected().iterator().next().getId());
        action.updateEnabledState();
        assertEquals(middleState, action.isEnabled());

        vectorDataSet.setSelected(new SimplePrimitiveId(end, OsmPrimitiveType.NODE));
        assertEquals(end, vectorDataSet.getSelected().iterator().next().getId());
        action.updateEnabledState();
        assertEquals(endState, action.isEnabled());

        vectorDataSet.clearSelection();
        action.updateEnabledState();
        assertFalse(action.isEnabled());
    }

    @ParameterizedTest
    @MethodSource("selectNextActionObjects")
    void getDestinationImageSupplier(final SelectNextImageAction selectNextImageAction, final long initialId,
        final long nextId) {
        MapillaryLayer.getInstance().getData().setSelected(new SimplePrimitiveId(initialId, OsmPrimitiveType.NODE));
        assertEquals(initialId, MapillaryLayer.getInstance().getData().getSelected().iterator().next().getId());
        GuiHelper.runInEDTAndWait(() -> {
            /* do nothing -- just ensures EDT finishes */});
        assertNotNull(selectNextImageAction.getDestinationImageSupplier().get());
        GuiHelper.runInEDTAndWait(() -> {
            /* do nothing -- just ensures EDT finishes */});
        assertEquals(nextId, selectNextImageAction.getDestinationImageSupplier().get().getId());
    }
}
