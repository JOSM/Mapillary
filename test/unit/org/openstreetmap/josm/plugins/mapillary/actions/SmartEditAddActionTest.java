// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.actions;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.awt.GraphicsEnvironment;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import mockit.Invocation;
import mockit.Mock;
import mockit.MockUp;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.extension.RegisterExtension;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.data.osm.DataSet;
import org.openstreetmap.josm.data.osm.Node;
import org.openstreetmap.josm.data.osm.OsmPrimitive;
import org.openstreetmap.josm.data.osm.Tag;
import org.openstreetmap.josm.data.vector.VectorNode;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.layer.OsmDataLayer;
import org.openstreetmap.josm.gui.tagging.presets.TaggingPreset;
import org.openstreetmap.josm.gui.tagging.presets.TaggingPresetItem;
import org.openstreetmap.josm.gui.tagging.presets.items.Key;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.ObjectDetections;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.PointObjectLayer;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.MapillaryLayerAnnotation;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.MapillaryURLWireMock;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.MapillaryURLWireMockErrors;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.ObjectDetectionsAnnotation;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryKeys;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryMapFeatureUtils;
import org.openstreetmap.josm.testutils.JOSMTestRules;
import org.openstreetmap.josm.testutils.annotations.BasicPreferences;

/**
 * Test class for {@link SmartEditAddAction}
 *
 * @author Taylor Smock
 */
@MapillaryLayerAnnotation
@MapillaryURLWireMock
@BasicPreferences
@ObjectDetectionsAnnotation
class SmartEditAddActionTest {
    // Needed for layer cleanup and for UI mocks
    @RegisterExtension
    static JOSMTestRules josmTestRules = new JOSMTestRules().main().projection().presets();
    private PointObjectLayer pointObjectLayer;
    private VectorNode node;

    /**
     * Get a stream of arguments
     *
     * @return Arguments.of(ObjectDetection, boolean willBeAdded)
     */
    static Stream<Arguments> detectionsAreAdded() {
        return Stream.of(Arguments.of(ObjectDetections.valueOfMapillaryValue("object--fire-hydrant"), true),
            Arguments.of(ObjectDetections.valueOfMapillaryValue("regulatory--stop--g1"), true),
            Arguments.of(ObjectDetections.valueOfMapillaryValue("human--rider--bicyclist"), false));
    }

    @BeforeEach
    void setUp() {
        pointObjectLayer = new PointObjectLayer(MapillaryKeys.MAPILLARY_TRAFFIC_SIGNS);
        node = new VectorNode("");
        node.setCoor(LatLon.ZERO);
        // This id is actually for complementary--both-directions--g2
        node.setOsmId(496980935069177L, 1);
        pointObjectLayer.getData().addPrimitive(node);
    }

    @MapillaryURLWireMockErrors
    @ParameterizedTest
    @MethodSource("detectionsAreAdded")
    void actionPerformedNoOsmLayer(ObjectDetections detection) {
        node.put("value", detection.getKey());
        final SmartEditAddAction smartEditAddAction = new SmartEditAddAction(pointObjectLayer, node);
        smartEditAddAction.actionPerformed(null);
        assertAll(() -> assertFalse(node.isDeleted()), () -> assertFalse(node.isDisabled()),
            () -> assertTrue(node.isVisible()));

    }

    @MapillaryURLWireMockErrors
    @ParameterizedTest
    @MethodSource("detectionsAreAdded")
    void actionPerformedOsmLayerLocked(ObjectDetections detection) {
        node.put("value", detection.getKey());
        final SmartEditAddAction smartEditAddAction = new SmartEditAddAction(pointObjectLayer, node);
        final OsmDataLayer osmDataLayer = new OsmDataLayer(new DataSet(), "SmartEditAddActionTest", null);
        osmDataLayer.lock();
        MainApplication.getLayerManager().addLayer(osmDataLayer);
        smartEditAddAction.actionPerformed(null);
        assertAll(() -> assertFalse(node.isDeleted()), () -> assertFalse(node.isDisabled()),
            () -> assertTrue(node.isVisible()));
    }

    @MapillaryURLWireMockErrors
    @ParameterizedTest
    @MethodSource("detectionsAreAdded")
    void actionPerformedOsmLayerUnlockedNoApply(ObjectDetections detection) {
        node.put("value", detection.getKey());
        final SmartEditAddAction smartEditAddAction = new SmartEditAddAction(pointObjectLayer, node);
        final OsmDataLayer osmDataLayer = new OsmDataLayer(new DataSet(), "SmartEditAddActionTest", null);
        MainApplication.getLayerManager().addLayer(osmDataLayer);
        TaggingPresetMock taggingPresetMock = new TaggingPresetMock();
        taggingPresetMock.result = TaggingPreset.DIALOG_ANSWER_CANCEL;
        taggingPresetMock.tags = TaggingPresetMock
            .getTagOptions(detection.getTaggingPresets().stream().findFirst().orElse(null)).entrySet().stream()
            .filter(entry -> !entry.getValue().isEmpty()).map(entry -> new Tag(entry.getKey(), entry.getValue().get(0)))
            .collect(Collectors.toList());
        smartEditAddAction.actionPerformed(null);
        assertAll(() -> assertFalse(node.isDeleted()), () -> assertFalse(node.isDisabled()),
            () -> assertTrue(node.isVisible()));
    }

    @MapillaryURLWireMockErrors
    @ParameterizedTest
    @MethodSource("detectionsAreAdded")
    void actionPerformedOsmLayerUnlockedApply(ObjectDetections detection, boolean added) {
        final OsmDataLayer osmDataLayer = this.commonApply(detection);
        final SmartEditAddAction smartEditAddAction = new SmartEditAddAction(pointObjectLayer, node);

        smartEditAddAction.actionPerformed(null);
        if (added) {
            assertAll(() -> assertTrue(node.isDeleted(), "The node should be deleted"), () -> assertEquals(1,
                osmDataLayer.getDataSet().allPrimitives().size(), "The data layer should have a new node"));
        } else {
            assertAll(() -> assertFalse(node.isDeleted()), () -> assertFalse(node.isDisabled()),
                () -> assertTrue(node.isVisible()),
                () -> assertEquals(0, osmDataLayer.getDataSet().allPrimitives().size()));
        }
    }

    @MapillaryURLWireMockErrors
    @ParameterizedTest
    @MethodSource("detectionsAreAdded")
    void testAddMapillaryTagsMapObjectNoMapillaryLayer(ObjectDetections detection, boolean added) {
        // The detection has the id of the detection object
        node.setOsmId(1234, 1);
        node.put(MapillaryMapFeatureUtils.MapFeatureProperties.IMAGES.toString(), "12345678");
        final OsmDataLayer osmDataLayer = this.commonApply(detection);
        new SmartEditAddAction(pointObjectLayer, node).actionPerformed(null);
        if (added) {
            final Node osmNode = osmDataLayer.getDataSet().getNodes().stream().findAny().orElse(null);
            assertNotNull(osmNode);
            assertAll(() -> assertEquals(Long.toString(node.getId()), osmNode.get("mapillary:map_feature")),
                () -> assertFalse(osmNode.hasKey("mapillary")), () -> assertFalse(osmNode.hasKey("mapillary:image")));
        }
    }

    @MapillaryURLWireMockErrors
    @ParameterizedTest
    @MethodSource("detectionsAreAdded")
    void testAddMapillaryTagsMapObjectWithMapillaryLayerNoMapillarySelected(ObjectDetections detection, boolean added) {
        VectorNode image = new VectorNode("");
        image.setOsmId(12345678, 1);
        MapillaryLayer.getInstance().getData().addPrimitive(image);
        this.testAddMapillaryTagsMapObjectNoMapillaryLayer(detection, added);
    }

    @MapillaryURLWireMockErrors
    @ParameterizedTest
    @MethodSource("detectionsAreAdded")
    void testAddMapillaryTagsMapObjectWithMapillaryLayerMapillarySelected(ObjectDetections detection, boolean added) {
        VectorNode image = new VectorNode("");
        image.setOsmId(12345678, 1);
        MapillaryLayer.getInstance().getData().addPrimitive(image);
        MapillaryLayer.getInstance().getData().setSelected(image);
        node.setOsmId(1234, 1);
        node.put(MapillaryMapFeatureUtils.MapFeatureProperties.IMAGES.toString(), "12345678");
        final OsmDataLayer osmDataLayer = this.commonApply(detection);
        new SmartEditAddAction(pointObjectLayer, node).actionPerformed(null);
        if (added) {
            final Node osmNode = osmDataLayer.getDataSet().getNodes().stream().findAny().orElse(null);
            assertNotNull(osmNode);
            assertAll(() -> assertEquals(Long.toString(node.getId()), osmNode.get("mapillary:map_feature")),
                () -> assertFalse(osmNode.hasKey("mapillary")),
                () -> assertEquals(Long.toString(image.getId()), osmNode.get("mapillary:image")));
        }
    }

    /**
     * Do the common actions for actual apply code
     *
     * @param detection The detection that will be applied
     * @return The osm data layer that will be applied to
     */
    private OsmDataLayer commonApply(ObjectDetections detection) {
        final OsmDataLayer osmDataLayer = new OsmDataLayer(new DataSet(), "SmartEditAddActionTest", null);
        node.put("value", detection.getKey());
        MainApplication.getLayerManager().addLayer(osmDataLayer);
        TaggingPresetMock taggingPresetMock = new TaggingPresetMock();

        taggingPresetMock.result = TaggingPreset.DIALOG_ANSWER_APPLY;
        taggingPresetMock.tags = TaggingPresetMock
            .getTagOptions(detection.getTaggingPresets().stream().findFirst().orElse(null)).entrySet().stream()
            .filter(entry -> !entry.getValue().isEmpty()).map(entry -> new Tag(entry.getKey(), entry.getValue().get(0)))
            .collect(Collectors.toList());
        return osmDataLayer;
    }

    private static class TaggingPresetMock extends MockUp<TaggingPreset> {
        int result;
        List<Tag> tags = Collections.emptyList();

        @Mock
        public int showDialog(Invocation invocation, Collection<OsmPrimitive> sel, boolean showNewRelation) {
            if (!GraphicsEnvironment.isHeadless()) {
                invocation.proceed();
            }
            return this.result;
        }

        @Mock
        public List<Tag> getChangedTags(Invocation invocation) {
            if (!GraphicsEnvironment.isHeadless()) {
                return invocation.proceed();
            }
            return this.tags;
        }

        /**
         * Get the tag options for a preset
         *
         * @param taggingPreset The tagging preset to parse
         * @return The options (currently only returns set values, may change in future)
         */
        @Nonnull
        public static Map<String, List<String>> getTagOptions(@Nullable TaggingPreset taggingPreset) {
            if (taggingPreset == null) {
                return Collections.emptyMap();
            }
            Map<String, List<String>> tagMap = new HashMap<>(taggingPreset.data.size());
            for (TaggingPresetItem item : taggingPreset.data) {
                // For now, just do the items with known values
                if (item instanceof Key) {
                    tagMap.put(((Key) item).key, Collections.singletonList(((Key) item).value));
                }
            }
            return tagMap;
        }
    }
}
