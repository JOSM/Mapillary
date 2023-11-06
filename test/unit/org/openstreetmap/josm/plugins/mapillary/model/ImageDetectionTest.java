// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.model;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.awt.geom.Path2D;
import java.util.List;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.ObjectDetections;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.MapillaryCaches;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.MapillaryURLWireMock;

@MapillaryURLWireMock
@MapillaryCaches
class ImageDetectionTest {
    @Test
    void testBasics() {
        ImageDetection<Path2D.Double> id = new ImageDetection<>(new Path2D.Double(), 1, 2, "value");
        ImageDetection<Path2D.Double> trafficsign = new ImageDetection<>(new Path2D.Double(), 3, 4,
            ObjectDetections.valueOfMapillaryValue("regulatory--maximum-speed-limit-45--g3").toString());

        assertEquals(1, id.getImageKey());
        assertEquals(3, trafficsign.getImageKey());

        assertEquals(2, id.key());
        assertEquals(4, trafficsign.key());

        assertFalse(id.isTrafficSign());
        assertTrue(trafficsign.isTrafficSign());

        assertEquals("value", id.getUnknownValue());
        assertEquals(ObjectDetections.valueOfMapillaryValue("regulatory--maximum-speed-limit-45--g3").toString(),
            trafficsign.getUnknownValue());
    }

    static Stream<Arguments> getDetectionsArguments() {
        // 148137757289079 technically has 5 detections, but one is a duplicate.
        return Stream.of(Arguments.of(148137757289079L, 4), Arguments.of(496980935069177L, 3),
            Arguments.of(308609047601518L, 5), Arguments.of(135511895288847L, 2), Arguments.of(464249047982277L, 1),
            Arguments.of(4235112816526838L, 8), Arguments.of(311681117131457L, 1), Arguments.of(311799370533334L, 3));
    }

    @ParameterizedTest
    @MethodSource("getDetectionsArguments")
    void testGetDetectionsWait(long id, int expectedDetections) {
        List<ImageDetection<?>> detections = ImageDetection.getDetections(id, ImageDetection.Options.WAIT);
        assertEquals(expectedDetections, detections.size());
    }

    @ParameterizedTest
    @MethodSource("getDetectionsArguments")
    void testGetDetectionsNoWait(long id) {
        List<ImageDetection<?>> detections = ImageDetection.getDetections(id, ImageDetection.Options.FETCH);
        assertTrue(detections.isEmpty());
    }
}
