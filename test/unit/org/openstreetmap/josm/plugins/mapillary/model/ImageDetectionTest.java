// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.model;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.awt.geom.Path2D;
import java.util.List;
import java.util.stream.Stream;

import org.junit.jupiter.api.extension.RegisterExtension;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.ObjectDetections;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.MapillaryCaches;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.MapillaryURLWireMock;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.openstreetmap.josm.testutils.JOSMTestRules;

@MapillaryURLWireMock
@MapillaryCaches
class ImageDetectionTest {
  @RegisterExtension
  JOSMTestRules josmTestRules = new JOSMTestRules();

  @Test
  void testBasics() {
    ImageDetection<Path2D.Double> id = new ImageDetection<>(new Path2D.Double(), "imgKey", "key", "value");
    ImageDetection<Path2D.Double> trafficsign = new ImageDetection<>(new Path2D.Double(), "imgKey2", "key2",
      ObjectDetections.REGULATORY__MAXIMUM_SPEED_LIMIT_45__G3.toString());

    assertEquals("imgKey", id.getImageKey());
    assertEquals("imgKey2", trafficsign.getImageKey());

    assertEquals("key", id.getKey());
    assertEquals("key2", trafficsign.getKey());

    assertFalse(id.isTrafficSign());
    assertTrue(trafficsign.isTrafficSign());

    assertEquals("value", id.getUnknownValue());
    assertEquals(ObjectDetections.REGULATORY__MAXIMUM_SPEED_LIMIT_45__G3.toString(), trafficsign.getUnknownValue());
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
    List<ImageDetection<?>> detections = ImageDetection.getDetections(id, true);
    assertEquals(expectedDetections, detections.size());
  }

  @ParameterizedTest
  @MethodSource("getDetectionsArguments")
  void testGetDetectionsNoWait(long id, int expectedDetections) {
    List<ImageDetection<?>> detections = ImageDetection.getDetections(id, false);
    assertTrue(detections.isEmpty());
  }
}
