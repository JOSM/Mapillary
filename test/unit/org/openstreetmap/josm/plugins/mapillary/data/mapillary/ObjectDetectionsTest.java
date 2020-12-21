// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.data.mapillary;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;

import java.lang.reflect.Field;
import java.util.stream.Stream;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.RegisterExtension;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import org.openstreetmap.josm.actions.ExpertToggleAction;
import org.openstreetmap.josm.plugins.mapillary.gui.DeveloperToggleAction;
import org.openstreetmap.josm.testutils.JOSMTestRules;

/**
 *
 */
class ObjectDetectionsTest {
  @RegisterExtension
  static JOSMTestRules rule = new JOSMTestRules().presets();
  static Field osmKey;

  @BeforeAll
  static void beforeClass() throws ReflectiveOperationException {
    osmKey = ObjectDetections.class.getDeclaredField("osmKey");
    osmKey.setAccessible(true);
    ObjectDetections.updatePresets();
  }

  /**
   * Get production object detections for testing
   *
   * @return A stream of arguments for object detections that can be added <i>and</i> should be addable by default.
   */
  static Stream<Arguments> provideObjectDetections() {
    // Ensure that we are only testing production object detections
    assertFalse(ExpertToggleAction.isExpert());
    assertFalse(DeveloperToggleAction.isDeveloper());
    return Stream.of(ObjectDetections.values()).filter(d -> {
      try {
        return osmKey.get(d) != null;
      } catch (ReflectiveOperationException e) {
        return false;
      }
    }).filter(ObjectDetections::shouldBeAddable).map(Arguments::of);
  }

  @ParameterizedTest
  // Preferred method would be @EnumSource(ObjectDetections.class), but at ~0.5s per operation, it was too slow (~14.5
  // minutes to run).
  @MethodSource("org.openstreetmap.josm.plugins.mapillary.data.mapillary.ObjectDetectionsTest#provideObjectDetections")
  void testGetTaggingPreset(ObjectDetections detection) {
    assertNotEquals(0, ObjectDetections.getTaggingPresetsFor(detection.getKey()).length);
  }

  /**
   * Test a specific smart-add object for sanity, and as a faster check than {@link #testGetTaggingPreset}.
   */
  @Test
  void testFireHydrant() {
    ObjectDetections fireHydrant = ObjectDetections.OBJECT__FIRE_HYDRANT;
    assertEquals("object--fire-hydrant", fireHydrant.getKey());
    assertNotEquals(0, ObjectDetections.getTaggingPresetsFor(fireHydrant.getKey()).length);
  }

}
