// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.data.mapillary;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

import java.lang.reflect.Field;
import java.util.stream.Stream;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.RegisterExtension;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

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
  }

  static Stream<Arguments> provideObjectDetections() {
    return Stream.of(ObjectDetections.values()).filter(d -> {
      try {
        return osmKey.get(d) != null;
      } catch (ReflectiveOperationException e) {
        return false;
      }
    }).map(Arguments::of);
  }

  @ParameterizedTest
  // Preferred method would be @EnumSource(ObjectDetections.class), but at ~0.5s per operation, it was too slow (~14.5
  // minutes to run).
  @MethodSource("org.openstreetmap.josm.plugins.mapillary.data.mapillary.ObjectDetectionsTest#provideObjectDetections")
  void testGetTaggingPreset(ObjectDetections detection) {
    assertFalse(ObjectDetections.getTaggingPresetsFor(detection.getKey()).isEmpty());
  }

  /**
   * Test a specific smart-add object for sanity, and as a faster check than {@link #testGetTaggingPreset}.
   */
  @Test
  void testFireHydrant() {
    ObjectDetections fireHydrant = ObjectDetections.OBJECT__FIRE_HYDRANT;
    assertEquals("object--fire-hydrant", fireHydrant.getKey());
    assertFalse(ObjectDetections.getTaggingPresetsFor(fireHydrant.getKey()).isEmpty());
  }

}
