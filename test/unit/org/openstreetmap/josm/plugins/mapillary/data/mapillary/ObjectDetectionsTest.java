// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.data.mapillary;

import static org.junit.jupiter.api.Assertions.assertFalse;

import java.lang.reflect.Field;

import org.junit.jupiter.api.extension.RegisterExtension;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

import org.openstreetmap.josm.testutils.JOSMTestRules;

/**
 *
 */
class ObjectDetectionsTest {
  @RegisterExtension
  static JOSMTestRules rule = new JOSMTestRules().presets();

  @ParameterizedTest
  @EnumSource(ObjectDetections.class)
  void testGetTaggingPreset(ObjectDetections detection) throws ReflectiveOperationException {
    Field osmKey = ObjectDetections.class.getDeclaredField("osmKey");
    osmKey.setAccessible(true);
    if (osmKey.get(detection) != null) {
      assertFalse(ObjectDetections.getTaggingPresetsFor(detection.getKey()).isEmpty());
    }
  }

}
