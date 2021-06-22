package org.openstreetmap.josm.plugins.mapillary.gui;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.data.vector.VectorNode;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.MapillaryURLWireMock;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.MapillaryURLWireMockErrors;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL;
import org.openstreetmap.josm.testutils.JOSMTestRules;
import org.openstreetmap.josm.tools.Logging;

import org.awaitility.Awaitility;
import org.awaitility.Durations;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.RegisterExtension;

/**
 * Test class for {@link MapillaryMainDialog}
 *
 * @author Taylor Smock
 */
@MapillaryURLWireMock
class MapillaryMainDialogTest {
  @RegisterExtension
  JOSMTestRules rule = new JOSMTestRules().main();

  @AfterEach
  void tearDown() throws ReflectiveOperationException {
    for (Class<?> obj : Arrays.asList(MapillaryLayer.class, MapillaryMainDialog.class)) {
      final Field instanceField = obj.getDeclaredField("instance");
      instanceField.setAccessible(true);
      instanceField.set(null, null);
    }
  }

  /**
   * Non-regression test for JOSM #21028, #21029, #21030
   */
  @MapillaryURLWireMockErrors(MapillaryURLWireMockErrors.Type.APPLICATION_REQUEST_LIMIT_REACHED)
  @Test
  void testNonRegression21028() {
    final VectorNode testImage = new VectorNode("test");
    testImage.setCoor(new LatLon(39.065986975316, -108.57079091664));
    testImage.put(MapillaryURL.APIv4.ImageProperties.ID.toString(), "311799370533334");
    testImage.put(MapillaryURL.APIv4.ImageProperties.SEQUENCE_ID.toString(), "7nfcwfvjdtphz7yj6zat6a");
    // We have a check for whether or not the image is downloaded, based off of number of tags
    IntStream.range(0, 20).forEach(i -> testImage.put(Integer.toString(i), Integer.toString(i)));
    MapillaryLayer.getInstance().getData().addPrimitive(testImage);
    Logging.clearLastErrorAndWarnings();
    assertDoesNotThrow(() -> MapillaryMainDialog.getInstance().setImage(testImage));
    Awaitility.await().pollDelay(Durations.TWO_SECONDS).catchUncaughtExceptions().ignoreNoExceptions()
      .until(() -> true);
    List<String> lastErrors = new ArrayList<>(Logging.getLastErrorAndWarnings());
    lastErrors.removeIf(string -> string.contains("Exception"));
    assertTrue(lastErrors.isEmpty(), lastErrors.stream().collect(Collectors.joining(System.lineSeparator())));
  }
}
