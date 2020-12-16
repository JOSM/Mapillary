// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import static org.openstreetmap.josm.plugins.mapillary.utils.api.JsonDecoderTest.assertDecodesToNull;

import java.awt.geom.AffineTransform;
import java.awt.geom.PathIterator;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonReader;
import javax.json.JsonValue;

import org.junit.jupiter.api.Test;

import org.openstreetmap.josm.plugins.mapillary.data.mapillary.ObjectDetections;
import org.openstreetmap.josm.plugins.mapillary.model.ImageDetection;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil;

class JsonImageDetectionDecoderTest {

  @Test
  void testDecodeImageDetections() throws IOException {
    try (InputStream stream = this.getClass().getResourceAsStream("/api/v3/responses/searchImageDetections.json");
      JsonReader reader = Json.createReader(stream)) {
      final Collection<ImageDetection<?>> decodeResult = JsonDecoder.decodeFeatureCollection(reader.readObject(),
        JsonImageDetectionDecoder::decodeImageDetection);

      assertNotNull(decodeResult);
      assertEquals(2, decodeResult.size());

      // Keep in mind, that no particular order is enforced for the decoded result.
      // That's why the entries of the collection are put into a Map, so that we can refer to the entries by key.
      final Iterator<ImageDetection<?>> it = decodeResult.iterator();
      HashMap<String, ImageDetection<?>> detections = new HashMap<>();
      while (it.hasNext()) {
        final ImageDetection<?> detection = it.next();
        detections.put(detection.getKey(), detection);
      }

      final ImageDetection<?> id1 = detections.get("bzqdn10wz1s1xd3lae3hawgja0");
      assertEquals("33zgql54_tBVvmIij0zrcA", id1.getImageKey());
      assertEquals("bzqdn10wz1s1xd3lae3hawgja0", id1.getKey());
      assertEquals(0.000001, id1.getScore(), 1e-9);
      assertEquals(ObjectDetections.INFORMATION__PEDESTRIANS_CROSSING__G1, id1.getValue());

      final PathIterator pi1 = id1.getShape().getPathIterator(new AffineTransform());
      testPathSegment(pi1, PathIterator.SEG_MOVETO, 0.42724609375, 0.69091796875);
      testPathSegment(pi1, PathIterator.SEG_LINETO, 0.436279296875, 0.69091796875);
      testPathSegment(pi1, PathIterator.SEG_LINETO, 0.436279296875, 0.70263671875);
      testPathSegment(pi1, PathIterator.SEG_LINETO, 0.42724609375, 0.70263671875);
      testPathSegment(pi1, PathIterator.SEG_LINETO, 0.42724609375, 0.69091796875);
      testPathSegment(pi1, PathIterator.SEG_CLOSE, 0, 0);

      final ImageDetection<?> id2 = detections.get("uzve1xkyk5qbjwrzaq0do09u1x");
      assertEquals("33zgql54_tBVvmIij0zrcA", id2.getImageKey());
      assertEquals("uzve1xkyk5qbjwrzaq0do09u1x", id2.getKey());
      assertEquals(0.000001, id2.getScore(), 1e-9);
      assertEquals(ObjectDetections.INFORMATION__PEDESTRIANS_CROSSING__G1, id2.getValue());

      final PathIterator pi2 = id2.getShape().getPathIterator(new AffineTransform());
      testPathSegment(pi2, PathIterator.SEG_MOVETO, 0.42724609375, 0.69091796875);
      testPathSegment(pi2, PathIterator.SEG_LINETO, 0.436279296875, 0.69091796875);
      testPathSegment(pi2, PathIterator.SEG_LINETO, 0.436279296875, 0.70263671875);
      testPathSegment(pi2, PathIterator.SEG_LINETO, 0.42724609375, 0.70263671875);
      testPathSegment(pi2, PathIterator.SEG_LINETO, 0.42724609375, 0.69091796875);
      testPathSegment(pi2, PathIterator.SEG_CLOSE, 0, 0);
    }
  }

  @Test
  void testDecodeImageDetection() throws IOException {
    try (InputStream stream = this.getClass().getResourceAsStream("/api/v3/responses/imageDetection.json");
      JsonReader reader = Json.createReader(stream)) {
      final ImageDetection<?> decodeResult = JsonImageDetectionDecoder.decodeImageDetection(reader.readObject());

      assertNotNull(decodeResult);
      assertEquals("QhRcdlGS_Rn_a1_HTclefg", decodeResult.getImageKey());
      assertEquals("gn0llgitnnuqonecevbmf52ino", decodeResult.getKey());
      assertEquals("trafficsign", decodeResult.getPackage());
      assertTrue(decodeResult.isTrafficSign());
      assertEquals(0.710661225175, decodeResult.getScore(), 1e-9);
      assertEquals(ObjectDetections.REGULATORY__NO_OVERTAKING_BY_HEAVY_GOODS_VEHICLES__G1, decodeResult.getValue());

      final PathIterator pi = decodeResult.getShape().getPathIterator(new AffineTransform());
      testPathSegment(pi, PathIterator.SEG_MOVETO, 0.330078125, 0.466064453125);
      testPathSegment(pi, PathIterator.SEG_LINETO, 0.3642578125, 0.466064453125);
      testPathSegment(pi, PathIterator.SEG_LINETO, 0.3642578125, 0.51171875);
      testPathSegment(pi, PathIterator.SEG_LINETO, 0.330078125, 0.51171875);
      testPathSegment(pi, PathIterator.SEG_LINETO, 0.330078125, 0.466064453125);
      testPathSegment(pi, PathIterator.SEG_CLOSE, 0, 0);
      assertTrue(pi.isDone());
    }
  }

  private static void testPathSegment(final PathIterator it, final int expSegmentType, final double x, final double y) {
    final double[] segmentData = new double[6];
    final int actualSegmentType = it.currentSegment(segmentData);
    assertFalse(it.isDone());
    assertEquals(expSegmentType, actualSegmentType);
    assertEquals(x, segmentData[0], 1e-9);
    assertEquals(y, segmentData[1], 1e-9);
    assertEquals(0, segmentData[2], 1e-9);
    assertEquals(0, segmentData[3], 1e-9);
    assertEquals(0, segmentData[4], 1e-9);
    assertEquals(0, segmentData[5], 1e-9);
    it.next();
  }

  @Test
  void testImageDetectionInvalid() {
    // null input
    assertNull(JsonImageDetectionDecoder.decodeImageDetection(null));
    // invalid type is set
    assertDecodesToNull(JsonImageDetectionDecoder::decodeImageDetection, "{\"type\":\"ItsABugNotA\"}");
    // `properties` key is not set
    assertDecodesToNull(JsonImageDetectionDecoder::decodeImageDetection, "{\"type\": \"Feature\"}");
    // invalid score value
    assertDecodesToNull(JsonImageDetectionDecoder::decodeImageDetection,
      "{\"type\": \"Feature\", \"properties\":{\"score\":\"invalidScoreValue\"}}");
    // Different properties missing
    assertDecodesToNull(JsonImageDetectionDecoder::decodeImageDetection,
      "{\"type\": \"Feature\", \"properties\":{\"shape\":{\"type\":\"Polygon\", \"coordinates\":[[[0,0], [0,1]]]}}}");
    assertDecodesToNull(JsonImageDetectionDecoder::decodeImageDetection,
      "{\"type\": \"Feature\", \"properties\":{\"image_key\":\"the_image_key\",",
      "\"shape\":{\"type\":\"Polygon\", \"coordinates\":[[[0,0], [0,1]]]}}}");
    assertDecodesToNull(JsonImageDetectionDecoder::decodeImageDetection,
      "{\"type\": \"Feature\", \"properties\":{\"image_key\":\"the_image_key\",\"key\": \"detection_key\",",
      "\"shape\":{\"type\":\"Polygon\", \"coordinates\":[[[0,0], [0,1]]]}}}");
    assertDecodesToNull(JsonImageDetectionDecoder::decodeImageDetection,
      "{\"type\": \"Feature\", \"properties\":{\"image_key\":\"the_image_key\",\"key\": \"detection_key\",",
      "\"score\":314159e-5, \"shape\":{\"type\":\"Polygon\", \"coordinates\":[[[0,0], [0,1]]]}}}");
    assertDecodesToNull(JsonImageDetectionDecoder::decodeImageDetection,
      "{\"type\": \"Feature\", \"properties\":{\"image_key\":\"the_image_key\",\"key\": \"detection_key\",",
      "\"score\":314159e-5, \"package\":\"arbitrary_package\", \"shape\":{\"type\":\"Polygon\", \"coordinates\":[[[0,0], [0,1]]]}}}");
  }

  @Test
  void testDecodePolygon() throws NoSuchMethodException, SecurityException, IllegalAccessException,
    IllegalArgumentException, InvocationTargetException {
    Method decodePolygon = JsonImageDetectionDecoder.class.getDeclaredMethod("decodePolygon", JsonArray.class);
    decodePolygon.setAccessible(true);
    try (InputStream stream = new ByteArrayInputStream("[\"notAnArray\"]".getBytes(StandardCharsets.UTF_8));
      JsonReader reader = Json.createReader(stream)) {
      assertNull(decodePolygon.invoke(null, reader.readArray()));
    } catch (IOException e) {
      fail(e);
    }
  }

  @Test
  void testDecodeShape() throws NoSuchMethodException, SecurityException, IllegalAccessException,
    IllegalArgumentException, InvocationTargetException {
    Method decodeShape = JsonImageDetectionDecoder.class.getDeclaredMethod("decodeShape", JsonValue.class);
    decodeShape.setAccessible(true);
    for (String i : Arrays.asList("{\"type\":\"Point\"}", "{\"type\":\"Polygon\"}",
      "{\"type\":\"Polygon\", \"coordinates\":[]}")) {
      try (InputStream stream = new ByteArrayInputStream(i.getBytes(StandardCharsets.UTF_8));
        JsonReader reader = Json.createReader(stream)) {
        assertNull(decodeShape.invoke(null, reader.readObject()));
      } catch (IOException e) {
        fail(i, e);
      }
    }
  }

  @Test
  void testDecodeSimpleShape() throws NoSuchMethodException, SecurityException, IllegalAccessException,
    IllegalArgumentException, InvocationTargetException, IOException {
    Method decodeSimplePolygon = JsonImageDetectionDecoder.class.getDeclaredMethod("decodeSimplePolygon",
      JsonArray.class);
    decodeSimplePolygon.setAccessible(true);
    try (InputStream stream = new ByteArrayInputStream("[\"notAnArray\"]".getBytes(StandardCharsets.UTF_8));
      JsonReader reader = Json.createReader(stream)) {
      assertNull(decodeSimplePolygon.invoke(null, reader.readArray()));
    }
  }

  @Test
  void testUtilityClass() {
    TestUtil.testUtilityClass(JsonImageDetectionDecoder.class);
  }
}
