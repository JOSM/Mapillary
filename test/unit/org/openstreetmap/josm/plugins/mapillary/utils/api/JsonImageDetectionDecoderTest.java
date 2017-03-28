// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.openstreetmap.josm.plugins.mapillary.utils.api.JsonDecoderTest.assertDecodesToNull;

import java.awt.geom.AffineTransform;
import java.awt.geom.PathIterator;
import java.io.ByteArrayInputStream;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonReader;
import javax.json.JsonValue;

import org.junit.Test;
import org.openstreetmap.josm.plugins.mapillary.detection.ImageDetection;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil;

public class JsonImageDetectionDecoderTest {

  @Test
  public void testDecodeImageDetections() {
    final Collection<ImageDetection> decodeResult = JsonDecoder.decodeFeatureCollection(
      Json.createReader(this.getClass().getResourceAsStream("/api/v3/responses/searchImageDetections.json")).readObject(),
      JsonImageDetectionDecoder::decodeImageDetection
    );

    assertNotNull(decodeResult);
    assertEquals(2, decodeResult.size());

    // Keep in mind, that no particular order is enforced for the decoded result.
    // That's why the entries of the collection are put into a Map, so that we can refer to the entries by key.
    final Iterator<ImageDetection> it = decodeResult.iterator();
    HashMap<String, ImageDetection> detections = new HashMap<>();
    while (it.hasNext()) {
      final ImageDetection detection = it.next();
      detections.put(detection.getKey(), detection);
    }

    final ImageDetection id1 = detections.get("u0lvc577hes4a0k8pgkak6pr5g");
    assertEquals("LNuUpMXZax88-VuqNoiqEQ", id1.getImageKey());
    assertEquals("u0lvc577hes4a0k8pgkak6pr5g", id1.getKey());
    assertEquals(0.999222, id1.getScore(), 1e-9);
    assertEquals("regulatory--go-straight--g1", id1.getValue());

    final PathIterator pi1 = id1.getShape().getPathIterator(new AffineTransform());
    testPathSegment(pi1, PathIterator.SEG_MOVETO, 0.74365234375, 0.530029296875);
    testPathSegment(pi1, PathIterator.SEG_LINETO, 0.7646484375, 0.530029296875);
    testPathSegment(pi1, PathIterator.SEG_LINETO, 0.7646484375, 0.573974609375);
    testPathSegment(pi1, PathIterator.SEG_LINETO, 0.74365234375, 0.573974609375);
    testPathSegment(pi1, PathIterator.SEG_LINETO, 0.74365234375, 0.530029296875);
    testPathSegment(pi1, PathIterator.SEG_CLOSE, 0, 0);

    final ImageDetection id2 = detections.get("as4sbsi48vbfk4555q18rq9jnj");
    assertEquals("8Crlqn_Tq3Gbj_QrTSeJqQ", id2.getImageKey());
    assertEquals("as4sbsi48vbfk4555q18rq9jnj", id2.getKey());
    assertEquals(0.985142, id2.getScore(), 1e-9);
    assertEquals("regulatory--u-turn--g1", id2.getValue());

    final PathIterator pi2 = id2.getShape().getPathIterator(new AffineTransform());
    testPathSegment(pi2, PathIterator.SEG_MOVETO, 0.828125, 0.490478515625);
    testPathSegment(pi2, PathIterator.SEG_LINETO, 0.846923828125, 0.490478515625);
    testPathSegment(pi2, PathIterator.SEG_LINETO, 0.846923828125, 0.510498046875);
    testPathSegment(pi2, PathIterator.SEG_LINETO, 0.828125, 0.510498046875);
    testPathSegment(pi2, PathIterator.SEG_LINETO, 0.828125, 0.490478515625);
    testPathSegment(pi2, PathIterator.SEG_CLOSE, 0, 0);
  }

  @Test
  public void testDecodeImageDetection() {
    final ImageDetection decodeResult = JsonImageDetectionDecoder.decodeImageDetection(
      Json.createReader(this.getClass().getResourceAsStream("/api/v3/responses/imageDetection.json")).readObject()
    );

    assertNotNull(decodeResult);
    assertEquals("QhRcdlGS_Rn_a1_HTclefg", decodeResult.getImageKey());
    assertEquals("gn0llgitnnuqonecevbmf52ino", decodeResult.getKey());
    assertEquals(0.710661225175, decodeResult.getScore(), 1e-9);
    assertEquals("regulatory--no-overtaking-by-heavy-goods-vehicles--g1", decodeResult.getValue());

    final PathIterator pi = decodeResult.getShape().getPathIterator(new AffineTransform());
    testPathSegment(pi, PathIterator.SEG_MOVETO, 0.330078125, 0.466064453125);
    testPathSegment(pi, PathIterator.SEG_LINETO, 0.3642578125, 0.466064453125);
    testPathSegment(pi, PathIterator.SEG_LINETO, 0.3642578125, 0.51171875);
    testPathSegment(pi, PathIterator.SEG_LINETO, 0.330078125, 0.51171875);
    testPathSegment(pi, PathIterator.SEG_LINETO, 0.330078125, 0.466064453125);
    testPathSegment(pi, PathIterator.SEG_CLOSE, 0, 0);
    assertTrue(pi.isDone());
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
  public void testImageDetectionInvalid() {
    // null input
    assertNull(JsonImageDetectionDecoder.decodeImageDetection(null));
    // invalid type is set
    assertDecodesToNull(JsonImageDetectionDecoder::decodeImageDetection, "{\"type\":\"ItsABugNotA\"}");
    // `properties` key is not set
    assertDecodesToNull(JsonImageDetectionDecoder::decodeImageDetection, "{\"type\": \"Feature\"}");
    // invalid score value
    assertDecodesToNull(
      JsonImageDetectionDecoder::decodeImageDetection,
      "{\"type\": \"Feature\", \"properties\":{\"score\":\"invalidScoreValue\"}}"
    );
    // Different properties missing
    assertDecodesToNull(
      JsonImageDetectionDecoder::decodeImageDetection,
      "{\"type\": \"Feature\", \"properties\":{\"shape\":{\"type\":\"Polygon\", \"coordinates\":[[[0,0], [0,1]]]}}}"
    );
    assertDecodesToNull(
      JsonImageDetectionDecoder::decodeImageDetection,
      "{\"type\": \"Feature\", \"properties\":{\"image_key\":\"the_image_key\",",
      "\"shape\":{\"type\":\"Polygon\", \"coordinates\":[[[0,0], [0,1]]]}}}"
    );
    assertDecodesToNull(
      JsonImageDetectionDecoder::decodeImageDetection,
      "{\"type\": \"Feature\", \"properties\":{\"image_key\":\"the_image_key\",\"key\": \"detection_key\",",
      "\"shape\":{\"type\":\"Polygon\", \"coordinates\":[[[0,0], [0,1]]]}}}"
    );
    assertDecodesToNull(
      JsonImageDetectionDecoder::decodeImageDetection,
      "{\"type\": \"Feature\", \"properties\":{\"image_key\":\"the_image_key\",\"key\": \"detection_key\",",
      "\"score\":314159e-5, \"shape\":{\"type\":\"Polygon\", \"coordinates\":[[[0,0], [0,1]]]}}}"
    );
  }

  @Test
  public void testDecodePolygon() throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
    Method decodePolygon = JsonImageDetectionDecoder.class.getDeclaredMethod("decodePolygon", JsonArray.class);
    decodePolygon.setAccessible(true);
    assertNull(decodePolygon.invoke(
      null,
      Json.createReader(new ByteArrayInputStream("[\"notAnArray\"]".getBytes())).readArray()
    ));
  }

  @Test
  public void testDecodeShape() throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
    Method decodeShape = JsonImageDetectionDecoder.class.getDeclaredMethod("decodeShape", JsonValue.class);
    decodeShape.setAccessible(true);
    assertNull(decodeShape.invoke(
      null,
      Json.createReader(new ByteArrayInputStream("{\"type\":\"Point\"}".getBytes())).readObject()
    ));
    assertNull(decodeShape.invoke(
      null,
      Json.createReader(new ByteArrayInputStream("{\"type\":\"Polygon\"}".getBytes())).readObject()
    ));
    assertNull(decodeShape.invoke(
      null,
      Json.createReader(new ByteArrayInputStream("{\"type\":\"Polygon\", \"coordinates\":[]}".getBytes())).readObject()
    ));
  }

  @Test
  public void testDecodeSimpleShape() throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
    Method decodeSimplePolygon = JsonImageDetectionDecoder.class.getDeclaredMethod("decodeSimplePolygon", JsonArray.class);
    decodeSimplePolygon.setAccessible(true);
    assertNull(decodeSimplePolygon.invoke(
      null,
      Json.createReader(new ByteArrayInputStream("[\"notAnArray\"]".getBytes())).readArray()
    ));
  }

  @Test
  public void testUtilityClass() {
    TestUtil.testUtilityClass(JsonImageDetectionDecoder.class);
  }
}
