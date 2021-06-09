// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import org.junit.jupiter.api.Test;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.data.osm.IWay;
import org.openstreetmap.josm.data.vector.VectorWay;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.MapillaryURLWireMock;
import org.openstreetmap.josm.plugins.mapillary.utils.JsonUtil;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillarySequenceUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.JsonReader;
import javax.json.JsonValue;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.util.Collection;
import java.util.function.Function;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.fail;
import static org.openstreetmap.josm.plugins.mapillary.utils.api.JsonDecoderTest.assertDecodesToNull;

@MapillaryURLWireMock
class JsonSequencesDecoderTest {

  @Test
  void testDecodeSequences() throws IOException {
    try (InputStream stream = this.getClass().getResourceAsStream("/api/v4/responses/searchSequences.json");
      JsonReader reader = Json.createReader(stream)) {
      // TODO fix
      Collection<VectorWay> exampleSequences = null; // JsonDecoder.decodeFeatureCollection(reader.readObject(),
                                                     // JsonSequencesDecoder::decodeSequence);
      assertNotNull(exampleSequences);
      assertEquals(1, exampleSequences.size());
      IWay<?> seq = exampleSequences.iterator().next();
      assertEquals(4, seq.getNodes().size());

      assertEquals("LwrHXqFRN_pszCopTKHF_Q", MapillaryImageUtils.getKey(seq.getNodes().get(0)));
      assertEquals("Aufjv2hdCKwg9LySWWVSwg", MapillaryImageUtils.getKey(seq.getNodes().get(1)));
      assertEquals("QEVZ1tp-PmrwtqhSwdW9fQ", MapillaryImageUtils.getKey(seq.getNodes().get(2)));
      assertEquals("G_SIwxNcioYeutZuA8Rurw", MapillaryImageUtils.getKey(seq.getNodes().get(3)));

      assertEquals(323.0319999999999, MapillaryImageUtils.getAngle(seq.getNodes().get(0)), 1e-10);
      assertEquals(320.8918, MapillaryImageUtils.getAngle(seq.getNodes().get(1)), 1e-10);
      assertEquals(333.62239999999997, MapillaryImageUtils.getAngle(seq.getNodes().get(2)), 1e-10);
      assertEquals(329.94820000000004, MapillaryImageUtils.getAngle(seq.getNodes().get(3)), 1e-10);

      assertEqualsLatLon(new LatLon(7.246497, 16.432958), seq.getNodes().get(0).getCoor());
      assertEqualsLatLon(new LatLon(7.246567, 16.432955), seq.getNodes().get(1).getCoor());
      assertEqualsLatLon(new LatLon(7.248372, 16.432971), seq.getNodes().get(2).getCoor());
      assertEqualsLatLon(new LatLon(7.249027, 16.432976), seq.getNodes().get(3).getCoor());

      assertEquals(1_457_963_093_860L, MapillarySequenceUtils.getCreatedAt(seq).toEpochMilli()); // 2016-03-14T13:44:53.860
                                                                                                 // UTC
    }
  }

  private void assertEqualsLatLon(LatLon expected, LatLon actual) {
    assertEquals(expected.lat(), actual.lat(), 0.000_000_1);
    assertEquals(expected.lon(), actual.lon(), 0.000_000_1);
  }

  @Test
  void testDecodeSequencesInvalid() {
    // null input
    assertEquals(0, JsonDecoder.decodeFeatureCollection(null, JsonSequencesDecoder::decodeSequence).size());
    // empty object
    assertNumberOfDecodedSequences(0, "{}");
    // object without type=FeatureCollection
    assertNumberOfDecodedSequences(0, "{\"features\": []}");
    // object with wrong value for the type attribute
    assertNumberOfDecodedSequences(0, "{\"type\": \"SomethingArbitrary\", \"features\": []}");
    // object without features-array
    assertNumberOfDecodedSequences(0, "{\"type\": \"FeatureCollection\"}");
    // object with a value for the features-key, but wrong type (in this case string instead of Array)
    assertNumberOfDecodedSequences(0, "{\"type\": \"FeatureCollection\", \"features\": \"notAnArray\"}");
  }

  @Test
  void testDecodeSequencesWithArbitraryObjectAsFeature() {
    assertNumberOfDecodedSequences(0, "{\"type\": \"FeatureCollection\", \"features\": [{}]}");
  }

  private static void assertNumberOfDecodedSequences(int expectedNumberOfSequences, String jsonString) {
    try (InputStream stream = new ByteArrayInputStream(jsonString.getBytes(StandardCharsets.UTF_8));
      JsonReader reader = Json.createReader(stream)) {
      assertEquals(expectedNumberOfSequences,
        JsonDecoder.decodeFeatureCollection(reader.readObject(), JsonSequencesDecoder::decodeSequence).size());
    } catch (IOException e) {
      fail(jsonString, e);
    }
  }

  @Test
  void testDecodeSequence() throws IOException {
    try (InputStream stream = this.getClass().getResourceAsStream("/api/v3/responses/sequence.json");
      JsonReader reader = Json.createReader(stream)) {
      IWay<?> exampleSequence = JsonSequencesDecoder.decodeSequence(reader.readObject()).get(0);
      assertEquals("cHBf9e8n0pG8O0ZVQHGFBQ", MapillarySequenceUtils.getKey(exampleSequence));
      // 1_457_963_077_206L -> 2016-03-14T13:44:37.206 UTC
      assertEquals(1_457_963_077_206L, MapillarySequenceUtils.getCreatedAt(exampleSequence).toEpochMilli());
      assertEquals(2, exampleSequence.getNodes().size());

      assertEquals(JsonImageDetailsDecoderTest.createDownloadedImage("76P0YUrlDD_lF6J7Od3yoA",
        new LatLon(16.43279, 7.246085), 96.71454, false), exampleSequence.getNodes().get(0));
      assertEquals(JsonImageDetailsDecoderTest.createDownloadedImage("Ap_8E0BwoAqqewhJaEbFyQ",
        new LatLon(16.432799, 7.246082), 96.47705000000002, false), exampleSequence.getNodes().get(1));
    }
  }

  @Test
  void testDecodeSequenceInvalid() {
    // null input
    assertNull(JsonSequencesDecoder.decodeSequence(null));
    // `properties` key is not set
    assertDecodesToNull(JsonSequencesDecoder::decodeSequence, "{\"type\": \"Feature\"}");
    // value for the key `key` in the properties is missing
    assertDecodesToNull(JsonSequencesDecoder::decodeSequence, "{\"type\": \"Feature\", \"properties\": {}}");
    // value for the key `user_key` in the properties is missing
    assertDecodesToNull(JsonSequencesDecoder::decodeSequence,
      "{\"type\": \"Feature\", \"properties\": {\"key\": \"someKey\"}}");
    // value for the key `captured_at` in the properties is missing
    assertDecodesToNull(JsonSequencesDecoder::decodeSequence,
      "{\"type\": \"Feature\", \"properties\": {\"key\": \"someKey\", \"user_key\": \"arbitraryUserKey\"}}");
    // the date in `captured_at` has an unrecognized format
    assertDecodesToNull(JsonSequencesDecoder::decodeSequence,
      "{\"type\": \"Feature\", \"properties\": {\"key\": \"someKey\", \"captured_at\": \"unrecognizedDateFormat\"}}");
    // the `image_key` array and the `cas` array contain unexpected values (in this case `null`)
    assertDecodesToNull(JsonSequencesDecoder::decodeSequence,
      "{\"type\": \"Feature\", \"properties\": {\"key\": \"someKey\", \"user_key\": \"arbitraryUserKey\",",
      "\"captured_at\": \"1970-01-01T00:00:00.000Z\",",
      "\"coordinateProperties\": {\"cas\": [null, null, null, null, 1.0, 1.0, 1.0],",
      "\"image_keys\": [null, null, \"key\", \"key\", null, null, \"key\"]}},",
      "\"geometry\": {\"type\": \"LineString\", \"coordinates\": [null, [1,1], null, [1,1], null, [1,1], null]}}");
  }

  /**
   * Checks if an empty array is returned, if <code>null</code> is supplied to the method as the array.
   */
  @Test
  void testDecodeJsonArray() throws ReflectiveOperationException {
    Method method = JsonSequencesDecoder.class.getDeclaredMethod("decodeJsonArray", JsonArray.class, Function.class,
      Class.class);
    method.setAccessible(true);
    assertEquals(0,
      ((String[]) method.invoke(null, null, (Function<JsonValue, String>) val -> null, String.class)).length);
  }

  @Test
  void testDecodeCoordinateProperty() throws ReflectiveOperationException {
    Method decodeCoordinateProperty = JsonSequencesDecoder.class.getDeclaredMethod("decodeCoordinateProperty",
      JsonObject.class, String.class, Function.class, Class.class);
    decodeCoordinateProperty.setAccessible(true);

    assertEquals(0, ((String[]) decodeCoordinateProperty.invoke(null, null, "key",
      (Function<JsonValue, String>) val -> "string", String.class)).length);

    assertEquals(0,
      ((String[]) decodeCoordinateProperty.invoke(null,
        JsonUtil.string2jsonObject("{\"coordinateProperties\":{\"key\":0}}"), "key",
        (Function<JsonValue, String>) val -> "string", String.class)).length);
  }

  @Test
  void testDecodeLatLons() throws ReflectiveOperationException, IOException {
    Method decodeLatLons = JsonSequencesDecoder.class.getDeclaredMethod("decodeLatLons", JsonObject.class);
    decodeLatLons.setAccessible(true);

    assertEquals(0, ((LatLon[]) decodeLatLons.invoke(null, (JsonObject) null)).length);
    assertEquals(0, ((LatLon[]) decodeLatLons.invoke(null, JsonUtil.string2jsonObject("{\"coordinates\":0}"))).length);
    assertEquals(0, ((LatLon[]) decodeLatLons.invoke(null, JsonUtil.string2jsonObject("{\"coordinates\":[]}"))).length);

    try (
      InputStream inputStream = new ByteArrayInputStream(
        "{\"type\": \"Feature\", \"coordinates\": []}".getBytes(StandardCharsets.UTF_8));
      JsonReader reader = Json.createReader(inputStream)) {
      assertEquals(0, ((LatLon[]) decodeLatLons.invoke(null, reader.readObject())).length);
    }

    try (
      InputStream inputStream = new ByteArrayInputStream(
        "{\"type\": \"LineString\", \"coordinates\": [ [1,2,3], [\"a\", 2], [1, \"b\"] ]}"
          .getBytes(StandardCharsets.UTF_8));
      JsonReader reader = Json.createReader(inputStream)) {
      LatLon[] example = (LatLon[]) decodeLatLons.invoke(null, reader.readObject());
      assertEquals(3, example.length);
      assertNull(example[0]);
      assertNull(example[1]);
      assertNull(example[2]);
    }
  }

  @Test
  void testUtilityClass() {
    TestUtil.testUtilityClass(JsonSequencesDecoder.class);
  }

}
