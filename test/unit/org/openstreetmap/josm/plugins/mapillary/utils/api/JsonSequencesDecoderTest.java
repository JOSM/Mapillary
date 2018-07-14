// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import static org.openstreetmap.josm.plugins.mapillary.utils.api.JsonDecoderTest.assertDecodesToNull;

import java.io.ByteArrayInputStream;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.util.Collection;
import java.util.function.Function;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.JsonValue;

import org.junit.Test;

import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.MapillarySequence;
import org.openstreetmap.josm.plugins.mapillary.utils.JsonUtil;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil;

public class JsonSequencesDecoderTest {

  @Test
  public void testDecodeSequences() {
    Collection<MapillarySequence> exampleSequences = JsonDecoder.decodeFeatureCollection(
      Json.createReader(this.getClass().getResourceAsStream("/api/v3/responses/searchSequences.json")).readObject(),
      JsonSequencesDecoder::decodeSequence
    );
    assertNotNull(exampleSequences);
    assertEquals(1, exampleSequences.size());
    MapillarySequence seq = exampleSequences.iterator().next();
    assertEquals(4, seq.getImages().size());

    assertEquals("LwrHXqFRN_pszCopTKHF_Q", ((MapillaryImage) seq.getImages().get(0)).getKey());
    assertEquals("Aufjv2hdCKwg9LySWWVSwg", ((MapillaryImage) seq.getImages().get(1)).getKey());
    assertEquals("QEVZ1tp-PmrwtqhSwdW9fQ", ((MapillaryImage) seq.getImages().get(2)).getKey());
    assertEquals("G_SIwxNcioYeutZuA8Rurw", ((MapillaryImage) seq.getImages().get(3)).getKey());

    assertEquals(323.0319999999999, seq.getImages().get(0).getCa(), 1e-10);
    assertEquals(320.8918, seq.getImages().get(1).getCa(), 1e-10);
    assertEquals(333.62239999999997, seq.getImages().get(2).getCa(), 1e-10);
    assertEquals(329.94820000000004, seq.getImages().get(3).getCa(), 1e-10);

    assertEquals(new LatLon(7.246497, 16.432958),  seq.getImages().get(0).getLatLon());
    assertEquals(new LatLon(7.246567, 16.432955),  seq.getImages().get(1).getLatLon());
    assertEquals(new LatLon(7.248372, 16.432971),  seq.getImages().get(2).getLatLon());
    assertEquals(new LatLon(7.249027, 16.432976),  seq.getImages().get(3).getLatLon());

    assertEquals(1_457_963_093_860L, seq.getCapturedAt()); // 2016-03-14T13:44:53.860 UTC
  }

  @Test
  public void testDecodeSequencesInvalid() {
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
  public void testDecodeSequencesWithArbitraryObjectAsFeature() {
    assertNumberOfDecodedSequences(0, "{\"type\": \"FeatureCollection\", \"features\": [{}]}");
  }

  private static void assertNumberOfDecodedSequences(int expectedNumberOfSequences, String jsonString) {
    assertEquals(
      expectedNumberOfSequences,
      JsonDecoder.decodeFeatureCollection(
        Json.createReader(new ByteArrayInputStream(jsonString.getBytes(StandardCharsets.UTF_8))).readObject(),
        JsonSequencesDecoder::decodeSequence
      ).size()
    );
  }

  @Test
  public void testDecodeSequence() {
    MapillarySequence exampleSequence = JsonSequencesDecoder.decodeSequence(
      Json.createReader(this.getClass().getResourceAsStream("/api/v3/responses/sequence.json")).readObject()
    );
    assertEquals("cHBf9e8n0pG8O0ZVQHGFBQ", exampleSequence.getKey());
    assertEquals(1_457_963_077_206L, exampleSequence.getCapturedAt()); // 2016-03-14T13:44:37.206 UTC
    assertEquals(2, exampleSequence.getImages().size());

    assertEquals(
      new MapillaryImage("76P0YUrlDD_lF6J7Od3yoA", new LatLon(16.43279, 7.246085), 96.71454, false),
      exampleSequence.getImages().get(0)
    );
    assertEquals(
      new MapillaryImage("Ap_8E0BwoAqqewhJaEbFyQ", new LatLon(16.432799, 7.246082), 96.47705000000002, false),
      exampleSequence.getImages().get(1)
    );
  }

  @Test
  public void testDecodeSequenceInvalid() {
    // null input
    assertNull(JsonSequencesDecoder.decodeSequence(null));
    // `properties` key is not set
    assertDecodesToNull(JsonSequencesDecoder::decodeSequence, "{\"type\": \"Feature\"}");
    // value for the key `key` in the properties is missing
    assertDecodesToNull(JsonSequencesDecoder::decodeSequence,
      "{\"type\": \"Feature\", \"properties\": {}}"
    );
    // value for the key `user_key` in the properties is missing
    assertDecodesToNull(
      JsonSequencesDecoder::decodeSequence,
      "{\"type\": \"Feature\", \"properties\": {\"key\": \"someKey\"}}"
    );
    // value for the key `captured_at` in the properties is missing
    assertDecodesToNull(
      JsonSequencesDecoder::decodeSequence,
      "{\"type\": \"Feature\", \"properties\": {\"key\": \"someKey\", \"user_key\": \"arbitraryUserKey\"}}"
    );
    // the date in `captured_at` has an unrecognized format
    assertDecodesToNull(
      JsonSequencesDecoder::decodeSequence,
      "{\"type\": \"Feature\", \"properties\": {\"key\": \"someKey\", \"captured_at\": \"unrecognizedDateFormat\"}}"
    );
    // the `image_key` array and the `cas` array contain unexpected values (in this case `null`)
    assertDecodesToNull(
      JsonSequencesDecoder::decodeSequence,
      "{\"type\": \"Feature\", \"properties\": {\"key\": \"someKey\", \"user_key\": \"arbitraryUserKey\",",
      "\"captured_at\": \"1970-01-01T00:00:00.000Z\",",
      "\"coordinateProperties\": {\"cas\": [null, null, null, null, 1.0, 1.0, 1.0],",
      "\"image_keys\": [null, null, \"key\", \"key\", null, null, \"key\"]}},",
      "\"geometry\": {\"type\": \"LineString\", \"coordinates\": [null, [1,1], null, [1,1], null, [1,1], null]}}"
    );
  }

  /**
   * Checks if an empty array is returned, if <code>null</code> is supplied to the method as the array.
   */
  @Test
  public void testDecodeJsonArray()
      throws NoSuchMethodException, SecurityException, IllegalAccessException,
      IllegalArgumentException, InvocationTargetException {
    Method method = JsonSequencesDecoder.class.getDeclaredMethod("decodeJsonArray", JsonArray.class, Function.class, Class.class);
    method.setAccessible(true);
    assertEquals(
      0,
      ((String[]) method.invoke(null, null, (Function<JsonValue, String>) val -> null, String.class)).length
    );
  }

  @Test
  public void testDecodeCoordinateProperty() throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
    Method decodeCoordinateProperty = JsonSequencesDecoder.class.getDeclaredMethod(
      "decodeCoordinateProperty",
      JsonObject.class,
      String.class,
      Function.class,
      Class.class
    );
    decodeCoordinateProperty.setAccessible(true);

    assertEquals(
      0,
      ((String[]) decodeCoordinateProperty.invoke(
        null, null, "key", (Function<JsonValue, String>) val -> "string", String.class
      )).length
    );

    assertEquals(
      0,
      ((String[]) decodeCoordinateProperty.invoke(
        null, JsonUtil.string2jsonObject("{\"coordinateProperties\":{\"key\":0}}"), "key", (Function<JsonValue, String>) val -> "string", String.class
      )).length
    );
  }

  @Test
  public void testDecodeLatLons()
      throws NoSuchMethodException, SecurityException, IllegalAccessException,
      IllegalArgumentException, InvocationTargetException {
    Method decodeLatLons = JsonSequencesDecoder.class.getDeclaredMethod("decodeLatLons", JsonObject.class);
    decodeLatLons.setAccessible(true);

    assertEquals(0, ((LatLon[]) decodeLatLons.invoke(null, (JsonObject) null)).length);
    assertEquals(0, ((LatLon[]) decodeLatLons.invoke(null, JsonUtil.string2jsonObject("{\"coordinates\":0}"))).length);
    assertEquals(0, ((LatLon[]) decodeLatLons.invoke(null, JsonUtil.string2jsonObject("{\"coordinates\":[]}"))).length);

    assertEquals(0, ((LatLon[]) decodeLatLons.invoke(null, Json.createReader(new ByteArrayInputStream(
      "{\"type\": \"Feature\", \"coordinates\": []}".getBytes(StandardCharsets.UTF_8)
    )).readObject())).length);

    LatLon[] example = (LatLon[]) decodeLatLons.invoke(null, Json.createReader(new ByteArrayInputStream(
      "{\"type\": \"LineString\", \"coordinates\": [ [1,2,3], [\"a\", 2], [1, \"b\"] ]}".getBytes(StandardCharsets.UTF_8)
    )).readObject());
    assertEquals(3, example.length);
    assertNull(example[0]);
    assertNull(example[1]);
    assertNull(example[2]);
  }

  @Test
  public void testUtilityClass() {
    TestUtil.testUtilityClass(JsonSequencesDecoder.class);
  }

}
