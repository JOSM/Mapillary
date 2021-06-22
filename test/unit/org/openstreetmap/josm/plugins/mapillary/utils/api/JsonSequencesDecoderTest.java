// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.RegisterExtension;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullSource;
import org.junit.jupiter.params.provider.ValueSource;

import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.data.osm.IWay;
import org.openstreetmap.josm.data.vector.VectorWay;
import org.openstreetmap.josm.plugins.mapillary.oauth.OAuthUtils;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.MapillaryURLWireMock;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillarySequenceUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil;
import org.openstreetmap.josm.testutils.JOSMTestRules;

import javax.json.Json;
import javax.json.JsonObject;
import javax.json.JsonReader;
import javax.json.JsonValue;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.openstreetmap.josm.plugins.mapillary.utils.api.JsonDecoderTest.stringToJsonValue;

@MapillaryURLWireMock
class JsonSequencesDecoderTest {
  @RegisterExtension
  static JOSMTestRules rules = new JOSMTestRules().preferences().main();

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
    final JsonObject json = OAuthUtils
      .getWithHeader(new URL(MapillaryURL.APIv4.getImagesBySequences("7nfcwfvjdtphz7yj6zat6a")));
    final Collection<VectorWay> exampleSequences = JsonDecoder.decodeData(json, JsonSequencesDecoder::decodeSequence);
    assertEquals(1, exampleSequences.size());
    final IWay<?> exampleSequence = exampleSequences.iterator().next();
    // Check that the nodes/ids were correctly decoded
    assertEquals("7nfcwfvjdtphz7yj6zat6a", MapillarySequenceUtils.getKey(exampleSequence));
    assertEquals("148137757289079", MapillaryImageUtils.getKey(exampleSequence.getNode(0)));
    assertEquals("311799370533334", MapillaryImageUtils.getKey(exampleSequence.getNode(1)));
    assertEquals("4235112816526838", MapillaryImageUtils.getKey(exampleSequence.getNode(2)));
    assertEquals("464249047982277", MapillaryImageUtils.getKey(exampleSequence.getNode(3)));
    assertEquals("308609047601518", MapillaryImageUtils.getKey(exampleSequence.getNode(4)));
    assertEquals("135511895288847", MapillaryImageUtils.getKey(exampleSequence.getNode(5)));
    assertEquals("311681117131457", MapillaryImageUtils.getKey(exampleSequence.getNode(6)));
    assertEquals(7, exampleSequence.getNodesCount());
    // 1_457_963_077_206L -> 2016-03-14T13:44:37.206 UTC
    assertEquals(1_457_963_077_206L, MapillarySequenceUtils.getCreatedAt(exampleSequence).toEpochMilli());
    assertEquals(2, exampleSequence.getNodes().size());

    assertEquals(JsonImageDetailsDecoderTest.createDownloadedImage("76P0YUrlDD_lF6J7Od3yoA",
      new LatLon(16.43279, 7.246085), 96.71454, false), exampleSequence.getNodes().get(0));
    assertEquals(JsonImageDetailsDecoderTest.createDownloadedImage("Ap_8E0BwoAqqewhJaEbFyQ",
      new LatLon(16.432799, 7.246082), 96.47705000000002, false), exampleSequence.getNodes().get(1));
  }

  @ParameterizedTest
  // null input
  @NullSource
  @ValueSource(strings = {
    /* *** Start new graph API checks */
    // Data is expected to be removed prior to the call. And this is an object
    "{\"data\":[{\"id\":\"148137757289079\"}]}",
    // ids should not be null
    "[{\"id\":null}]",
    // ids should not be dicts
    "[{\"id\":{\"bad\":5}}]",
    // ids should not be arrays
    "[{\"id\":[\"bad\",5]}]",
    // There should be an id...
    "[{\"random\":\"dict\"}]",
    // There should be dicts
    "[null]",
  /* *** End new graph API checks */
  })
  /**
   * Check various strings that are not valid sequences.
   */
  void testDecodeSequenceInvalid(final String toCheck) {
    final JsonValue json = toCheck != null ? stringToJsonValue(toCheck) : null;
    final List<VectorWay> seq = JsonSequencesDecoder.decodeSequence(json);
    assertTrue(seq.isEmpty(), seq.stream().map(VectorWay::getNodes).flatMap(Collection::stream)
      .map(MapillaryImageUtils::getKey).collect(Collectors.joining(",")));
  }

  @Test
  void testUtilityClass() {
    TestUtil.testUtilityClass(JsonSequencesDecoder.class);
  }

}
