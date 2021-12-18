// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.openstreetmap.josm.plugins.mapillary.utils.api.JsonDecoderTest.stringToJsonValue;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import javax.json.Json;
import javax.json.JsonObject;
import javax.json.JsonReader;
import javax.json.JsonValue;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.RegisterExtension;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.openstreetmap.josm.data.osm.IPrimitive;
import org.openstreetmap.josm.data.osm.IWay;
import org.openstreetmap.josm.data.vector.VectorWay;
import org.openstreetmap.josm.plugins.mapillary.oauth.OAuthUtils;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.MapillaryCaches;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.MapillaryURLWireMock;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.MapillaryURLWireMockErrors;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil;
import org.openstreetmap.josm.testutils.JOSMTestRules;

@MapillaryCaches
@MapillaryURLWireMock
class JsonSequencesDecoderTest {
    @RegisterExtension
    static JOSMTestRules rules = new JOSMTestRules().main();

    @Test
    void testDecodeSequencesInvalid() {
        // null input
        assertThrows(NullPointerException.class,
            () -> JsonDecoder.decodeData(null, JsonSequencesDecoder::decodeSequence));
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
                JsonDecoder.decodeData(reader.readObject(), JsonSequencesDecoder::decodeSequence).size());
        } catch (IOException e) {
            fail(jsonString, e);
        }
    }

    @Test
    @Disabled("JOSM Jenkins server throws IOException (#21121)")
    void testDecodeSequence() throws IOException {
        final JsonObject json = OAuthUtils
            .getWithHeader(new URL(MapillaryURL.APIv4.getImagesBySequences("7nfcwfvjdtphz7yj6zat6a")));
        final Collection<VectorWay> exampleSequences = JsonDecoder.decodeData(json,
            JsonSequencesDecoder::decodeSequence);
        assertEquals(1, exampleSequences.size());
        final IWay<?> exampleSequence = exampleSequences.iterator().next();
        // Since the sequence key isn't returned in the API response, we have to rely upon the sequence key
        // being present in the vector tiles. Therefore, we cannot test that the expected sequence id is in the
        // vector way. We also cannot check anything _except_ that the sequence now has data.

        // Check that the nodes/ids were correctly decoded
        assertEquals(148137757289079L, MapillaryImageUtils.getKey(exampleSequence.getNode(0)));
        assertEquals(311799370533334L, MapillaryImageUtils.getKey(exampleSequence.getNode(1)));
        assertEquals(338231874314914L, MapillaryImageUtils.getKey(exampleSequence.getNode(2)));
        assertEquals(4235112816526838L, MapillaryImageUtils.getKey(exampleSequence.getNode(3)));
        assertEquals(464249047982277L, MapillaryImageUtils.getKey(exampleSequence.getNode(4)));
        assertEquals(308609047601518L, MapillaryImageUtils.getKey(exampleSequence.getNode(5)));
        assertEquals(135511895288847L, MapillaryImageUtils.getKey(exampleSequence.getNode(6)));
        assertEquals(311681117131457L, MapillaryImageUtils.getKey(exampleSequence.getNode(7)));
        assertEquals(8, exampleSequence.getNodesCount());
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
    @MapillaryURLWireMockErrors(MapillaryURLWireMockErrors.Type.SERVER_ERROR)
    void testDecodeSequenceInvalid(final String toCheck) {
        final JsonValue json = toCheck != null ? stringToJsonValue(toCheck) : null;
        final List<VectorWay> seq = JsonSequencesDecoder.decodeSequence(json);
        assertTrue(seq.isEmpty(), seq.stream().map(VectorWay::getNodes).flatMap(Collection::stream)
            .mapToLong(IPrimitive::getUniqueId).mapToObj(Long::toString).collect(Collectors.joining(",")));
    }

    @Test
    void testUtilityClass() {
        TestUtil.testUtilityClass(JsonSequencesDecoder.class);
    }

}
