// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.openstreetmap.josm.plugins.mapillary.utils.api.JsonDecoderTest.stringToJsonValue;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.util.Collection;
import java.util.stream.Collectors;

import jakarta.json.Json;
import jakarta.json.JsonObject;
import jakarta.json.JsonReader;
import jakarta.json.JsonValue;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.RegisterExtension;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.openstreetmap.josm.plugins.mapillary.oauth.OAuthUtils;
import org.openstreetmap.josm.plugins.mapillary.spi.preferences.MapillaryConfig;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.MapillaryCaches;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.MapillaryURLWireMock;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.MapillaryURLWireMockErrors;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil;
import org.openstreetmap.josm.testutils.JOSMTestRules;

@MapillaryCaches
@MapillaryURLWireMock
class JsonSequencesDecoderTest {
    @RegisterExtension
    static JOSMTestRules rules = new JOSMTestRules().main();

    @Test
    void testDecodeSequencesInvalid() throws IOException {
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
    void testDecodeSequencesWithArbitraryObjectAsFeature() throws IOException {
        assertNumberOfDecodedSequences(0, "{\"type\": \"FeatureCollection\", \"features\": [{}]}");
    }

    private static void assertNumberOfDecodedSequences(int expectedNumberOfSequences, String jsonString)
        throws IOException {
        try (InputStream stream = new ByteArrayInputStream(jsonString.getBytes(StandardCharsets.UTF_8));
            JsonReader reader = Json.createReader(stream)) {
            assertEquals(expectedNumberOfSequences,
                JsonDecoder.decodeData(reader.readObject(), JsonSequencesDecoder::decodeSequence).size());
        }
    }

    @Test
    void testDecodeSequence() throws IOException {
        final JsonObject json = OAuthUtils
            .getWithHeader(URI.create(MapillaryConfig.getUrls().getImagesBySequences("7nfcwfvjdtphz7yj6zat6a")));
        final Collection<Long> exampleSequences = JsonDecoder.decodeData(json, JsonSequencesDecoder::decodeSequence);
        // Since the sequence key isn't returned in the API response, we have to rely upon the sequence key
        // being present in the vector tiles. Therefore, we cannot test that the expected sequence id is in the
        // vector way. We also cannot check anything _except_ that the sequence now has data.

        // Check that the nodes/ids were correctly decoded
        final long[] expectedArray = { 148137757289079L, 311799370533334L, 338231874314914L, 4235112816526838L,
            464249047982277L, 308609047601518L, 135511895288847L, 311681117131457L };
        final long[] actualArray = exampleSequences.stream().mapToLong(Long::longValue).toArray();
        assertArrayEquals(expectedArray, actualArray);
    }

    /**
     * Check various strings that are not valid sequences.
     */
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
    @MapillaryURLWireMockErrors(MapillaryURLWireMockErrors.Type.SERVER_ERROR)
    void testDecodeSequenceInvalid(final String toCheck) {
        final JsonValue json = toCheck != null ? stringToJsonValue(toCheck) : null;
        final Collection<Long> seq = JsonSequencesDecoder.decodeSequence(json);
        assertTrue(seq.isEmpty(),
            seq.stream().mapToLong(Long::longValue).mapToObj(Long::toString).collect(Collectors.joining(",")));
    }

    @Test
    void testUtilityClass() {
        TestUtil.testUtilityClass(JsonSequencesDecoder.class);
    }

}
