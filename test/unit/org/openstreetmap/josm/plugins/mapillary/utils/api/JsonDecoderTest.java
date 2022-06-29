// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.function.Function;

import javax.json.Json;
import javax.json.JsonReader;
import javax.json.JsonValue;

import org.junit.jupiter.api.Test;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil;

class JsonDecoderTest {

    @Test
    void testUtilityClass() {
        TestUtil.testUtilityClass(JsonDecoder.class);
    }

    @Test
    void testDecodeDoublePair() {
        assertArrayEquals(new double[0], JsonDecoder.decodeDoublePair(null));
    }

    /**
     * Convert a series of strings to JSON
     *
     * @param parts The string parts to convert
     * @return A json value
     */
    static JsonValue stringToJsonValue(final String... parts) {
        return stringToJsonValue(JsonReader::readValue, parts);
    }

    /**
     * Convert a series of strings to JSON
     *
     * @param parts The string parts to convert
     * @param function The conversion function
     * @param <T> The type to return
     * @return A json value
     */
    static <T extends JsonValue> T stringToJsonValue(final Function<JsonReader, T> function, final String... parts) {
        try (
            ByteArrayInputStream stream = new ByteArrayInputStream(
                String.join(" ", parts).getBytes(StandardCharsets.UTF_8));
            JsonReader reader = Json.createReader(stream)) {
            return function.apply(reader);
        } catch (IOException e) {
            fail(e);
            return null;
        }
    }

}
