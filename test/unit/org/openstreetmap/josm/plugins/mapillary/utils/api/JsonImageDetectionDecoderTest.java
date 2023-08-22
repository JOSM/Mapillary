// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;

import java.io.ByteArrayInputStream;
import java.nio.charset.StandardCharsets;

import jakarta.json.Json;
import jakarta.json.JsonReader;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.MapillaryCaches;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.MapillaryURLWireMock;

@MapillaryCaches
@MapillaryURLWireMock
class JsonImageDetectionDecoderTest {

    /**
     * This is a non-regression test for JOSM #21254
     *
     * @param geometry The geometry to check
     */
    @ParameterizedTest
    @ValueSource(strings = { "GiB4AgoGbXB5LW9yKIAgEhEYAwgBIgsJxheqLxIEFQAeDw==" })
    void testDecodeImageDetectionGeometryNonRegression21254(final String geometry) {
        final String jsonString = "{\"value\":\"construction--barrier--concrete-block\",\"id\":\"1\",\"geometry\":\""
            + geometry + "\",\"image\":{\"id\":\"1\"}}";
        try (JsonReader jsonReader = Json
            .createReader(new ByteArrayInputStream(jsonString.getBytes(StandardCharsets.UTF_8)))) {
            assertDoesNotThrow(() -> JsonImageDetectionDecoder.decodeImageDetection(jsonReader.read(), null));
        }
    }
}
