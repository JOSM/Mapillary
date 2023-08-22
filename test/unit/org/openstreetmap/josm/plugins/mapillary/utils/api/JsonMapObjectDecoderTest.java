// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.util.Collection;
import java.util.stream.LongStream;
import java.util.stream.Stream;

import jakarta.json.Json;
import jakarta.json.JsonObject;
import jakarta.json.JsonReader;
import jakarta.json.JsonValue;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.NullSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.data.vector.VectorNode;
import org.openstreetmap.josm.data.vector.VectorPrimitive;
import org.openstreetmap.josm.data.vector.VectorWay;
import org.openstreetmap.josm.plugins.mapillary.oauth.OAuthUtils;
import org.openstreetmap.josm.plugins.mapillary.spi.preferences.MapillaryConfig;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.MapillaryLayerAnnotation;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.MapillaryURLWireMock;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.MapillaryURLWireMockErrors;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryMapFeatureUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil;
import org.openstreetmap.josm.testutils.annotations.BasicPreferences;
import org.openstreetmap.josm.testutils.annotations.HTTP;

@BasicPreferences
@HTTP
@MapillaryLayerAnnotation
@MapillaryURLWireMock
class JsonMapObjectDecoderTest {

    @Test
    void testDecodeMapObject() throws IOException {
        final JsonObject jsonObject = OAuthUtils.getWithHeader(URI.create(MapillaryConfig.getUrls()
            .getMapFeatureInformation(496980935069177L, MapillaryMapFeatureUtils.MapFeatureProperties.values())));
        final VectorNode node1 = new VectorNode("test");
        Collection<VectorPrimitive> exampleMapObjects = JsonDecoder.decodeData(jsonObject,
            json -> JsonMapObjectDecoder.decodeMapFeatureObject(json, node1));
        assertEquals(1, exampleMapObjects.size());

        final VectorPrimitive exampleMapObject = exampleMapObjects.iterator().next();
        assertSame(node1, exampleMapObject);
        // 2017-08-23T21:32:26 UTC -- both are currently the same time
        assertEquals(1_503_523_946L, MapillaryMapFeatureUtils.getFirstSeenAt(node1).getEpochSecond());
        assertEquals(1_503_523_946L, MapillaryMapFeatureUtils.getLastSeenAt(node1).getEpochSecond());
        assertEquals("complementary--both-directions--g2", MapillaryMapFeatureUtils.getValue(node1));
        assertEquals(496980935069177L, node1.getUniqueId());
        assertEquals(new LatLon(41.341166490122, -83.417193328459), node1.getCoor());

        final long[] images = MapillaryMapFeatureUtils.getImageIds(node1);
        assertTrue(LongStream.of(images).anyMatch(l -> l == 828_719_391_332_432L));
        assertTrue(LongStream.of(images).anyMatch(l -> l == 512_146_709_982_392L));
        assertTrue(LongStream.of(images).anyMatch(l -> l == 133_075_088_816_573L));

        // Note: The layer does not get updated at this time. Change this if it does.
        assertNotEquals("trafficsign", exampleMapObject.getLayer());
    }

    @Test
    void testNoOverwriteFromInitialVector() throws IOException {
        final JsonObject jsonObject = OAuthUtils.getWithHeader(URI.create(MapillaryConfig.getUrls()
            .getMapFeatureInformation(496980935069177L, MapillaryMapFeatureUtils.MapFeatureProperties.values())));
        final VectorNode node1 = new VectorNode("test");
        node1.setCoor(new LatLon(12, 2));
        node1.put(MapillaryMapFeatureUtils.MapFeatureProperties.FIRST_SEEN_AT.toString(),
            Long.toString(1_000_000_000L));
        node1.put(MapillaryMapFeatureUtils.MapFeatureProperties.LAST_SEEN_AT.toString(), Long.toString(2_000_000_000L));
        node1.put(MapillaryMapFeatureUtils.MapFeatureProperties.VALUE.toString(), "test--g1");
        Collection<VectorPrimitive> exampleMapObjects = JsonDecoder.decodeData(jsonObject,
            json -> JsonMapObjectDecoder.decodeMapFeatureObject(json, node1));
        assertEquals(1, exampleMapObjects.size());
        assertEquals(0,
            JsonDecoder.decodeData(jsonObject, json -> JsonMapObjectDecoder.decodeMapFeatureObject(json, null)).size());

        final VectorPrimitive exampleMapObject = exampleMapObjects.iterator().next();
        assertSame(node1, exampleMapObject);

        assertEquals(1_000_000_000L, MapillaryMapFeatureUtils.getFirstSeenAt(node1).toEpochMilli()); // 2017-08-23T21:32:26
                                                                                                     // UTC
        assertEquals(2_000_000_000L, MapillaryMapFeatureUtils.getLastSeenAt(node1).toEpochMilli()); // 2017-08-23T21:32:34
                                                                                                    // UTC
        assertEquals("complementary--both-directions--g2", MapillaryMapFeatureUtils.getValue(node1));
        assertEquals(496980935069177L,
            Long.parseLong(node1.get(MapillaryMapFeatureUtils.MapFeatureProperties.ID.toString())));
        assertEquals(new LatLon(41.341166490122, -83.417193328459), node1.getCoor());

        final long[] images = MapillaryMapFeatureUtils.getImageIds(node1);
        assertTrue(LongStream.of(images).anyMatch(l -> l == 828_719_391_332_432L));
        assertTrue(LongStream.of(images).anyMatch(l -> l == 512_146_709_982_392L));
        assertTrue(LongStream.of(images).anyMatch(l -> l == 133_075_088_816_573L));

        // Note: The layer does not get updated at this time. Change this if it does.
        assertNotEquals("trafficsign", exampleMapObject.getLayer());
    }

    @ParameterizedTest
    @NullSource
    @ValueSource(strings = { "[]", "{}" })
    void testDecodeMapObjectInvalid(final String json) {
        final VectorNode vectorNode = new VectorNode("test");
        if (json != null) {
            try (JsonReader reader = Json
                .createReader(new ByteArrayInputStream(json.getBytes(StandardCharsets.UTF_8)))) {
                final JsonValue jsonValue = reader.readValue();
                assertDoesNotThrow(() -> JsonMapObjectDecoder.decodeMapFeatureObject(jsonValue, vectorNode));
            }
        } else {
            JsonMapObjectDecoder.decodeMapFeatureObject(null, vectorNode);
        }
        assertFalse(vectorNode.getCoor().isValid());
        assertEquals("test", vectorNode.getLayer());
        assertTrue(vectorNode.getKeys().isEmpty());
    }

    static Stream<Arguments> testGeometrySource() {
        return Stream.of(Arguments.of(new LatLon(0, 0), "{\"geometry\":{\"type\":\"Point\",\"coordinates\":[0, 0]}}"),
            Arguments.of(new LatLon(1, 0), "{\"geometry\":{\"type\":\"Point\",\"coordinates\":[0, 1]}}"),
            Arguments.of(new LatLon(0, 1), "{\"geometry\":{\"type\":\"Point\",\"coordinates\":[1, 0]}}"),
            Arguments.of(new LatLon(-30, -20), "{\"geometry\":{\"type\":\"Point\",\"coordinates\":[-20, -30]}}"));
    }

    @ParameterizedTest
    @MethodSource("testGeometrySource")
    void testGeometry(final LatLon expected, final String jsonString) {
        final VectorNode testNode = new VectorNode("test");
        try (JsonReader reader = Json
            .createReader(new ByteArrayInputStream(jsonString.getBytes(StandardCharsets.UTF_8)))) {
            JsonMapObjectDecoder.decodeMapFeatureObject(reader.readValue(), testNode);
        }
        assertEquals(expected, testNode.getCoor());
    }

    @ParameterizedTest
    @ValueSource(strings = { "{\"geometry\":{\"type\":\"Point\",\"coordinates\":{}}}",
        "{\"geometry\":{\"type\":\"Point\",\"coordinates\":[0]}}",
        "{\"geometry\":{\"type\":\"Point\",\"bad_coordinates\":[0, 0]}}",
        "{\"geometry\":{\"type\":\"Point\",\"coordinates\":[0, 0, 0]}}",
        "{\"geometry\":{\"type\":\"Point\",\"coordinates\":[[0, 0], [1, 1]]}}",
        "{\"geometry\":{\"type\":\"LineString\",\"coordinates\":[[0, 0], [1, 1]]}}",
        "{\"geometry\":{\"type\":\"LineString\",\"coordinates\":[[0, 0]]}}" })
    void testBadGeometryNode(final String jsonString) {
        final VectorNode testNode = new VectorNode("test");
        try (JsonReader reader = Json
            .createReader(new ByteArrayInputStream(jsonString.getBytes(StandardCharsets.UTF_8)))) {
            final JsonValue jsonValue = reader.readValue();
            assertThrows(IllegalArgumentException.class,
                () -> JsonMapObjectDecoder.decodeMapFeatureObject(jsonValue, testNode));
        }
    }

    @ParameterizedTest
    @ValueSource(strings = { "{\"geometry\":{\"type\":\"Point\",\"coordinates\":{}}}",
        "{\"geometry\":{\"type\":\"Point\",\"coordinates\":[0]}}",
        "{\"geometry\":{\"type\":\"Point\",\"bad_coordinates\":[0, 0]}}",
        "{\"geometry\":{\"type\":\"Point\",\"coordinates\":[0, 0, 0]}}",
        "{\"geometry\":{\"type\":\"Point\",\"coordinates\":[[0, 0], [1, 1]]}}",
        "{\"geometry\":{\"type\":\"LineString\",\"coordinates\":[[0, 0], [1, 1, 1]]}}",
        "{\"geometry\":{\"type\":\"LineString\",\"coordinates\":[[0, 0]]}}" })
    void testBadGeometryWay(final String jsonString) {
        final VectorWay testWay = new VectorWay("test");
        try (JsonReader reader = Json
            .createReader(new ByteArrayInputStream(jsonString.getBytes(StandardCharsets.UTF_8)))) {
            final JsonValue jsonValue = reader.readValue();
            assertThrows(IllegalArgumentException.class,
                () -> JsonMapObjectDecoder.decodeMapFeatureObject(jsonValue, testWay));
        }
    }

    @ParameterizedTest
    @ValueSource(strings = { "{\"images\":{}}", "{\"images\":[{\"id\":\"8\"}]}", "{\"images\":{\"data\":[]}}",
        "{\"images\":{\"data\":[{\"tid\":\"8\"}]}}", "{\"images\":{\"data\":[8]}}", })
    @MapillaryURLWireMockErrors(MapillaryURLWireMockErrors.Type.NOT_FOUND)
    void testBadImages(final String jsonString) {
        final VectorNode testNode = new VectorNode("test");
        try (JsonReader reader = Json
            .createReader(new ByteArrayInputStream(jsonString.getBytes(StandardCharsets.UTF_8)))) {
            assertDoesNotThrow(() -> JsonMapObjectDecoder.decodeMapFeatureObject(reader.readValue(), testNode));
        }
        assertEquals(0, MapillaryMapFeatureUtils.getImageIds(testNode).length);
    }

    @Test
    void testUtilityClass() {
        TestUtil.testUtilityClass(JsonMapObjectDecoder.class);
    }
}
