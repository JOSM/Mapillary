// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;

import jakarta.json.Json;
import jakarta.json.JsonArray;
import jakarta.json.JsonObject;
import jakarta.json.JsonReader;
import jakarta.json.JsonValue;
import jakarta.json.JsonWriter;
import jakarta.json.JsonWriterFactory;
import jakarta.json.stream.JsonGenerator;

public final class JsonUtil {

    private static final Map<String, Object> prettyJsonWriterConfig = new HashMap<>();
    static {
        prettyJsonWriterConfig.put(JsonGenerator.PRETTY_PRINTING, true);
    }
    private static final JsonWriterFactory prettyJsonWriterFactory = Json.createWriterFactory(prettyJsonWriterConfig);

    private JsonUtil() {
        // Private constructor to avoid instantiation
    }

    private static void assertJsonArrayEquals(final JsonArray expected, final JsonArray actual, final String path) {
        assertEquals(expected.size(), actual.size(),
            errorMessage(String.format("JSON array length is different at %s", path), expected, actual));
        for (int i = 0; i < expected.size(); i++) {
            final JsonValue expectedValue = expected.get(i);
            final JsonValue actualValue = actual.get(i);
            assertEquals(expectedValue.getClass(), actualValue.getClass(), errorMessage(
                String.format("JSON values have different type at %s[%d]", path, i), expectedValue, actualValue));
            if (expectedValue instanceof JsonObject && actualValue instanceof JsonObject) {
                assertJsonObjectEquals((JsonObject) expectedValue, (JsonObject) actualValue, path + "[" + i + "]");
            }
            if (expectedValue instanceof JsonArray && actualValue instanceof JsonArray) {
                assertJsonArrayEquals((JsonArray) expectedValue, (JsonArray) actualValue, path + "[" + i + "]");
            }
        }
        assertEquals(expected, actual, errorMessage(String.format("JSON is different at %s", path), expected, actual));
    }

    private static void assertJsonObjectEquals(final JsonObject expected, final JsonObject actual, final String path) {
        assertEquals(expected.keySet(), actual.keySet(),
            errorMessage(String.format("JSON keys are different at %s", path), expected, actual));
        for (final String key : expected.keySet()) {
            final JsonValue expectedValue = expected.get(key);
            final JsonValue actualValue = actual.get(key);
            assertEquals(expectedValue.getClass(), actualValue.getClass(), errorMessage(
                String.format("JSON values have different type at %s/%s", path, key), expectedValue, actualValue));
            if (expectedValue instanceof JsonObject && actualValue instanceof JsonObject) {
                assertJsonObjectEquals((JsonObject) expectedValue, (JsonObject) actualValue, path + "/" + key);
            }
            if (expectedValue instanceof JsonArray && actualValue instanceof JsonArray) {
                assertJsonArrayEquals((JsonArray) expectedValue, (JsonArray) actualValue, path + "/" + key);
            }
        }

        assertEquals(expected, actual, String.format("JSON is different at %s", path));
    }

    private static String errorMessage(final String msg, final JsonValue expectedJson, final JsonValue actualJson) {
        return String.format("%s\n\nExpected:\n%s\n\nActual:\n%s\n\n", msg, prettify(expectedJson),
            prettify(actualJson));
    }

    public static String prettify(final JsonValue jsonObject) {
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        try (JsonWriter writer = prettyJsonWriterFactory.createWriter(baos)) {
            writer.write(jsonObject);
            return new String(baos.toByteArray(), StandardCharsets.UTF_8);
        }
    }

    public static JsonObject string2jsonObject(String s) {
        try (ByteArrayInputStream stream = new ByteArrayInputStream(s.getBytes(StandardCharsets.UTF_8));
            JsonReader reader = Json.createReader(stream)) {
            return reader.readObject();
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }
}
