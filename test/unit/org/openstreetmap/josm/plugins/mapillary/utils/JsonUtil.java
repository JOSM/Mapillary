// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import static org.junit.Assert.assertEquals;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;
import javax.json.JsonValue;
import javax.json.JsonWriter;
import javax.json.JsonWriterFactory;
import javax.json.stream.JsonGenerator;
import javax.json.stream.JsonParser;

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
    assertEquals(errorMessage(String.format("JSON array length is different at %s", path), expected, actual), expected.size(), actual.size());
    for (int i = 0; i < expected.size(); i++) {
      final JsonValue expectedValue = expected.get(i);
      final JsonValue actualValue = actual.get(i);
      assertEquals(errorMessage(String.format("JSON values have different type at %s[%d]", path, i), expectedValue, actualValue), expectedValue.getClass(), actualValue.getClass());
      if (expectedValue instanceof JsonObject && actualValue instanceof JsonObject) {
        assertJsonObjectEquals((JsonObject) expectedValue, (JsonObject) actualValue, path + "[" + i + "]");
      }
      if (expectedValue instanceof JsonArray && actualValue instanceof JsonArray) {
        assertJsonArrayEquals((JsonArray) expectedValue, (JsonArray) actualValue, path + "[" + i + "]");
      }
    }
    assertEquals(errorMessage(String.format("JSON is different at %s", path), expected, actual), expected, actual);
  }

  public static void assertJsonEquals(final Class<?> resourcesBaseClass, final String expectedResourceFilePath, final JsonObjectBuilder actualJson) {
    System.out.println("Expected JSON is loaded from file: " + resourcesBaseClass.getResource(expectedResourceFilePath).toString());
    try (JsonParser parser = Json.createParser(resourcesBaseClass.getResourceAsStream(expectedResourceFilePath))) {
      assertEquals(JsonParser.Event.START_OBJECT, parser.next());
      final JsonObject expected = parser.getObject();
      final JsonObject actual = actualJson.build();
      assertJsonObjectEquals(expected, actual);
    }
  }

  private static void assertJsonObjectEquals(final JsonObject expected, final JsonObject actual) {
    assertJsonObjectEquals(expected, actual, "");
  }

  private static void assertJsonObjectEquals(final JsonObject expected, final JsonObject actual, final String path) {
    assertEquals(errorMessage(String.format("JSON keys are different at %s", path), expected, actual), expected.keySet(), actual.keySet());
    for (final String key : expected.keySet()) {
      final JsonValue expectedValue = expected.get(key);
      final JsonValue actualValue = actual.get(key);
      assertEquals(
        errorMessage(String.format("JSON values have different type at %s/%s", path, key), expectedValue, actualValue), expectedValue.getClass(), actualValue.getClass());
      if (expectedValue instanceof JsonObject && actualValue instanceof JsonObject) {
        assertJsonObjectEquals((JsonObject) expectedValue, (JsonObject) actualValue, path + "/" + key);
      }
      if (expectedValue instanceof JsonArray && actualValue instanceof JsonArray) {
        assertJsonArrayEquals((JsonArray) expectedValue, (JsonArray) actualValue, path + "/" + key);
      }
    }

    assertEquals(String.format("JSON is different at %s", path), expected, actual);
  }

  private static String errorMessage(final String msg, final JsonValue expectedJson, final JsonValue actualJson) {
    return String.format("%s\n\nExpected:\n%s\n\nActual:\n%s\n\n", msg, prettify(expectedJson), prettify(actualJson));
  }

  public static String prettify(final JsonValue jsonObject) {
    final ByteArrayOutputStream baos = new ByteArrayOutputStream();
    try (JsonWriter writer = prettyJsonWriterFactory.createWriter(baos)) {
      writer.write(jsonObject);
      return new String(baos.toByteArray(), StandardCharsets.UTF_8);
    }
  }

  public static JsonObject string2jsonObject(String s) {
    return Json.createReader(new ByteArrayInputStream(s.getBytes(StandardCharsets.UTF_8))).readObject();
  }
}
