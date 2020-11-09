// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.function.Function;

import javax.json.Json;
import javax.json.JsonObject;
import javax.json.JsonReader;

import org.junit.jupiter.api.Test;

import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil;

class JsonDecoderTest {

  @Test
  void testUtilityClass() {
    TestUtil.testUtilityClass(JsonDecoder.class);
  }

  @Test
  void testDecodeDoublePair() {
    assertNull(JsonDecoder.decodeDoublePair(null));
  }

  static void assertDecodesToNull(Function<JsonObject, ?> function, String... parts) {
    try (
      ByteArrayInputStream stream = new ByteArrayInputStream(String.join(" ", parts).getBytes(StandardCharsets.UTF_8));
      JsonReader reader = Json.createReader(stream)) {
      assertNull(function.apply(reader.readObject()));
    } catch (IOException e) {
      fail(e);
    }
  }

}
