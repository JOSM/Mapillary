// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import static org.junit.Assert.assertNull;

import java.io.ByteArrayInputStream;
import java.nio.charset.StandardCharsets;
import java.util.function.Function;

import javax.json.Json;
import javax.json.JsonObject;

import org.junit.Test;

import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil;

public class JsonDecoderTest {

  @Test
  public void testUtilityClass() {
    TestUtil.testUtilityClass(JsonDecoder.class);
  }

  @Test
  public void testDecodeDoublePair() {
    assertNull(JsonDecoder.decodeDoublePair(null));
  }

  static void assertDecodesToNull(Function<JsonObject, ?> function, String...parts) {
    assertNull(function.apply(
      Json.createReader(new ByteArrayInputStream(String.join(" ", parts).getBytes(StandardCharsets.UTF_8))).readObject()
    ));
  }

}
