// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.Collection;

import javax.json.Json;
import javax.json.JsonReader;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.RegisterExtension;

import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.plugins.mapillary.model.MapObject;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil.MapillaryTestRules;
import org.openstreetmap.josm.testutils.JOSMTestRules;

class JsonMapObjectDecoderTest {

  @RegisterExtension
  JOSMTestRules rules = new MapillaryTestRules();

  @Test
  void testDecodeMapObjects() throws IOException {
    try (InputStream stream = this.getClass().getResourceAsStream("/api/v3/responses/searchMapObjects.json");
      JsonReader reader = Json.createReader(stream)) {
      Collection<MapObject> exampleMapObjects = JsonDecoder.decodeFeatureCollection(reader.readObject(),
        JsonMapObjectDecoder::decodeMapObject);
      assertEquals(1, exampleMapObjects.size());

      MapObject exampleMapObject = exampleMapObjects.iterator().next();

      assertEquals(1_476_610_976_060L, exampleMapObject.getFirstSeenTime()); // 2016-10-16T09:42:56.060 UTC
      assertEquals(1_476_610_976_060L, exampleMapObject.getLastSeenTime()); // 2016-10-16T09:42:56.060 UTC
      assertEquals("trafficsign", exampleMapObject.getLayer());
      assertEquals("regulatory--no-parking--g1", exampleMapObject.getValue());
      assertEquals("qpku21qv8rjn7fll1v671732th", exampleMapObject.getKey());
      assertEquals(new LatLon(55.608367919921875, 13.005650520324707), exampleMapObject.getCoordinate());
    }
  }

  @Test
  void testDecodeMapObject() throws IOException {
    try (InputStream stream = this.getClass().getResourceAsStream("/api/v3/responses/mapObject.json");
      JsonReader reader = Json.createReader(stream)) {
      MapObject exampleMapObject = JsonMapObjectDecoder.decodeMapObject(reader.readObject());
      assertNotNull(exampleMapObject);
      assertEquals("9f3tl0z2xanom2inyyks65negx", exampleMapObject.getKey());
      assertEquals("trafficsign", exampleMapObject.getLayer());
      assertEquals("regulatory--no-entry--g1", exampleMapObject.getValue());
      assertEquals(1_467_377_348_553L, exampleMapObject.getLastSeenTime()); // 2016-07-01T12:49:08.553 UTC
      assertEquals(1_467_377_348_553L, exampleMapObject.getFirstSeenTime()); // 2016-07-01T12:49:08.553 UTC
    }
  }

  @Test
  void testDecodeMapObjectInvalid() throws IOException {
    assertNull(JsonMapObjectDecoder.decodeMapObject(null));
    try (InputStream stream = new ByteArrayInputStream("{}".getBytes(StandardCharsets.UTF_8));
      JsonReader reader = Json.createReader(stream)) {
      assertNull(JsonMapObjectDecoder.decodeMapObject(reader.readObject()));
    }
    assertMapObjectInvalid("{\"type\":\"Feature\", \"geometry\":{}}");
    assertMapObjectInvalid("{\"type\":\"Feature\", \"properties\":{}}");
    assertMapObjectInvalid("{\"type\":\"Feature\", \"geometry\":{ \"type\":\"bla\"}, \"properties\":{}}");
    assertMapObjectInvalid("{\"type\":\"Feature\", \"geometry\":{}, \"properties\":{\"key\":\"a\"}}");
    assertMapObjectInvalid(
      "{\"type\":\"Feature\", \"geometry\":{}, \"properties\":{\"key\":\"a\", \"package\":\"b\"}}");
    assertMapObjectInvalid(
      "{\"type\":\"Feature\", \"geometry\":{}, \"properties\":{\"key\":\"a\", \"package\":\"b\", \"value\":\"c\"}}");
    assertMapObjectInvalid("{\"type\":\"Feature\", \"geometry\":{}, \"properties\":{\"key\":\"a\", \"package\":\"b\", "
      + "\"value\":\"c\", \"first_seen_at\":\"1970-01-01T00:00:00.000+0100\"}}");
    assertMapObjectInvalid(
      "{\"type\":\"Feature\", \"geometry\":{}, \"properties\":{\"key\":\"a\", \"package\":\"b\", \"value\":\"c\", "
        + "\"first_seen_at\":\"1970-01-01T00:00:00.000+0100\", \"last_seen_at\":\"2000-12-31T23:59:59.999Z\"}}");
    assertMapObjectInvalid(
      "{\"type\":\"Feature\", \"geometry\":{}, \"properties\":{\"key\":\"a\", \"package\":\"b\", \"value\":\"c\", "
        + "\"first_seen_at\":\"1970-01-01T00:00:00.000+0100\", \"last_seen_at\":\"2000-12-31T23:59:59.999Z\", "
        + "\"updated_at\": \"1970-01-01T00:00:00.000Z\"}}");
  }

  private static void assertMapObjectInvalid(String json) {
    try (InputStream stream = new ByteArrayInputStream(json.getBytes(StandardCharsets.UTF_8));
      JsonReader reader = Json.createReader(stream)) {
      assertNull(JsonMapObjectDecoder.decodeMapObject(reader.readObject()));
    } catch (IOException e) {
      fail(json, e);
    }
  }

  @Test
  void testUtilityClass() {
    TestUtil.testUtilityClass(JsonMapObjectDecoder.class);
  }
}
