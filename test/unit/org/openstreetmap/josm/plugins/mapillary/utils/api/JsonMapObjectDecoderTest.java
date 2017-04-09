// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.io.ByteArrayInputStream;
import java.util.Collection;

import javax.json.Json;

import org.junit.Test;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.plugins.mapillary.model.MapObject;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil;

public class JsonMapObjectDecoderTest {

  @Test
  public void testDecodeMapObjects() {
    Collection<MapObject> exampleMapObjects = JsonDecoder.decodeFeatureCollection(
      Json.createReader(this.getClass().getResourceAsStream("/api/v3/responses/searchMapObjects.json")).readObject(),
      JsonMapObjectDecoder::decodeMapObject
    );
    assertEquals(1, exampleMapObjects.size());

    MapObject exampleMapObject = exampleMapObjects.iterator().next();

    assertEquals(1_476_610_976_060L, exampleMapObject.getFirstSeenTime()); // 2016-10-16T09:42:56.060 UTC
    assertEquals(1_476_610_976_060L, exampleMapObject.getLastSeenTime()); // 2016-10-16T09:42:56.060 UTC
    assertEquals(1_480_422_082_275L, exampleMapObject.getUpdatedTime()); // 2016-11-29T12:21:22.275 UTC
    assertEquals("trafficsign", exampleMapObject.getPackage());
    assertEquals("regulatory--no-parking--g1", exampleMapObject.getValue());
    assertEquals("qpku21qv8rjn7fll1v671732th", exampleMapObject.getKey());
    assertEquals(new LatLon(55.608367919921875, 13.005650520324707), exampleMapObject.getCoordinate());
  }

  @Test
  public void testDecodeMapObject() {
    MapObject exampleMapObject = JsonMapObjectDecoder.decodeMapObject(
      Json.createReader(this.getClass().getResourceAsStream("/api/v3/responses/mapObject.json")).readObject()
    );
    assertNotNull(exampleMapObject);
    assertEquals("9f3tl0z2xanom2inyyks65negx", exampleMapObject.getKey());
    assertEquals("trafficsign", exampleMapObject.getPackage());
    assertEquals("regulatory--no-entry--g1", exampleMapObject.getValue());
    assertEquals(1_467_377_348_553L, exampleMapObject.getLastSeenTime()); // 2016-07-01T12:49:08.553 UTC
    assertEquals(1_467_377_348_553L, exampleMapObject.getFirstSeenTime()); // 2016-07-01T12:49:08.553 UTC
    assertEquals(1_486_566_123_778L, exampleMapObject.getUpdatedTime()); // 2017-02-08T15:02:03.778 UTC
  }

  @Test
  public void testDecodeMapObjectInvalid() {
    assertNull(JsonMapObjectDecoder.decodeMapObject(null));
    assertNull(JsonMapObjectDecoder.decodeMapObject(Json.createReader(new ByteArrayInputStream("{}".getBytes())).readObject()));
    assertMapObjectInvalid("{\"type\":\"Feature\", \"geometry\":{}}");
    assertMapObjectInvalid("{\"type\":\"Feature\", \"properties\":{}}");
    assertMapObjectInvalid("{\"type\":\"Feature\", \"geometry\":{ \"type\":\"bla\"}, \"properties\":{}}");
    assertMapObjectInvalid("{\"type\":\"Feature\", \"geometry\":{}, \"properties\":{\"key\":\"a\"}}");
    assertMapObjectInvalid("{\"type\":\"Feature\", \"geometry\":{}, \"properties\":{\"key\":\"a\", \"package\":\"b\"}}");
    assertMapObjectInvalid(
      "{\"type\":\"Feature\", \"geometry\":{}, \"properties\":{\"key\":\"a\", \"package\":\"b\", \"value\":\"c\"}}"
    );
    assertMapObjectInvalid(
      "{\"type\":\"Feature\", \"geometry\":{}, \"properties\":{\"key\":\"a\", \"package\":\"b\", " +
      "\"value\":\"c\", \"first_seen_at\":\"1970-01-01T00:00:00.000+0100\"}}"
    );
    assertMapObjectInvalid(
      "{\"type\":\"Feature\", \"geometry\":{}, \"properties\":{\"key\":\"a\", \"package\":\"b\", \"value\":\"c\", " +
      "\"first_seen_at\":\"1970-01-01T00:00:00.000+0100\", \"last_seen_at\":\"2000-12-31T23:59:59.999Z\"}}"
    );
    assertMapObjectInvalid(
      "{\"type\":\"Feature\", \"geometry\":{}, \"properties\":{\"key\":\"a\", \"package\":\"b\", \"value\":\"c\", " +
      "\"first_seen_at\":\"1970-01-01T00:00:00.000+0100\", \"last_seen_at\":\"2000-12-31T23:59:59.999Z\", " +
      "\"updated_at\": \"1970-01-01T00:00:00.000Z\"}}"
    );
  }

  private static void assertMapObjectInvalid(String json) {
    assertNull(JsonMapObjectDecoder.decodeMapObject(
      Json.createReader(new ByteArrayInputStream(json.getBytes())).readObject()
    ));
  }

  @Test
  public void testUtilityClass() {
    TestUtil.testUtilityClass(JsonMapObjectDecoder.class);
  }
}
