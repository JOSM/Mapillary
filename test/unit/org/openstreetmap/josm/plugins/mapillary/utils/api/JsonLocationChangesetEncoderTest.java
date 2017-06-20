// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import static org.junit.Assert.assertEquals;

import java.lang.reflect.Field;
import java.util.Comparator;
import java.util.Map.Entry;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;
import javax.json.JsonValue;

import org.junit.Test;

import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLocationChangeset;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil;

public class JsonLocationChangesetEncoderTest {
  @Test
  public void test() throws IllegalArgumentException, IllegalAccessException {
    Field tempLatLonField = TestUtil.getAccessibleField(MapillaryAbstractImage.class, "tempLatLon");
    Field tempCaField = TestUtil.getAccessibleField(MapillaryAbstractImage.class, "tempCa");

    MapillaryImage img1 = new MapillaryImage("wMAqAFr3xE9072G8Al6WLQ", new LatLon(0, 0), 0);
    tempLatLonField.set(img1, new LatLon(50.44612, 13.3323));
    tempCaField.set(img1, 273.3);
    MapillaryImage img2 = new MapillaryImage("7erPn382xDMtmfdh0xtvUw", new LatLon(0, 0), 0);
    tempLatLonField.set(img2, new LatLon(50.44619, 13.3328));
    MapillaryImage img3 = new MapillaryImage("31KDbCOzla0fJBtIeoBr1A", new LatLon(0, 0), 0);
    tempCaField.set(img3, 13.4);
    MapillaryImage img4 = new MapillaryImage("invalid image key will be ignored", new LatLon(0, 0), 0);
    tempCaField.set(img4, 13.4);

    MapillaryLocationChangeset cs = new MapillaryLocationChangeset();
    cs.add(img1);
    cs.add(img2);
    cs.add(img3);
    cs.add(img4);

    assertEquals(
      sortChanges(Json.createReader(JsonLocationChangesetEncoderTest.class.getResourceAsStream("/api/v3/requests/changeset.json")).readObject()),
      sortChanges(JsonLocationChangesetEncoder.encodeLocationChangeset(cs).build())
    );
  }

  private static JsonObject sortChanges(JsonObject changeset) {
    final JsonObjectBuilder result = Json.createObjectBuilder();
    for (Entry<String, JsonValue> e : changeset.entrySet()) {
      if ("changes".equals(e.getKey())) {
        final JsonArray changes = (JsonArray) e.getValue();
        final JsonArrayBuilder sortedChanges = Json.createArrayBuilder();
        changes.stream().sorted(Comparator.comparing(o -> ((JsonObject) o).getString("image_key"))).forEachOrdered(a -> sortedChanges.add(a));
        result.add(e.getKey(), sortedChanges);
      } else {
        result.add(e.getKey(), e.getValue());
      }
    }
    return result.build();
  }

  @Test
  public void testUtilityClass() {
    TestUtil.testUtilityClass(JsonLocationChangesetEncoder.class);
  }
}
