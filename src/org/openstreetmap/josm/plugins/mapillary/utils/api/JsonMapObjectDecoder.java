// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.JsonValue;

import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.plugins.mapillary.objects.MapObject;

public final class JsonMapObjectDecoder {
  private JsonMapObjectDecoder() {
    // Private constructor to avoid instantiation
  }

  public static MapObject decodeMapObject(final JsonObject json) {
    if (json == null || !"Feature".equals(json.getString("type", null))) {
      return null;
    }

    final JsonValue properties = json.get("properties");
    final JsonValue geometry = json.get("geometry");
    if (properties instanceof JsonObject && geometry instanceof JsonObject) {
      final String key = ((JsonObject) properties).getString("key", null);
      final String packg = ((JsonObject) properties).getString("package", null);
      final String value = ((JsonObject) properties).getString("value", null);
      final Long firstSeenTime = JsonDecoder.decodeTimestamp(((JsonObject) properties).getString("first_seen_at", null));
      final Long lastSeenTime = JsonDecoder.decodeTimestamp(((JsonObject) properties).getString("last_seen_at", null));
      final Long updatedTime = JsonDecoder.decodeTimestamp(((JsonObject) properties).getString("updated_at", null));

      final JsonValue coordVal = "Point".equals(((JsonObject) geometry).getString("type", null))
        ? ((JsonObject) geometry).get("coordinates")
        : null;
      final LatLon coordinate = coordVal instanceof JsonArray ? JsonDecoder.decodeLatLon((JsonArray) coordVal) : null;

      if (
        key != null &&
        packg != null &&
        value != null &&
        firstSeenTime != null &&
        lastSeenTime != null &&
        updatedTime != null &&
        coordinate != null
      ) {
        return new MapObject(
          coordinate,
          key,
          packg,
          value,
          firstSeenTime, lastSeenTime, updatedTime
        );
      }
    }
    return null;
  }
}
