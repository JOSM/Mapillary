// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.JsonValue;

import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.plugins.mapillary.model.MapObject;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL.APIv3;

/**
 * Decodes the JSON returned by {@link APIv3} into Java objects.
 * Takes a {@link JsonObject} and {@link #decodeMapObject(JsonObject)} tries to convert it to a {@link MapObject}.
 */
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
      final String layer = ((JsonObject) properties).getString("layer", null);
      final String value = ((JsonObject) properties).getString("value", null);
      final Long firstSeenTime = JsonDecoder.decodeTimestamp(((JsonObject) properties).getString("first_seen_at", null));
      final Long lastSeenTime = JsonDecoder.decodeTimestamp(((JsonObject) properties).getString("last_seen_at", null));
      final Long updatedTime = JsonDecoder.decodeTimestamp(((JsonObject) properties).getString("last_seen_at", null));

      final JsonValue coordVal = "Point".equals(((JsonObject) geometry).getString("type", null))
        ? ((JsonObject) geometry).get("coordinates")
        : null;
      final LatLon coordinate = coordVal instanceof JsonArray ? JsonDecoder.decodeLatLon((JsonArray) coordVal) : null;

      if (
        key != null &&
        layer != null &&
        value != null &&
        firstSeenTime != null &&
        lastSeenTime != null &&
        coordinate != null
      ) {
        return new MapObject(
          coordinate,
          key,
          layer,
          value,
          firstSeenTime, lastSeenTime
        );
      }
    }
    return null;
  }
}
