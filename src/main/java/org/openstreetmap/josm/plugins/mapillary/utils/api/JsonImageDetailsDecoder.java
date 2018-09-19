// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import javax.json.JsonObject;
import javax.json.JsonValue;

import org.openstreetmap.josm.plugins.mapillary.MapillaryData;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL.APIv3;

/**
 * Decodes the JSON returned by {@link APIv3} into Java objects.
 * Takes a {@link JsonObject} and {@link #decodeImageInfos(JsonObject, MapillaryData)} tries to add the timestamps.
 */
public final class JsonImageDetailsDecoder {
  private JsonImageDetailsDecoder() {
    // Private constructor to avoid instantiation
  }

  public static void decodeImageInfos(final JsonObject json, final MapillaryData data) {
    if (data != null) {
      JsonDecoder.decodeFeatureCollection(json, j -> {
        decodeImageInfo(j, data);
        return null;
      });
    }
  }

  private static void decodeImageInfo(final JsonObject json, final MapillaryData data) {
    if (json != null && data != null) {
      JsonValue properties = json.get("properties");
      if (properties instanceof JsonObject) {
        String key = ((JsonObject) properties).getString("key", null);
        Long capturedAt = JsonDecoder.decodeTimestamp(((JsonObject)properties).getString("captured_at", null));
        if (key != null && capturedAt != null) {
          data.getImages().stream().filter(
            img -> img instanceof MapillaryImage && key.equals(((MapillaryImage) img).getKey())
          ).forEach(img -> img.setCapturedAt(capturedAt));
        }
      }
    }
  }
}
