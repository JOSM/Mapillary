// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.JsonValue;

import org.openstreetmap.josm.plugins.mapillary.MapillaryData;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;

public final class JsonImageDetailsDecoder {
  private JsonImageDetailsDecoder() {
    // Private constructor to avoid instantiation
  }

  public static void decodeImageInfos(final JsonObject json, final MapillaryData data) {
    if (json != null && "FeatureCollection".equals(json.getString("type", null))) {
      JsonValue features = json.get("features");
      if (features instanceof JsonArray) {
        for (int i = 0; i < ((JsonArray) features).size(); i++) {
          JsonValue feature = ((JsonArray) features).get(i);
          if (feature instanceof JsonObject) {
            decodeImageInfo((JsonObject) ((JsonArray) features).get(i), data);
          }
        }
      }
    }
  }

  private static void decodeImageInfo(final JsonObject json, final MapillaryData data) {
    if (json != null) {
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
