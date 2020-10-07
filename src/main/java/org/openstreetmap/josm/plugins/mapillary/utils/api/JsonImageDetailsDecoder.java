// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collector;
import java.util.stream.Collectors;

import javax.json.JsonObject;
import javax.json.JsonValue;

import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryData;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.MapillarySequence;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL.APIv3;
import org.openstreetmap.josm.tools.Pair;

/**
 * Decodes the JSON returned by {@link APIv3} into Java objects. Takes a
 * {@link JsonObject} and {@link #decodeImageInfos(JsonObject, MapillaryData)}
 * tries to add the timestamps.
 */
public final class JsonImageDetailsDecoder {
  private JsonImageDetailsDecoder() {
    // Private constructor to avoid instantiation
  }

  /**
   * Decode a json of image information
   *
   * @param json The JSON to decode
   * @return The added or modified images sorted by sequences
   */
  public static Map<String, Collection<MapillaryAbstractImage>> decodeImageInfos(final JsonObject json) {
    return decodeImageInfos(json, MapillaryLayer.getInstance().getData());
  }

  /**
   * Decode a json of image information
   *
   * @param json The JSON to decode
   * @param data The data to add the information to
   * @return The added or modified images sorted by sequences
   */
  public static Map<String, Collection<MapillaryAbstractImage>> decodeImageInfos(final JsonObject json,
      final MapillaryData data) {
    if (data != null) {
      return JsonDecoder.decodeFeatureCollection(json, j -> decodeImageInfo(j, data)).stream()
          .filter(entry -> entry.b != null)
          .collect(Collectors.groupingBy(p -> p.a, Collector.of(ArrayList<MapillaryAbstractImage>::new,
              (list, entry) -> list.add(entry.b), (list1, list2) -> {
                list1.addAll(list2);
                return list1;
              })));
    }
    return null;
  }

  /**
   * Decode a single image info (only use this if the initial type is Feature, not
   * FeatureCollection)
   *
   * @param json The Feature json
   * @param data The data to add the image info to
   * @return The MapillaryAbstractImage that was added/modified
   */
  private static Pair<String, MapillaryAbstractImage> decodeImageInfo(final JsonObject json,
      final MapillaryData data) {
    if (json != null && data != null) {
      JsonValue propertiesValue = json.get("properties");
      if (propertiesValue instanceof JsonObject) {
        JsonObject properties = (JsonObject) propertiesValue;
        String key = properties.getString("key", null);
        Long capturedAt = JsonDecoder.decodeTimestamp(properties.getString("captured_at", null));
        if (key != null && capturedAt != null) {
          MapillaryAbstractImage image = data.getImage(key);
          if (image == null) {
            image = new MapillaryImage(key, null, properties.getJsonNumber("ca").doubleValue(),
                properties.getBoolean("pano", false), properties.getBoolean("private", false));
            data.add(image);
          }
          if (image.getExifCoor() == null) {
            image.setExifCoor(JsonDecoder.decodeLatLon(json.getJsonObject("geometry").getJsonArray("coordinates")));
          }
          image.setCapturedAt(capturedAt);
          String sequence = properties.getString("sequence_key", "");
          Optional<MapillarySequence> rSequence = data.getSequences().stream()
              .filter(seq -> sequence.equals(seq.getKey())).findAny();
          if (rSequence.isPresent()) {
            image.setSequence(rSequence.get());
          }
          return Pair.create(sequence, image);
        }
      }
    }
    return null;
  }
}
