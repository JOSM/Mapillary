// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.data.vector.VectorDataSet;
import org.openstreetmap.josm.data.vector.VectorNode;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryKeys;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL.APIv3;
import org.openstreetmap.josm.tools.Pair;

import javax.json.JsonObject;
import javax.json.JsonValue;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import java.util.stream.Collector;
import java.util.stream.Collectors;

/**
 * Decodes the JSON returned by {@link APIv3} into Java objects. Takes a
 * {@link JsonObject} and {@link #decodeImageInfos(JsonObject, VectorDataSet)}
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
  public static Map<String, Collection<INode>> decodeImageInfos(final JsonObject json) {
    return decodeImageInfos(json, MapillaryLayer.getInstance().getData());
  }

  /**
   * Decode a json of image information
   *
   * @param json The JSON to decode
   * @param data The data to add the information to
   * @return The added or modified images sorted by sequences
   */
  public static Map<String, Collection<INode>> decodeImageInfos(final JsonObject json, final VectorDataSet data) {
    if (data != null) {
      return JsonDecoder.decodeFeatureCollection(json, j -> decodeImageInfo(j, data)).stream()
        .filter(entry -> entry.b != null).collect(Collectors.groupingBy(p -> p.a,
          Collector.of(ArrayList<INode>::new, (list, entry) -> list.add(entry.b), (list1, list2) -> {
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
  private static Pair<String, INode> decodeImageInfo(final JsonObject json, final VectorDataSet data) {
    if (json != null && data != null) {
      JsonValue propertiesValue = json.get("properties");
      if (propertiesValue instanceof JsonObject) {
        JsonObject properties = (JsonObject) propertiesValue;
        String key = properties.getString(MapillaryImageUtils.KEY, null);
        LatLon coordinates = JsonDecoder.decodeLatLon(json.getJsonObject("geometry").getJsonArray("coordinates"));
        if (key != null) {
          VectorNode image = data.getNodes().stream().filter(node -> key.equals(node.get(MapillaryImageUtils.KEY)))
            .findAny().orElseGet(() -> {
              VectorNode tImage = new VectorNode(MapillaryKeys.IMAGE_LAYER);
              tImage.put(MapillaryImageUtils.KEY, key);
              tImage.put(MapillaryImageUtils.CAMERA_ANGLE,
                properties.getJsonNumber(MapillaryImageUtils.CAMERA_ANGLE).toString());
              tImage.put(MapillaryImageUtils.PANORAMIC,
                properties.getBoolean("pano", false) ? MapillaryKeys.PANORAMIC_TRUE : MapillaryKeys.PANORAMIC_FALSE);
              if (coordinates != null) {
                tImage.setCoor(coordinates);
              }
              data.addPrimitive(tImage);
              return tImage;
            });
          if (image.getCoor() == null && coordinates != null) {
            image.setCoor(coordinates);
          }
          String sequence = properties.getString(MapillaryImageUtils.SEQUENCE_KEY, "");
          image.setKeys(JsonTagMapDecoder.getTagMap(properties));
          // Reset the instant
          image.setInstant(Instant.EPOCH);
          // Recache the instant
          MapillaryImageUtils.getDate(image);
          return Pair.create(sequence, image);
        }
      }
    }
    return null;
  }
}
