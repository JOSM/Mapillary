// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import java.time.Instant;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.JsonString;
import javax.json.JsonValue;

import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.data.osm.BBox;
import org.openstreetmap.josm.data.osm.TagMap;
import org.openstreetmap.josm.data.vector.VectorDataSet;
import org.openstreetmap.josm.data.vector.VectorNode;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryVectorTileWorkarounds;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryKeys;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL.APIv4;
import org.openstreetmap.josm.tools.Pair;
import org.openstreetmap.josm.tools.Utils;

/**
 * Decodes the JSON returned by {@link APIv4} into Java objects. Takes a
 * {@link JsonValue} and {@link VectorDataSet}
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
  @Nonnull
  public static Collection<VectorNode> decodeImageInfos(final JsonValue json) {
    return decodeImageInfos(json, MapillaryLayer.getInstance().getData());
  }

  /**
   * Decode a json of image information
   *
   * @param json The JSON to decode
   * @param data The data to add the information to
   * @return The added or modified images sorted by sequences
   */
  @Nonnull
  public static Collection<VectorNode> decodeImageInfos(final JsonValue json, final VectorDataSet data) {
    if (data != null) {
      if (json instanceof JsonObject) {
        Pair<String, VectorNode> pair = decodeImageInfo((JsonObject) json, data);
        if (pair == null) {
          return Collections.emptyList();
        }
        return Collections.singletonList(pair.b);
      } else if (json instanceof JsonArray) {
        return ((JsonArray) json).getValuesAs(JsonObject.class).stream().map(j -> decodeImageInfo(j, data))
          .filter(Objects::nonNull).map(pair -> pair.b).collect(Collectors.toList());
      }
    }
    return Collections.emptyList();
  }

  /**
   * Decode a single image info
   *
   * @param json The data json
   * @param data The data to add the image info to
   * @return The MapillaryAbstractImage that was added/modified
   */
  @Nullable
  private static Pair<String, VectorNode> decodeImageInfo(@Nullable final JsonObject json,
    @Nullable final VectorDataSet data) {
    if (json != null && data != null) {
      final boolean useComputedData = Boolean.TRUE.equals(MapillaryProperties.USE_COMPUTED_LOCATIONS.get());
      final String key = json.getString(MapillaryImageUtils.ImageProperties.ID.toString(), null);
      if (key == null) {
        return null;
      }
      final LatLon coordinates = JsonDecoder.decodeLatLon(
        json.getJsonObject(useComputedData ? MapillaryImageUtils.ImageProperties.COMPUTED_GEOMETRY.toString()
          : MapillaryImageUtils.ImageProperties.GEOMETRY.toString()).getJsonArray("coordinates"));
      final BBox searchBBox = new BBox(coordinates);
      searchBBox.addLatLon(coordinates, 0.001);
      VectorNode image = data.searchNodes(searchBBox).stream()
        .filter(node -> key.equals(MapillaryImageUtils.getKey(node))).findAny().orElseGet(() -> {
          VectorNode tImage = createNewImage(json, coordinates);
          data.addPrimitive(tImage);
          return tImage;
        });
      if (coordinates != null) {
        image.setCoor(coordinates);
      }
      for (Map.Entry<String, String> entry : JsonTagMapDecoder.getTagMap(json).entrySet()) {
        if (!image.hasKey(entry.getKey())) {
          image.put(entry.getKey(), entry.getValue());
        }
      }
      TagMap map = image.getKeys();
      // Clean up bad key value combinations
      image.setKeys(map
        .entrySet().stream().filter(entry -> entry.getKey() != null && entry.getValue() != null
          && !Utils.isStripEmpty(entry.getKey()) && !Utils.isStripEmpty(entry.getValue()))
        .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue)));
      final String sequence = MapillaryImageUtils.getSequenceKey(image);
      // Reset the instant
      image.setInstant(Instant.EPOCH);
      // Re-cache the instant
      MapillaryImageUtils.getDate(image);
      return Pair.create(sequence, image);
    }
    return null;
  }

  /**
   * Create a new image from a json
   *
   * @param json The json with the data
   * @param coordinates The coordinates of the image
   * @return A new image
   */
  @Nonnull
  private static VectorNode createNewImage(@Nonnull final JsonObject json, @Nullable final LatLon coordinates) {
    VectorNode tImage = new VectorNode(MapillaryKeys.IMAGE_LAYER);
    for (final MapillaryImageUtils.ImageProperties property : MapillaryImageUtils.ImageProperties.values()) {
      final String propertyKey = property.toString();
      if (json.containsKey(propertyKey)) {
        JsonValue value = json.get(propertyKey);
        if (value.getValueType() == JsonValue.ValueType.NUMBER) {
          tImage.put(propertyKey, value.toString());
        } else if (value.getValueType() == JsonValue.ValueType.STRING) {
          tImage.put(propertyKey, ((JsonString) value).getString());
        } else {
          tImage.put(propertyKey, value.toString());
        }
      }
    }
    if (coordinates != null) {
      tImage.setCoor(coordinates);
    }
    final String id = MapillaryImageUtils.getKey(tImage);
    if (id != null && MapillaryVectorTileWorkarounds.NUMBER_PATTERN.matcher(id).matches()) {
      tImage.setOsmId(Long.parseLong(id), 1);
    }
    return tImage;
  }
}
