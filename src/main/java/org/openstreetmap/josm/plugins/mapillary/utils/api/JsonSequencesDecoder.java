// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import java.lang.reflect.Array;
import java.util.function.Function;

import javax.json.JsonArray;
import javax.json.JsonNumber;
import javax.json.JsonObject;
import javax.json.JsonString;
import javax.json.JsonValue;

import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.MapillarySequence;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL.APIv3;

/**
 * Decodes the JSON returned by {@link APIv3} into Java objects.
 * Takes a {@link JsonObject} and {@link #decodeSequence(JsonObject)} tries to convert it to a {@link MapillarySequence}.
 */
public final class JsonSequencesDecoder {
  private JsonSequencesDecoder() {
    // Private constructor to avoid instantiation
  }

  /**
   * Parses a given {@link JsonObject} as a GeoJSON Feature into a {@link MapillarySequence}.
   * @param json the {@link JsonObject} to be parsed
   * @return a {@link MapillarySequence} that is parsed from the given {@link JsonObject}. If mandatory information is
   *         missing from the JSON or it's not meeting the expecting format in another way, <code>null</code> will be
   *         returned.
   */
  public static MapillarySequence decodeSequence(final JsonObject json) {
    if (json == null || !"Feature".equals(json.getString("type", null))) {
      return null;
    }
    MapillarySequence result = null;
    final JsonObject properties = json.getJsonObject("properties");
    final Long capturedAt = properties == null ? null : JsonDecoder.decodeTimestamp(properties.getString("captured_at", null));
    if (properties != null && properties.getString("key", null) != null && properties.getString("user_key", null) != null && capturedAt != null) {
      result = new MapillarySequence(properties.getString("key", null), properties.getString("user_key", null), capturedAt);

      final Double[] cas = decodeCoordinateProperty(
        properties,
        "cas",
        val ->  val instanceof JsonNumber ? ((JsonNumber) val).doubleValue() : null,
        Double.class
      );
      final String[] imageKeys = decodeCoordinateProperty(
        properties,
        "image_keys",
        val -> val instanceof JsonString ? ((JsonString) val).getString() : null,
        String.class
      );
      final LatLon[] geometry = decodeLatLons(json.getJsonObject("geometry"));
      final int sequenceLength = Math.min(Math.min(cas.length, imageKeys.length), geometry.length);
      boolean pano = properties.getBoolean("pano", false);
      for (int i = 0; i < sequenceLength; i++) {
        if (cas[i] != null && imageKeys[i] != null && geometry[i] != null) {
          final MapillaryImage img = new MapillaryImage(imageKeys[i], geometry[i], cas[i], pano);
          result.add(img);
        }
      }
      if (result.getImages().isEmpty()) {
        result = null;
      }
    }
    return result;
  }

  /**
   * Converts a {@link JsonArray} to a java array.
   * The conversion from {@link JsonValue} to a java type is done by the supplied function.
   * @param array the array to be converted
   * @param decodeValueFunction the function used for conversion from {@link JsonValue} to the desired type.
   * @param clazz the desired type that the elements of the resulting array should have
   * @return the supplied array converted from {@link JsonArray} to a java array of the supplied type, converted using
   *         the supplied function. Never <code>null</code>, in case of array==null, an array of length 0 is returned.
   */
  @SuppressWarnings("unchecked")
  private static <T> T[] decodeJsonArray(final JsonArray array, final Function<JsonValue, T> decodeValueFunction, final Class<T> clazz) {
    final T[] result;
    if (array == null) {
      result =  (T[]) Array.newInstance(clazz, 0);
    } else {
      result = (T[]) Array.newInstance(clazz, array.size());
      for (int i = 0; i < result.length; i++) {
        result[i] = decodeValueFunction.apply(array.get(i));
      }
    }
    return result;
  }

  /**
   * Given the JSON object representing the `properties` of a sequence, this method converts you one attribute from the
   * `coordinateProperties` object to an array of appropriate type.
   *
   * For example this is used to convert the `image_keys` JSON array to a String[] array or the `cas` JSON array to a
   * Double[] array.
   * @param json the JSON object representing the `properties` of a sequence
   * @param key the key, which identifies the desired array inside the `coordinateProperties` object to be converted
   * @param decodeValueFunction a function that converts the {@link JsonValue}s in the JSON array to java objects of the
   *        desired type
   * @param clazz the {@link Class} object of the desired type, that the entries of the resulting array should have
   * @return the resulting array, when converting the desired entry of the `coordinateProperties`.
   *         Never <code>null</code>. If no `coordinateProperties` are set, or if the desired key is not set or is not
   *         an array, then an empty array of the desired type is returned.
   */
  @SuppressWarnings("unchecked")
  private static <T> T[] decodeCoordinateProperty(
    final JsonObject json, final String key, final Function<JsonValue, T> decodeValueFunction, final Class<T> clazz
  ) {
    final JsonValue coordinateProperties = json == null ? null : json.get("coordinateProperties");
    if (coordinateProperties instanceof JsonObject) {
      JsonValue valueArray = ((JsonObject) coordinateProperties).get(key);
      if (valueArray instanceof JsonArray) {
        return decodeJsonArray((JsonArray) valueArray, decodeValueFunction, clazz);
      }
    }
    return (T[]) Array.newInstance(clazz, 0);
  }

  private static LatLon[] decodeLatLons(final JsonObject json) {
    final JsonValue coords = json == null ? null : json.get("coordinates");
    if (coords instanceof JsonArray && "LineString".equals(json.getString("type", null))) {
      final LatLon[] result = new LatLon[((JsonArray) coords).size()];
      for (int i = 0; i < ((JsonArray) coords).size(); i++) {
        final JsonValue coord = ((JsonArray) coords).get(i);
        if (coord instanceof JsonArray) {
          result[i] = JsonDecoder.decodeLatLon((JsonArray) coord);
        }
      }
      return result;
    }
    return new LatLon[0];
  }
}
