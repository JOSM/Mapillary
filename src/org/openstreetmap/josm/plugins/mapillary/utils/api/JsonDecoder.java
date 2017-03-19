// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Locale;
import java.util.function.Function;

import javax.json.JsonArray;
import javax.json.JsonNumber;
import javax.json.JsonObject;
import javax.json.JsonValue.ValueType;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.plugins.mapillary.MapillarySequence;

/**
 *
 */
public final class JsonDecoder {
  private JsonDecoder() {
    // Private constructor to avoid instantiation
  }

  /**
   * Parses a given {@link JsonObject} as a GeoJSON FeatureCollection into a {@link Collection}
   * of the desired Java objects. The method, which converts the GeoJSON features into Java objects
   * is given as a parameter to this method.
   * @param json the {@link JsonObject} to be parsed
   * @return a {@link Collection} of {@link MapillarySequence}s that are parsed from the given {@link JsonObject}.
   *         Currently a {@link HashMap} is used, but please don't rely on it, this could change at any time without
   *         prior notice. The return value will not be <code>null</code>.
   */
  public static <T> Collection<T> decodeFeatureCollection(final JsonObject json, Function<JsonObject, T> featureDecoder) {
    final Collection<T> result = new HashSet<>();
    if (
      json != null && "FeatureCollection".equals(json.getString("type", null)) &&
      json.containsKey("features") && json.get("features").getValueType() == ValueType.ARRAY
    ) {
      final JsonArray features = json.getJsonArray("features");
      for (int i = 0; i < features.size(); i++) {
        final T feature = featureDecoder.apply(features.getJsonObject(i));
        if (feature != null) {
          result.add(feature);
        }
      }
    }
    return result;
  }

  static LatLon decodeLatLon(final JsonArray json) {
    if (
      json.size() == 2 &&
      json.get(0) instanceof JsonNumber &&
      json.get(1) instanceof JsonNumber
    ) {
      return new LatLon(json.getJsonNumber(1).doubleValue(), json.getJsonNumber(0).doubleValue());
    }
    return null;
  }

  static Long decodeTimestamp(final String timestamp) {
    if (timestamp != null) {
      try {
        return new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSX", Locale.UK).parse(timestamp).getTime();
      } catch (ParseException e) {
        StackTraceElement calledBy = e.getStackTrace()[Math.min(e.getStackTrace().length - 1, 2)];
        Main.warn(e, String.format(
          "Could not decode time from the timestamp `%s` (called by %s.%s:%d)",
          timestamp, calledBy.getClassName(), calledBy.getMethodName(), calledBy.getLineNumber()
        ));
      }
    }
    return null;
  }
}
