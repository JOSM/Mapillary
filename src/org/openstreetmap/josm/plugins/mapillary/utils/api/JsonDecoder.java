// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.HashSet;
import java.util.Locale;
import java.util.function.Function;

import javax.json.JsonArray;
import javax.json.JsonNumber;
import javax.json.JsonObject;
import javax.json.JsonValue;

import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.tools.Logging;

public final class JsonDecoder {
  private JsonDecoder() {
    // Private constructor to avoid instantiation
  }

  /**
   * Parses a given {@link JsonObject} as a GeoJSON FeatureCollection into a {@link Collection}
   * of the desired Java objects. The method, which converts the GeoJSON features into Java objects
   * is given as a parameter to this method.
   * @param <T> feature type
   * @param json the {@link JsonObject} to be parsed
   * @param featureDecoder feature decoder which transforms JSON objects to Java objects
   * @return a {@link Collection} which is parsed from the given {@link JsonObject}, which contains GeoJSON.
   *         Currently a {@link HashSet} is used, but please don't rely on it, this could change at any time without
   *         prior notice. The return value will not be <code>null</code>.
   */
  public static <T> Collection<T> decodeFeatureCollection(final JsonObject json, Function<JsonObject, T> featureDecoder) {
    final Collection<T> result = new HashSet<>();
    if (
      json != null && "FeatureCollection".equals(json.getString("type", null)) && json.containsKey("features")
    ) {
      final JsonValue features = json.get("features");
      for (int i = 0; features instanceof JsonArray && i < ((JsonArray) features).size(); i++) {
        final JsonValue val = ((JsonArray) features).get(i);
        if (val instanceof JsonObject) {
          final T feature = featureDecoder.apply((JsonObject) val);
          if (feature != null) {
            result.add(feature);
          }
        }
      }
    }
    return result;
  }

  /**
   * Decodes a {@link JsonArray} of exactly size 2 to a {@link LatLon} instance.
   * The first value in the {@link JsonArray} is treated as longitude, the second one as latitude.
   * @param json the {@link JsonArray} containing the two numbers
   * @return the decoded {@link LatLon} instance, or <code>null</code> if the parameter is
   *         not a {@link JsonArray} of exactly size 2 containing two {@link JsonNumber}s.
   */
  static LatLon decodeLatLon(final JsonArray json) {
    final double[] result = decodeDoublePair(json);
    if (result != null) {
      return new LatLon(result[1], result[0]);
    }
    return null;
  }

  /**
   * Decodes a pair of double values, which are stored in a {@link JsonArray} of exactly size 2.
   * @param json the {@link JsonArray} containing the two values
   * @return a double array which contains the two values in the same order, or <code>null</code>
   *         if the parameter was not a {@link JsonArray} of exactly size 2 containing two {@link JsonNumber}s
   */
  @SuppressWarnings("PMD.ReturnEmptyArrayRatherThanNull")
  static double[] decodeDoublePair(final JsonArray json) {
    if (
      json != null &&
      json.size() == 2 &&
      json.get(0) instanceof JsonNumber &&
      json.get(1) instanceof JsonNumber
    ) {
      return new double[]{json.getJsonNumber(0).doubleValue(), json.getJsonNumber(1).doubleValue()};
    }
    return null;
  }

  /**
   * Decodes a timestamp formatted as a {@link String} to the equivalent UNIX epoch timestamp
   * (number of milliseconds since 1970-01-01T00:00:00.000+0000).
   * @param timestamp the timestamp formatted according to the format <code>yyyy-MM-dd'T'HH:mm:ss.SSSX</code>
   * @return the point in time as a {@link Long} value representing the UNIX epoch time, or <code>null</code> if the
   *   parameter does not match the required format (this also triggers a warning via
   *   {@link Logging#warn(Throwable)}), or the parameter is <code>null</code>.
   */
  static Long decodeTimestamp(final String timestamp) {
    if (timestamp != null) {
      try {
        return new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSX", Locale.UK).parse(timestamp).getTime();
      } catch (ParseException e) {
        StackTraceElement calledBy = e.getStackTrace()[Math.min(e.getStackTrace().length - 1, 2)];
        Logging.log(Logging.LEVEL_WARN,String.format(
          "Could not decode time from the timestamp `%s` (called by %s.%s:%d)",
          timestamp, calledBy.getClassName(), calledBy.getMethodName(), calledBy.getLineNumber()
        ), e);
      }
    }
    return null;
  }
}
