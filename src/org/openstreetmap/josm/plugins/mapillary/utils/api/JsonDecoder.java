// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Locale;

import javax.json.JsonArray;
import javax.json.JsonNumber;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.data.coor.LatLon;

/**
 *
 */
public final class JsonDecoder {
  private JsonDecoder() {
    // Private constructor to avoid instantiation
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
