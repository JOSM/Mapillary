// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import static org.openstreetmap.josm.plugins.mapillary.utils.api.JsonDecoder.decodeDoublePair;

import java.awt.Shape;
import java.awt.geom.Path2D;

import javax.json.JsonArray;
import javax.json.JsonNumber;
import javax.json.JsonObject;
import javax.json.JsonValue;

import org.openstreetmap.josm.plugins.mapillary.model.ImageDetection;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL.APIv3;
import org.openstreetmap.josm.tools.Logging;

/**
 * Decodes the JSON returned by {@link APIv3} into Java objects.
 * Takes a {@link JsonObject} and {@link #decodeImageDetection(JsonObject)} tries to convert it to a {@link ImageDetection}.
 */
public final class JsonImageDetectionDecoder {
  private JsonImageDetectionDecoder() {
    // Private constructor to avoid instantiation
  }

  public static ImageDetection decodeImageDetection(final JsonObject json) {
    if (json == null || !"Feature".equals(json.getString("type", null))) {
      return null;
    }

    final JsonValue properties = json.get("properties");
    if (properties instanceof JsonObject) {
      final String key = ((JsonObject) properties).getString("key", null);
      final String packag = ((JsonObject) properties).getString("package", null);
      final String imageKey = ((JsonObject) properties).getString("image_key", null);
      final String value = ((JsonObject) properties).getString("value", null);
      final JsonValue scoreVal = ((JsonObject) properties).get("score");
      final Double score = scoreVal instanceof JsonNumber ? ((JsonNumber) scoreVal).doubleValue() : null;
      final Shape shape = decodeShape(((JsonObject) properties).get("shape"));
      if (shape instanceof Path2D && imageKey != null && key != null && score != null && packag != null && value != null) {
        return new ImageDetection((Path2D) shape, imageKey, key, score, packag, value);
      }
    }
    return null;
  }

  private static Shape decodeShape(JsonValue json) {
    if (json instanceof JsonObject) {
      if (!"Polygon".equals(((JsonObject) json).getString("type", null))) {
        Logging.warn(
          String.format("Image detections using shapes with type=%s are currently not supported!",
          ((JsonObject) json).getString("type", "‹no type set›"))
        );
      } else {
        final JsonValue coordinates = ((JsonObject) json).get("coordinates");
        if (coordinates instanceof JsonArray && !((JsonArray) coordinates).isEmpty()) {
          return decodePolygon((JsonArray) coordinates);
        }
      }
    }
    return null;
  }

  /**
   * Decodes a polygon (may be a multipolygon) from JSON
   * @param json the json array to decode, must not be <code>null</code>
   * @return the decoded polygon as {@link Path2D.Double}
   */
  private static Path2D decodePolygon(final JsonArray json) {
    final Path2D shape = new Path2D.Double();
    json.forEach(val -> {
      final Shape part = val instanceof JsonArray ? decodeSimplePolygon((JsonArray) val) : null;
      if (part != null) {
        shape.append(part, false);
      }
    });
    if (shape.getCurrentPoint() != null) {
      return shape;
    }
    return null;
  }

  /**
   * Decodes a simple polygon (consisting of only one continuous path) from JSON
   * @param json the json array to decode, must not be <code>null</code>
   * @return the decoded polygon as {@link Path2D.Double}
   * @throws NullPointerException if parameter is <code>null</code>
   */
  private static Path2D decodeSimplePolygon(final JsonArray json) {
    final Path2D shape = new Path2D.Double();
    json.forEach(val -> {
      double[] coord = decodeDoublePair(val instanceof JsonArray ? (JsonArray) val : null);
      if (shape.getCurrentPoint() == null && coord != null) {
        shape.moveTo(coord[0], coord[1]);
      } else if (coord != null) {
        shape.lineTo(coord[0], coord[1]);
      }
    });
    if (shape.getCurrentPoint() != null) {
      shape.closePath();
      return shape;
    }
    return null;
  }
}
