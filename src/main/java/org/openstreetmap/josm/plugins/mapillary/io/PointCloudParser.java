// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.io;

import java.io.InputStream;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import jakarta.json.Json;
import jakarta.json.JsonArray;
import jakarta.json.JsonNumber;
import jakarta.json.JsonObject;
import jakarta.json.stream.JsonParser;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.pointcloud.PointCloudCamera;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.pointcloud.PointCloudColor;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.pointcloud.PointCloudLatLonAlt;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.pointcloud.PointCloudPoint;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.pointcloud.PointCloudReconstruction;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.pointcloud.PointCloudShot;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.pointcloud.PointCloudXYZ;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.pointcloud.ProjectionType;

public final class PointCloudParser {
    public static List<PointCloudReconstruction> parse(InputStream inputStream) {
        try (var parser = Json.createParser(inputStream)) {
            return switch (parser.next()) {
            case START_OBJECT -> Collections.singletonList(parseReconstruction(parser));
            case START_ARRAY -> Collections.unmodifiableList(parseReconstructions(parser));
            default -> Collections.emptyList();
            };
        }
    }

    private static List<PointCloudReconstruction> parseReconstructions(JsonParser parser) {
        var list = new ArrayList<PointCloudReconstruction>();
        while (parser.hasNext()) {
            if (parser.next() == JsonParser.Event.START_OBJECT) {
                list.add(parseReconstruction(parser));
            } else if (parser.currentEvent() == JsonParser.Event.START_ARRAY) {
                parser.skipArray();
            }
        }
        list.trimToSize();
        return list;
    }

    private static PointCloudReconstruction parseReconstruction(JsonParser parser) {
        final var cameras = new HashMap<String, PointCloudCamera>();
        final var shots = new HashMap<String, PointCloudShot>();
        final var points = new HashMap<String, PointCloudPoint>();
        PointCloudLatLonAlt referenceLLA = null;
        String lastKey = "";
        while (parser.hasNext()) {
            if (parser.next() == JsonParser.Event.KEY_NAME) {
                lastKey = parser.getString();
                switch (lastKey) {
                case "cameras" -> parseCameras(parser, cameras);
                case "shots" -> parseShots(parser, shots);
                case "points" -> parsePoints(parser, points);
                case "reference_lla" -> referenceLLA = parseReferenceLatLonAltitude(parser);
                case "biases", "rig_cameras", "rig_instances" -> {
                    /* Do nothing */ }
                default -> {
                    /* Do nothing */ } // skip this entry
                }
            } else if (parser.currentEvent() == JsonParser.Event.START_OBJECT) {
                parser.skipObject();
            } else if (parser.currentEvent() == JsonParser.Event.END_OBJECT) {
                break;
            }
        }
        return new PointCloudReconstruction(referenceLLA, Collections.unmodifiableMap(cameras),
            Collections.unmodifiableMap(shots), Collections.unmodifiableMap(points));
    }

    private static PointCloudLatLonAlt parseReferenceLatLonAltitude(JsonParser parser) {
        if (parser.next() == JsonParser.Event.START_OBJECT) {
            final var obj = parser.getObject();
            return new PointCloudLatLonAlt(obj.getJsonNumber("latitude").doubleValue(),
                obj.getJsonNumber("longitude").doubleValue(), obj.getJsonNumber("altitude").doubleValue());
        }
        throw new IllegalStateException("Unknown state for json");
    }

    private static void parseCameras(JsonParser parser, Map<String, PointCloudCamera> cameras) {
        while (parser.hasNext()) {
            if (parser.next() == JsonParser.Event.KEY_NAME) {
                final var cameraId = parser.getString();
                parser.next();
                cameras.put(cameraId, parseCamera(parser.getObject()));
            } else if (parser.currentEvent() == JsonParser.Event.END_OBJECT) {
                return;
            }
        }
    }

    private static PointCloudCamera parseCamera(JsonObject object) {
        final var perspective = switch (object.getString("projection_type")) {
        case "brown" -> ProjectionType.BROWN;
        case "fisheye" -> ProjectionType.FISHEYE;
        case "equirectangular", "spherical" -> ProjectionType.EQUIRECTANGULAR;
        case "perspective" -> ProjectionType.PERSPECTIVE;
        default ->
            throw new IllegalArgumentException("Unknown projection_type: " + object.getString("projection_type"));
        };
        return new PointCloudCamera(perspective, object.getInt("width"), object.getInt("height"),
            object.containsKey("focal") ? object.getJsonNumber("focal").doubleValue() : Double.NaN,
            object.containsKey("k1") ? object.getJsonNumber("k1").doubleValue() : Double.NaN,
            object.containsKey("k2") ? object.getJsonNumber("k2").doubleValue() : Double.NaN);
    }

    private static void parseShots(JsonParser parser, Map<String, PointCloudShot> shots) {
        while (parser.hasNext()) {
            if (parser.next() == JsonParser.Event.KEY_NAME) {
                final var shotId = parser.getString();
                parser.next();
                shots.put(shotId, parseShot(parser.getObject()));
            } else if (parser.currentEvent() == JsonParser.Event.END_OBJECT) {
                return;
            }
        }
    }

    private static PointCloudShot parseShot(JsonObject object) {
        return new PointCloudShot(object.getString("camera"), parseXYZ(object.getJsonArray("rotation")),
            parseXYZ(object.getJsonArray("translation")), parseXYZ(object.getJsonArray("gps_position")),
            object.getJsonNumber("gps_dop").doubleValue(), object.getJsonNumber("orientation").intValue(),
            Instant.ofEpochSecond(object.getJsonNumber("capture_time").longValue()));
    }

    private static void parsePoints(JsonParser parser, Map<String, PointCloudPoint> points) {
        while (parser.hasNext()) {
            if (parser.next() == JsonParser.Event.KEY_NAME) {
                final var pointId = parser.getString();
                parser.next();
                points.put(pointId, parsePoint(parser.getObject()));
            } else if (parser.currentEvent() == JsonParser.Event.END_OBJECT) {
                return;
            }
        }
    }

    private static PointCloudPoint parsePoint(JsonObject object) {
        final var rgb = object.getJsonArray("color").getValuesAs(JsonNumber.class);
        return new PointCloudPoint(parseXYZ(object.getJsonArray("coordinates")),
            new PointCloudColor(rgb.get(0).intValue(), rgb.get(1).intValue(), rgb.get(2).intValue()));
    }

    private static PointCloudXYZ parseXYZ(JsonArray array) {
        return new PointCloudXYZ(array.getJsonNumber(0).doubleValue(), array.getJsonNumber(1).doubleValue(),
            array.getJsonNumber(2).doubleValue());
    }
}
