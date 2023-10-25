// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.geom.Path2D;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Base64;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.json.JsonObject;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;
import org.openstreetmap.josm.data.imagery.vectortile.mapbox.Feature;
import org.openstreetmap.josm.data.imagery.vectortile.mapbox.Layer;
import org.openstreetmap.josm.data.protobuf.ProtobufParser;
import org.openstreetmap.josm.data.protobuf.ProtobufRecord;
import org.openstreetmap.josm.plugins.mapillary.model.ImageDetection;
import org.openstreetmap.josm.tools.JosmRuntimeException;
import org.openstreetmap.josm.tools.Logging;

/**
 * Decodes the JSON returned by {@link org.openstreetmap.josm.plugins.mapillary.spi.preferences.IMapillaryUrls} into
 * Java objects.
 * Takes a {@link JsonValue} and {@link #decodeImageDetection(JsonValue, Long)} tries to convert it to a
 * {@link ImageDetection}.
 */
public final class JsonImageDetectionDecoder {
    private JsonImageDetectionDecoder() {
        // Private constructor to avoid instantiation
    }

    /**
     * Convert a json value into a collection of image detections
     *
     * @param json The json to convert
     * @param defaultImageKey A default image key, if known (i.e., the detection is coming from an image request)
     * @return A collection of image detections
     */
    @Nonnull
    public static Collection<ImageDetection<?>> decodeImageDetection(@Nullable final JsonValue json,
        @Nullable final Long defaultImageKey) {
        if (json == null) {
            return Collections.emptyList();
        } else if (json.getValueType() == JsonValue.ValueType.ARRAY) {
            final List<ImageDetection<?>> returnList = new ArrayList<>(json.asJsonArray().size());
            for (JsonValue value : json.asJsonArray()) {
                returnList.addAll(decodeImageDetection(value, defaultImageKey));
            }
            returnList.removeIf(Objects::isNull);
            return Collections.unmodifiableList(returnList);
        } else if (json.getValueType() != JsonValue.ValueType.OBJECT) {
            return Collections.emptyList();
        }
        final JsonObject jsonObject = json.asJsonObject();

        final long key = Long.parseLong(jsonObject.getString("id", "0"));
        final Long imageKey = decodeImageIds(jsonObject.get("image"), defaultImageKey);
        final String value = jsonObject.getString("value", null);
        final Shape shape = decodeShape(jsonObject.get("geometry"));
        if (shape != null && imageKey != null && key != 0 && value != null) {
            try {
                return Collections.singletonList(new ImageDetection<>(shape, imageKey, key, value));
            } catch (IllegalArgumentException e) {
                if (e.getMessage().startsWith("Unknown detection")) {
                    Logging.error(e.getMessage());
                } else {
                    throw e;
                }
            }
        }
        return Collections.emptyList();
    }

    /**
     * Decode image ids (so we can select the appropriate image)
     *
     * @param jsonValue The value to decode
     * @param defaultImageId The default image id. May be {@code null}.
     * @return The image id
     */
    @Nullable
    private static Long decodeImageIds(@Nullable JsonValue jsonValue, @Nullable Long defaultImageId) {
        if (jsonValue != null && jsonValue.getValueType() == JsonValue.ValueType.OBJECT) {
            final JsonObject jsonObject = jsonValue.asJsonObject();
            if (jsonObject.containsKey("id")) {
                final JsonValue id = jsonObject.get("id");
                if (id.getValueType() == JsonValue.ValueType.STRING) {
                    return Long.parseLong(((JsonString) id).getString());
                }
            }
        }
        return defaultImageId;
    }

    /**
     * Decode a shape from a json value
     *
     * @param json The json value (should be a {@link JsonString})
     * @return The decoded shape or {@code null}
     */
    @Nullable
    private static Shape decodeShape(@Nullable JsonValue json) {
        if (json instanceof JsonString) {
            // v4 API returns geometry base64 encoded
            final byte[] base64Decode = Base64.getDecoder().decode(((JsonString) json).getString());
            final Collection<Shape> shapes = getShapes(base64Decode);
            if (shapes.size() == 1) {
                return shapes.iterator().next();
            } else if (!shapes.isEmpty()) {
                final Path2D.Double path = new Path2D.Double();
                for (Shape shape : shapes) {
                    path.append(shape, false);
                }
                return path;
            }
        }
        return null;
    }

    /**
     * Get the shapes from a byte array
     *
     * @param base64Decode The decoded byte array
     * @return The shapes from the vector tile (if there is only one feature)
     */
    @Nonnull
    private static Collection<Shape> getShapes(@Nonnull final byte[] base64Decode) {
        // The decoded bytes are further encoded in the Mapbox Vector Tile format
        try (ProtobufParser parser = new ProtobufParser(base64Decode)) {
            final Collection<ProtobufRecord> layerRecord = parser.allRecords();
            if (layerRecord.size() == 1) {
                try (ProtobufRecord protobufRecord = layerRecord.iterator().next()) {
                    // the layer field is 3
                    if (protobufRecord.getField() == 3) {
                        try (ProtobufParser layerParser = new ProtobufParser(protobufRecord.getBytes())) {
                            final Layer layer = new Layer(layerParser.allRecords());
                            if (layer.getFeatures().size() == 1) {
                                final Feature feature = layer.getFeatures().iterator().next();
                                return resizeShapes(feature.getGeometryObject().getShapes(), layer.getExtent());
                            }
                        }
                    }
                }
            }
        } catch (IOException e) {
            // This should never be hit -- the IOException should not occur when reading a byte array.
            // So throw a runtime exception if it happens.
            throw new JosmRuntimeException(e);
        }
        return Collections.emptyList();
    }

    /**
     * Resize shapes such that only one scale instance needs to be created to draw in the image viewer
     * (i.e., we don't need to pass the extent around)
     *
     * @param shapes The shapes to transform
     * @param extent The extent of the vector tile
     * @return The resized shapes
     */
    @Nonnull
    private static Collection<Shape> resizeShapes(@Nonnull final Collection<Shape> shapes, final int extent) {
        final AffineTransform scale = AffineTransform.getScaleInstance(1d / extent, 1d / extent);
        return shapes.stream().map(scale::createTransformedShape).collect(Collectors.toList());
    }
}
