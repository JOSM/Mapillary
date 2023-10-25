// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import java.time.Instant;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.json.JsonArray;
import jakarta.json.JsonObject;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;
import org.openstreetmap.josm.data.coor.ILatLon;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.data.osm.BBox;
import org.openstreetmap.josm.data.osm.Tag;
import org.openstreetmap.josm.data.osm.TagMap;
import org.openstreetmap.josm.data.vector.VectorDataSet;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.layer.geoimage.ImageViewerDialog;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.MapillaryNode;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.geoimage.MapillaryImageEntry;
import org.openstreetmap.josm.plugins.mapillary.model.ImageDetection;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.tools.Pair;
import org.openstreetmap.josm.tools.Utils;

/**
 * Decodes the JSON returned by {@link org.openstreetmap.josm.plugins.mapillary.spi.preferences.IMapillaryUrls} into
 * Java objects. Takes a
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
    public static Collection<MapillaryNode> decodeImageInfos(final JsonValue json) {
        if (json instanceof JsonObject) {
            Pair<String, MapillaryNode> pair = decodeImageInfo((JsonObject) json);
            if (pair == null) {
                return Collections.emptyList();
            }
            return Collections.singletonList(pair.b);
        } else if (json instanceof JsonArray) {
            return ((JsonArray) json).getValuesAs(JsonObject.class).stream()
                .map(JsonImageDetailsDecoder::decodeImageInfo).filter(Objects::nonNull).map(pair -> pair.b)
                .collect(Collectors.toList());
        }
        return Collections.emptyList();
    }

    /**
     * Decode a single image info
     *
     * @param json The data json
     * @return The MapillaryAbstractImage that was added/modified
     */
    @Nullable
    private static Pair<String, MapillaryNode> decodeImageInfo(@Nullable final JsonObject json) {
        if (json != null) {
            final boolean useComputedData = Boolean.TRUE.equals(MapillaryProperties.USE_COMPUTED_LOCATIONS.get());
            final String key = json.getString(MapillaryImageUtils.ImageProperties.ID.toString(), null);
            if (key == null) {
                return null;
            }
            final long id = Long.parseLong(key);
            final LatLon coordinates = getCoordinates(json, useComputedData);
            final BBox searchBBox = new BBox(coordinates);
            searchBBox.addLatLon(coordinates, 0.001);
            final MapillaryNode image = createNewImage(json, coordinates);

            if (coordinates != null) {
                image.setCoor(coordinates);
                if (Optional.ofNullable(MainApplication.getMap())
                    .map(map -> map.getToggleDialog(ImageViewerDialog.class)).isPresent()
                    && ImageViewerDialog.getCurrentImage() instanceof MapillaryImageEntry) {
                    MapillaryImageEntry entry = (MapillaryImageEntry) ImageViewerDialog.getCurrentImage();
                    if (coordinates.equals(entry.getPos())) {
                        entry.reload();
                    }
                }
            }
            for (Map.Entry<String, String> entry : JsonTagMapDecoder.getTagMap(json).entrySet()) {
                image.put(entry.getKey(), entry.getValue());
            }
            TagMap map = image.getKeys();
            // Clean up bad key value combinations
            // Using for loop to (hopefully) fix JOSM #21070 and #21072
            for (Tag tag : map.getTags()) {
                // Tag#getKey and Tag#getValue are never null. According to docs.
                if (Utils.isStripEmpty(tag.getKey()) || Utils.isStripEmpty(tag.getValue())) {
                    image.put(tag.getKey(), null);
                }
            }
            final String sequence = MapillaryImageUtils.getSequenceKey(image);
            // Reset the instant
            image.setInstant(Instant.EPOCH);
            // Re-cache the instant
            MapillaryImageUtils.getDate(image);
            if (json.containsKey(MapillaryImageUtils.ImageProperties.DETECTIONS.toString())) {
                ImageDetection.addDetections(id,
                    () -> JsonDecoder.decodeData(
                        json.getJsonObject(MapillaryImageUtils.ImageProperties.DETECTIONS.toString()),
                        j -> JsonImageDetectionDecoder.decodeImageDetection(j, id)));
            }
            return Pair.create(sequence, image);
        }
        return null;
    }

    private static LatLon getCoordinates(JsonObject json, boolean useComputedData) {
        final LatLon originalCoordinates = JsonDecoder.decodeLatLon(
            json.getJsonObject(MapillaryImageUtils.ImageProperties.GEOMETRY.toString()).getJsonArray("coordinates"));
        if (useComputedData && json.containsKey(MapillaryImageUtils.ImageProperties.COMPUTED_GEOMETRY.toString())) {
            final LatLon computedCoordinates = JsonDecoder
                .decodeLatLon(json.getJsonObject(MapillaryImageUtils.ImageProperties.COMPUTED_GEOMETRY.toString())
                    .getJsonArray("coordinates"));
            if (computedCoordinates != null && originalCoordinates != null && computedCoordinates
                .greatCircleDistance((ILatLon) originalCoordinates) < MapillaryProperties.ASSUMED_HDOP.get()) {
                return computedCoordinates;
            }
            return Utils.firstNonNull(originalCoordinates, computedCoordinates);
        }
        return originalCoordinates;
    }

    /**
     * Create a new image from a json
     *
     * @param json The json with the data
     * @param coordinates The coordinates of the image
     * @return A new image
     */
    @Nonnull
    private static MapillaryNode createNewImage(@Nonnull final JsonObject json, @Nullable final LatLon coordinates) {
        MapillaryNode tImage = new MapillaryNode();
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
        final long id = MapillaryImageUtils.getKey(tImage);
        if (id != 0) {
            tImage.setOsmId(id, 1);
        }
        return tImage;
    }
}
