// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.json.JsonArray;
import jakarta.json.JsonNumber;
import jakarta.json.JsonObject;
import jakarta.json.JsonValue;
import org.openstreetmap.gui.jmapviewer.interfaces.MapObject;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.data.osm.IPrimitive;
import org.openstreetmap.josm.data.vector.DataLayer;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryMapFeatureUtils;

/**
 * Decodes the JSON returned by {@link org.openstreetmap.josm.plugins.mapillary.spi.preferences.IMapillaryUrls} into
 * Java objects.
 * Takes a {@link JsonObject} and {@link #decodeMapFeatureObject(JsonValue, IPrimitive)} tries to convert it to a
 * {@link MapObject}.
 */
public final class JsonMapObjectDecoder {
    private static final String COORDINATES = "coordinates";

    private JsonMapObjectDecoder() {
        // Private constructor to avoid instantiation
    }

    /**
     * Decode a feature object
     *
     * @param json The object json
     * @param primitive The primitive
     * @return The primitive with added values as a singleton collection
     * @param <T> The primitive type
     */
    @Nonnull
    public static <T extends IPrimitive> Collection<T> decodeMapFeatureObject(@Nullable final JsonValue json,
        @Nullable final T primitive) {
        // We don't have a collection endpoint yet, so we should only be getting objects
        if (json == null || primitive == null || json.getValueType() != JsonValue.ValueType.OBJECT) {
            return Collections.emptyList();
        }
        final JsonObject jsonObject = json.asJsonObject();
        final Map<String, String> tags = JsonTagMapDecoder.getTagMap(jsonObject);
        // Remove duplicate tags -- specifically, remove the first/last seen at (ms since epoch -> timestamp in
        // YYYY-mm-DDTHH:MM:SS)
        for (MapillaryMapFeatureUtils.MapFeatureProperties entry : MapillaryMapFeatureUtils.MapFeatureProperties
            .values()) {
            final String entryString = entry.toString();
            if (tags.containsKey(entryString) && primitive.hasKey(entryString)) {
                tags.remove(entryString);
            }
        }
        // Update the layer if necessary
        if (primitive instanceof DataLayer && ((DataLayer<?>) primitive).getLayer() instanceof String
            && tags.containsKey(MapillaryMapFeatureUtils.MapFeatureProperties.OBJECT_TYPE.toString())) {
            // We've checked that the return type of the layer is a string
            @SuppressWarnings("unchecked")
            final DataLayer<String> dataLayer = (DataLayer<String>) primitive;
            dataLayer.setLayer(tags.get(MapillaryMapFeatureUtils.MapFeatureProperties.OBJECT_TYPE.toString()));
            tags.remove(MapillaryMapFeatureUtils.MapFeatureProperties.OBJECT_TYPE.toString());
        }
        // Update value (object_value and value seem to be equivalent)
        if (jsonObject.containsKey(MapillaryMapFeatureUtils.MapFeatureProperties.OBJECT_VALUE.toString())) {
            final String value = tags.remove(MapillaryMapFeatureUtils.MapFeatureProperties.OBJECT_VALUE.toString());
            primitive.put(MapillaryMapFeatureUtils.MapFeatureProperties.VALUE.toString(), value);
        }

        // Update geometry
        updateGeometry(jsonObject, primitive, tags);
        // Set image ids
        updateImageKeys(jsonObject, tags);
        // Add remaining tags to primitive
        if (jsonObject.containsKey(MapillaryMapFeatureUtils.MapFeatureProperties.ID.toString())) {
            long id = Long
                .parseLong(jsonObject.getString(MapillaryMapFeatureUtils.MapFeatureProperties.ID.toString(), "0"));
            primitive.setOsmId(id, 1);
        }
        tags.forEach(primitive::put);
        return Collections.singletonList(primitive);
    }

    /**
     * Update the geometry of a primitive
     *
     * @param jsonObject The json with the updated coordinates
     * @param primitive The primitive to update
     * @param tags The tags to remove extraneous geometry information from
     */
    private static void updateGeometry(@Nonnull final JsonObject jsonObject, @Nonnull final IPrimitive primitive,
        @Nonnull final Map<String, String> tags) {
        if (jsonObject.containsKey(MapillaryMapFeatureUtils.MapFeatureProperties.GEOMETRY.toString())) {
            tags.remove(MapillaryMapFeatureUtils.MapFeatureProperties.GEOMETRY.toString());
            final JsonObject geometry = jsonObject
                .getJsonObject(MapillaryMapFeatureUtils.MapFeatureProperties.GEOMETRY.toString());
            if ("Point".equals(geometry.getString("type", null)) && primitive instanceof INode
                && geometry.containsKey(COORDINATES)
                && geometry.get(COORDINATES).getValueType() == JsonValue.ValueType.ARRAY
                && geometry.getJsonArray(COORDINATES).size() == 2
                && geometry.getJsonArray(COORDINATES).stream().allMatch(JsonNumber.class::isInstance)) {
                final JsonArray jsonArray = geometry.getJsonArray(COORDINATES);
                ((INode) primitive).setCoor(
                    new LatLon(jsonArray.getJsonNumber(1).doubleValue(), jsonArray.getJsonNumber(0).doubleValue()));
            } else {
                throw new IllegalArgumentException(MessageFormat.format("Mapillary: Bad geometry for {0}: {1}",
                    primitive.getClass().getSimpleName(), geometry));
            }
        }
    }

    /**
     * Update the image keys
     *
     * @param jsonObject The json object with the image information
     * @param tags The tag map to update
     */
    private static void updateImageKeys(@Nonnull final JsonObject jsonObject, @Nonnull final Map<String, String> tags) {
        final String imagesKey = MapillaryMapFeatureUtils.MapFeatureProperties.IMAGES.toString();
        if (jsonObject.containsKey(imagesKey)
            && jsonObject.get(imagesKey).getValueType() == JsonValue.ValueType.OBJECT) {
            tags.remove(imagesKey);
            tags.put(imagesKey, String.join(",",
                JsonDecoder.decodeData(jsonObject.getJsonObject(imagesKey), JsonMapObjectDecoder::parseImages)));
        }
    }

    /**
     * Parse images from a json value
     *
     * @param value The value to parse
     * @return A collection of image ids
     */
    private static Collection<String> parseImages(final JsonValue value) {
        if (value.getValueType() == JsonValue.ValueType.ARRAY) {
            final List<String> parsedImages = new ArrayList<>(value.asJsonArray().size());
            for (final JsonValue tValue : value.asJsonArray()) {
                final Collection<String> tParsedImages = parseImages(tValue);
                parsedImages.addAll(tParsedImages.stream().filter(Objects::nonNull).collect(Collectors.toList()));
            }
            return Collections.unmodifiableList(parsedImages);
        } else if (value.getValueType() == JsonValue.ValueType.OBJECT
            && value.asJsonObject().containsKey(MapillaryImageUtils.ImageProperties.ID.toString())) {
            // Technically, we could get/create the MapillaryLayer, and add the images if needed.
            // The image information also contains a geometry field (probably not computed)
            return Collections
                .singletonList(value.asJsonObject().getString(MapillaryImageUtils.ImageProperties.ID.toString()));
        }
        return Collections.emptyList();
    }
}
