// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Consumer;

import javax.annotation.Nullable;
import javax.json.JsonArray;
import javax.json.JsonNumber;
import javax.json.JsonObject;
import javax.json.JsonString;
import javax.json.JsonValue;

import org.openstreetmap.josm.data.osm.IWay;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.MapillaryNode;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.MapillarySequence;
import org.openstreetmap.josm.plugins.mapillary.io.download.MapillaryDownloader;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL;
import org.openstreetmap.josm.tools.Logging;

/**
 * Decodes the JSON returned by {@link MapillaryURL.APIv4} into Java objects.
 * Takes a {@link JsonObject} and {@link #decodeSequence(JsonValue)} tries to convert it to a
 * {@link IWay}.
 */
public final class JsonSequencesDecoder {
    private JsonSequencesDecoder() {
        // Private constructor to avoid instantiation
    }

    /**
     * Parses a given {@link JsonObject} as an array of image identifiers into a {@link IWay}.
     *
     * @param json the {@link JsonObject} to be parsed
     * @return a singleton list of {@link IWay} that is parsed from the given {@link JsonObject}. If mandatory
     *         information
     *         is
     *         missing from the JSON or it's not meeting the expecting format in another way, an empty list will be
     *         returned.
     */
    public static List<MapillarySequence> decodeSequence(final JsonValue json) {
        return decodeSequence(json, null, null);
    }

    /**
     * Parses a given {@link JsonObject} as an array of image identifiers into a {@link IWay}.
     *
     * @param prioritizedImage The image to prioritize
     * @param updater The method to call when the surrounding nodes have been pulled
     * @param json the {@link JsonObject} to be parsed
     * @return a singleton list of {@link IWay} that is parsed from the given {@link JsonObject}. If mandatory
     *         information
     *         is
     *         missing from the JSON or it's not meeting the expecting format in another way, an empty list will be
     *         returned.
     */
    public static List<MapillarySequence> decodeSequence(final JsonValue json,
        @Nullable final MapillaryNode prioritizedImage,
        @Nullable final Consumer<Collection<MapillarySequence>> updater) {
        /*
         * The response looks like:
         * {"data":[{"id":"0"},{"id":"1"},{"id":"2"},...]}
         * We just have the "data" value
         */
        if (!(json instanceof JsonArray)) {
            Logging.error("Mapillary: The sequence endpoint just returns an array of picture ids");
            return Collections.emptyList();
        }
        final long[] imageIds = ((JsonArray) json)
            .getValuesAs(value -> value instanceof JsonObject ? (JsonObject) value : null).stream()
            .filter(Objects::nonNull).filter(image -> image.containsKey("id")).map(image -> image.get("id"))
            .filter(jsonObject -> jsonObject instanceof JsonString || jsonObject instanceof JsonNumber)
            .mapToLong(value -> value instanceof JsonString ? Long.parseLong(((JsonString) value).getString())
                : ((JsonNumber) value).longValue())
            .distinct().toArray();
        Map<String, Collection<MapillaryNode>> images = MapillaryDownloader.downloadImages(prioritizedImage, updater,
            imageIds);
        if (images.size() > 1) {
            throw new IllegalStateException(
                "Images cannot belong to more than one sequence: " + String.join(", ", images.keySet()));
        } else if (images.isEmpty()) {
            Logging.error("No sequences in json");
            return Collections.emptyList();
        }
        Map.Entry<String, Collection<MapillaryNode>> nodes = images.entrySet().iterator().next();
        if (nodes.getValue().isEmpty()) {
            Logging.error("Mapillary: The sequence does not have any nodes");
            return Collections.emptyList();
        }
        final MapillarySequence sequence = new MapillarySequence(nodes.getKey());
        sequence.setNodes(new ArrayList<>(nodes.getValue()));
        return Collections.singletonList(sequence);
    }
}
