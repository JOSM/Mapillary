// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import jakarta.json.JsonArray;
import jakarta.json.JsonNumber;
import jakarta.json.JsonObject;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;
import org.openstreetmap.josm.data.osm.IWay;
import org.openstreetmap.josm.plugins.mapillary.spi.preferences.IMapillaryUrls;
import org.openstreetmap.josm.tools.Logging;

/**
 * Decodes the JSON returned by {@link IMapillaryUrls} into Java objects.
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
     *         information is missing from the JSON, or it's not meeting the expecting format in another way,
     *         an empty list will be returned.
     */
    public static List<Long> decodeSequence(final JsonValue json) {
        /*
         * The response looks like:
         * {"data":[{"id":"0"},{"id":"1"},{"id":"2"},...]}
         * We just have the "data" value
         */
        if (!(json instanceof JsonArray)) {
            Logging.error("Mapillary: The sequence endpoint just returns an array of picture ids");
            return Collections.emptyList();
        }
        return ((JsonArray) json).getValuesAs(value -> value instanceof JsonObject ? (JsonObject) value : null).stream()
            .filter(Objects::nonNull).filter(image -> image.containsKey("id")).map(image -> image.get("id"))
            .filter(jsonObject -> jsonObject instanceof JsonString || jsonObject instanceof JsonNumber)
            .map(value -> value instanceof JsonString ? Long.parseLong(((JsonString) value).getString())
                : ((JsonNumber) value).longValue())
            .distinct().collect(Collectors.toList());
    }
}
