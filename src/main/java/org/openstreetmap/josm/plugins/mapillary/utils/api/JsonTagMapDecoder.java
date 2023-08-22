// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import java.util.Map;

import jakarta.json.JsonObject;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;
import org.openstreetmap.josm.data.osm.TagMap;

/**
 * Convert a properties object into a TagMap
 */
public final class JsonTagMapDecoder {
    private JsonTagMapDecoder() {
        // Utility class, shouldn't be instantiated
    }

    /**
     * Get a tag map for the values
     *
     * @param json The json to convert to a tag map (ignores sub arrays and sub objects)
     * @return The tagmap
     */
    public static Map<String, String> getTagMap(JsonObject json) {
        TagMap tags = new TagMap();
        for (Map.Entry<String, JsonValue> entry : json.entrySet()) {
            JsonValue.ValueType type = entry.getValue().getValueType();
            if (type == JsonValue.ValueType.STRING) {
                tags.put(entry.getKey(), ((JsonString) entry.getValue()).getString());
            } else if (type == JsonValue.ValueType.TRUE) {
                tags.put(entry.getKey(), Boolean.TRUE.toString());
            } else if (type == JsonValue.ValueType.FALSE) {
                tags.put(entry.getKey(), Boolean.FALSE.toString());
            } else if (type == JsonValue.ValueType.NUMBER) {
                tags.put(entry.getKey(), entry.getValue().toString());
            } else {
                tags.put(entry.getKey(), entry.getValue().toString());
            }
        }
        return tags;
    }
}
