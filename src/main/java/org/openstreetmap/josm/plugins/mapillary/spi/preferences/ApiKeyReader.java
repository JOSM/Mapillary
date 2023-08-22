// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.spi.preferences;

import java.io.IOException;
import java.io.InputStream;

import jakarta.json.Json;
import jakarta.json.JsonObject;
import jakarta.json.JsonReader;
import org.openstreetmap.josm.spi.preferences.Config;
import org.openstreetmap.josm.tools.Logging;
import org.openstreetmap.josm.tools.ResourceProvider;
import org.openstreetmap.josm.tools.Utils;

/**
 * Read api keys from a system property, then JOSM preferences, and finally from an API key file in the jar.
 */
public final class ApiKeyReader {
    private ApiKeyReader() {
        /* Hide constructor */ }

    private static final String API_KEY_FILE = "mapillary_api_keys.json";

    static String readValue(final String key) {
        // Prefer system property (this is something that can be changed fairly easily)
        if (System.getProperty(key) != null) {
            return System.getProperty(key);
        }
        // Then check if there was something stored in JOSM preferences
        if (Config.getPref() != null && !Utils.isBlank(Config.getPref().get("mapillary.api." + key))) {
            return Config.getPref().get("mapillary.api." + key);
        }
        // Then check and see if the api key file has the key
        final InputStream fileStream = ResourceProvider.getResourceAsStream(API_KEY_FILE);
        if (fileStream != null) {
            try (JsonReader reader = Json.createReader(fileStream)) {
                final JsonObject object = reader.readObject();
                if (object.containsKey(key)) {
                    return object.getString(key);
                }
            } finally {
                try {
                    fileStream.close();
                } catch (IOException e) {
                    Logging.error(e);
                }
            }
        }
        // Finally, return "test" no value has been found
        return "test";
    }

    static long readMapillaryClientId() {
        try {
            final String stringValue = readValue("MAPILLARY_CLIENT_ID");
            return Long.parseLong(stringValue);
        } catch (NumberFormatException numberFormatException) {
            Logging.error(numberFormatException);
        }
        return 4_280_585_711_960_869L;
    }
}
