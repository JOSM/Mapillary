// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Locale;

import javax.json.Json;
import javax.json.JsonObjectBuilder;

import org.openstreetmap.josm.plugins.mapillary.model.ImageDetection;
import org.openstreetmap.josm.plugins.mapillary.oauth.OAuthUtils;
import org.openstreetmap.josm.tools.HttpClient;
import org.openstreetmap.josm.tools.Logging;

/**
 * Vote for/against a detection
 */
public final class DetectionVerification {
    /** The type of vote to make */
    public enum TYPE {
        /** Approve the detection */
        APPROVE,
        /** Reject the detection */
        REJECT;

        @Override
        public String toString() {
            return this.name().toLowerCase(Locale.ENGLISH);
        }
    }

    /**
     * Vote for a detection
     *
     * @param detection The detection to vote for
     * @param type The type of vote to make
     * @return {@code true} if successful
     */
    public static boolean vote(ImageDetection<?> detection, TYPE type) {
        if (true) {
            throw new UnsupportedOperationException("Mapillary: v4 API does not currently support voting");
        }
        HttpClient client = HttpClient.create(MapillaryURL.APIv3
            .vote(detection.isTrafficSign() ? "traffic_sign" : "FIXME", Long.toString(detection.getKey())), "POST");
        client.setHeader("Content-Type", "application/json");
        JsonObjectBuilder builder = Json.createObjectBuilder();
        builder.add("change_type", type.toString());
        client.setRequestBody(builder.build().toString().getBytes(StandardCharsets.UTF_8));
        OAuthUtils.addAuthenticationHeader(client);
        try {
            client.connect();
            return client.getResponse().getResponseCode() == 202;
        } catch (IOException e) {
            Logging.error(e);
            return false;
        } finally {
            client.disconnect();
        }
    }

    private DetectionVerification() {
        // Don't instantiate
    }
}
