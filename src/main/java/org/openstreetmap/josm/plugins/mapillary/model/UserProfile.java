// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.model;

import java.io.IOException;
import java.io.StringReader;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javax.swing.ImageIcon;

import jakarta.json.Json;
import org.openstreetmap.josm.plugins.mapillary.oauth.OAuthUtils;
import org.openstreetmap.josm.plugins.mapillary.spi.preferences.MapillaryConfig;
import org.openstreetmap.josm.plugins.mapillary.utils.api.JsonUserProfileDecoder;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.Logging;

/**
 * A profile for a user
 *
 * @param key The user id
 * @param username The username for the user
 * @param avatar The avatar for the user
 */
public record UserProfile(long key, String username, ImageIcon avatar) {

    private static final Map<Long, UserProfile> CACHE = new ConcurrentHashMap<>(1);
    /** A default user profile */
    public static final UserProfile NONE = new UserProfile(Long.MIN_VALUE, "",
        ImageProvider.createBlankIcon(ImageProvider.ImageSizes.DEFAULT));

    static {
        CACHE.put(NONE.key(), NONE);
    }

    public static UserProfile getUser(String json) {
        final UserProfile user;
        try (var reader = Json.createReader(new StringReader(json))) {
            user = JsonUserProfileDecoder.decodeUserProfile(reader.readObject());
        }
        return CACHE.computeIfAbsent(user.key(), ignored -> user);
    }

    public static UserProfile getUser(long id) {
        final var user = CACHE.computeIfAbsent(id, UserProfile::getNewUser);
        if (NONE.equals(user)) {
            CACHE.remove(id);
        }
        return user;
    }

    private static UserProfile getNewUser(long id) {
        try {
            final var data = OAuthUtils.getWithHeader(MapillaryConfig.getUrls().getUserInformation(id));
            return JsonUserProfileDecoder.decodeUserProfile(data);
        } catch (IOException exception) {
            Logging.error(exception);
        }
        return NONE;
    }
}
