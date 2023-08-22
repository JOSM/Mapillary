// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.oauth;

import java.io.IOException;
import java.time.Instant;
import java.util.Arrays;
import java.util.Collections;
import java.util.Map;
import java.util.stream.Collectors;

import jakarta.json.JsonString;
import jakarta.json.JsonValue;
import org.openstreetmap.josm.data.preferences.AbstractProperty;
import org.openstreetmap.josm.plugins.mapillary.spi.preferences.MapillaryConfig;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.tools.ListenerList;
import org.openstreetmap.josm.tools.Logging;

/**
 * Represents the current logged in user and stores its data.
 *
 * @author nokutu
 */
public final class MapillaryUser {

    private static final ListenerList<MapillaryLoginListener> LISTENERS = ListenerList.create();
    private static String username;
    /** If the stored token is valid or not. */
    private static boolean isTokenValid = true;

    /** Various user information */
    private static Map<String, String> userInformation;

    private MapillaryUser() {
        // Private constructor to avoid instantiation
    }

    /**
     * Get the username of the current user
     *
     * @return The username of the logged in user.
     */
    public static synchronized String getUsername() {
        if (!isTokenValid) {
            return null;
        }
        Map<String, String> userInfo = getUserInformation();
        if (username == null && userInfo != null) {
            username = userInfo.get("username");
            LISTENERS.fireEvent(l -> l.onLogin(username));
        }
        return username;
    }

    /**
     * Get the information for the currently logged in user
     *
     * @return The user information of the logged in user.
     */
    public static synchronized Map<String, String> getUserInformation() {
        if (!isTokenValid) {
            return Collections.emptyMap();
        }
        if (userInformation == null) {
            try {
                userInformation = OAuthUtils.getWithHeader(MapillaryConfig.getUrls().getUserInformation()).entrySet()
                    .parallelStream().filter(e -> JsonValue.ValueType.STRING == e.getValue().getValueType())
                    .collect(Collectors.toMap(Map.Entry::getKey, e -> ((JsonString) e.getValue()).getString()));
            } catch (IOException e) {
                Logging.log(Logging.LEVEL_WARN, "Invalid Mapillary token, resetting field", e);
                reset();
            }
        }
        return userInformation == null ? Collections.emptyMap() : Collections.unmodifiableMap(userInformation);
    }

    /**
     * Resets the MapillaryUser to null values.
     */
    public static synchronized void reset() {
        username = null;
        userInformation = null;
        isTokenValid = false;
        for (AbstractProperty<?> property : Arrays.asList(MapillaryProperties.ACCESS_TOKEN,
            MapillaryProperties.ACCESS_TOKEN_EXPIRES_AT, MapillaryProperties.ACCESS_TOKEN_REFRESH_IN)) {
            property.remove();
        }
        LISTENERS.fireEvent(MapillaryLoginListener::onLogout);
    }

    /**
     * Check if the token exists and is valid
     *
     * @return {@code true} if the token is valid (currently only checks that we haven't passed the expiration date)
     */
    public static synchronized boolean isTokenValid() {
        if (!isTokenValid) {
            final boolean valid = MapillaryProperties.ACCESS_TOKEN.isSet()
                && Instant.now().getEpochSecond() < MapillaryProperties.ACCESS_TOKEN_EXPIRES_AT.get();
            setTokenValid(valid);
        }
        return isTokenValid;
    }

    /**
     * Set whether or not the token is valid
     *
     * @param value {@code true} if the token set is valid
     */
    public static synchronized void setTokenValid(boolean value) {
        isTokenValid = value;
    }

    public static void addListener(MapillaryLoginListener listener) {
        LISTENERS.addListener(listener);
    }

    public static void removeListener(MapillaryLoginListener listener) {
        LISTENERS.removeListener(listener);
    }
}
