// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.model;

import static org.openstreetmap.josm.plugins.mapillary.cache.Caches.USER_PROFILE_CACHE;

import java.io.Serializable;
import java.io.StringReader;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javax.swing.ImageIcon;

import jakarta.json.Json;
import org.openstreetmap.josm.data.imagery.vectortile.mapbox.MVTTile;
import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.plugins.mapillary.spi.preferences.MapillaryConfig;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.api.JsonUserProfileDecoder;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.ListenerList;

/**
 * A profile for a user
 *
 * @param key The user id
 * @param username The username for the user
 * @param avatar The avatar for the user
 */
public record UserProfile(long key, String username, ImageIcon avatar) implements Serializable {

    private static final Map<Long, UserProfile> CACHE = new ConcurrentHashMap<>(1);
    private static final ListenerList<UserProfileListener> LISTENERS = ListenerList.create();
    /** A default user profile */
    public static final UserProfile NONE = new UserProfile(Long.MIN_VALUE, "",
        ImageProvider.createBlankIcon(ImageProvider.ImageSizes.DEFAULT));

    static {
        CACHE.put(NONE.key(), NONE);
    }

    /**
     * Get a user from a json string
     *
     * @param json The json string
     * @return The user profile
     */
    public static UserProfile getUser(String json) {
        final UserProfile user;
        try (var reader = Json.createReader(new StringReader(json))) {
            user = JsonUserProfileDecoder.decodeUserProfile(reader.readObject());
        }
        return CACHE.computeIfAbsent(user.key(), ignored -> user);
    }

    /**
     * Get the user given a user id
     *
     * @param id The user id
     * @return The user profile
     */
    public static UserProfile getUser(long id) {
        final var user = CACHE.computeIfAbsent(id, UserProfile::getNewUser);
        if (NONE.equals(user)) {
            CACHE.remove(id);
        }
        return user;
    }

    private static UserProfile getNewUser(long id) {
        final var userProfile = USER_PROFILE_CACHE.get(MapillaryConfig.getUrls().getUserInformation(id).toString());
        if (userProfile == null) {
            return NONE;
        }
        LISTENERS.fireEvent(e -> e.userProfileAdded(userProfile));
        return userProfile;
    }

    /**
     * Get the currently downloaded and cached users
     *
     * @return The users
     */
    public static Collection<UserProfile> getUsers() {
        return Collections.unmodifiableCollection(CACHE.values());
    }

    /**
     * Read organizations from a tile and add them to the list
     *
     * @param tile The tile to read
     */
    public static void addFromTile(MVTTile tile) {
        tile.getData().getAllPrimitives().stream().filter(INode.class::isInstance).map(INode.class::cast)
            .forEach(MapillaryImageUtils::getUser);
    }

    /**
     * Add a new user listener
     *
     * @param listener The listener to call
     */
    public static void addUserProfileListener(UserProfileListener listener) {
        LISTENERS.addListener(listener);
    }

    /**
     * Remove a user listener
     *
     * @param listener the listner to remove
     */
    public static void removeUserProfileListener(UserProfileListener listener) {
        LISTENERS.removeListener(listener);
    }

    /**
     * The interface for listening for new organizations
     */
    public interface UserProfileListener extends Serializable {
        /**
         * Called when a new user is added
         *
         * @param userProfile The added user profile
         */
        void userProfileAdded(UserProfile userProfile);
    }
}
