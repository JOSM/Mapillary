// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import java.awt.image.BufferedImage;
import java.io.IOException;
import java.net.URI;

import javax.imageio.ImageIO;
import javax.swing.ImageIcon;

import org.openstreetmap.josm.plugins.mapillary.model.UserProfile;
import org.openstreetmap.josm.plugins.mapillary.spi.preferences.IMapillaryUrls;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.Logging;

import jakarta.json.JsonObject;

/**
 * Decodes the JSON returned by {@link IMapillaryUrls} into Java objects.
 * Takes a {@link JsonObject} and {@link #decodeUserProfile(JsonObject)} tries to convert it to a {@link UserProfile}.
 */
public final class JsonUserProfileDecoder {
    /** The avatar for profiles without an avatar */
    private static final ImageIcon FAKE_AVATAR = new ImageProvider("fake-avatar").get();

    private JsonUserProfileDecoder() {
        // Private constructor to avoid instantiation
    }

    /**
     * Decode the user profile
     *
     * @param json The user profile json object
     * @return The decoded profile
     */
    public static UserProfile decodeUserProfile(JsonObject json) {
        if (json == null) {
            return null;
        }
        final var username = json.getString("username", null);
        final var key = json.getString("id", null);
        if (key == null || username == null) {
            return null;
        }

        final var avatar = json.getString("avatar", null);
        ImageIcon icon = null;
        if (avatar != null) {
            try {
                BufferedImage img = ImageIO.read(URI.create(avatar).toURL());
                if (img != null) {
                    icon = new ImageIcon(img);
                }
            } catch (IOException e) {
                Logging.debug(e);
            }
        }
        if (icon == null) {
            icon = FAKE_AVATAR;
        }
        return new UserProfile(Long.parseLong(key), username, icon);
    }
}
