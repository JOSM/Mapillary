package org.openstreetmap.josm.plugins.mapillary.utils.api;

import java.awt.image.BufferedImage;
import java.io.IOException;
import java.net.URL;

import javax.imageio.ImageIO;
import javax.json.JsonObject;
import javax.swing.ImageIcon;

import org.openstreetmap.josm.plugins.mapillary.model.UserProfile;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL.APIv3;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.Logging;

/**
 * Decodes the JSON returned by {@link APIv3} into Java objects.
 * Takes a {@link JsonObject} and {@link #decodeUserProfile(JsonObject)} tries to convert it to a {@link UserProfile}.
 */
public final class JsonUserProfileDecoder {
  private static final ImageIcon FAKE_AVATAR = new ImageProvider("fake-avatar").get();

  private JsonUserProfileDecoder() {
    // Private constructor to avoid instantiation
  }

  public static UserProfile decodeUserProfile(JsonObject json) {
    if (json == null) {
      return null;
    }
    String username = json.getString("username", null);
    String key = json.getString("key", null);
    if (key == null || username == null) {
      return null;
    }

    String avatar = json.getString("avatar", null);
    ImageIcon icon = null;
    if (avatar != null) {
      try {
        BufferedImage img = ImageIO.read(new URL(avatar));
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
    return new UserProfile(key, username, icon);
  }
}
