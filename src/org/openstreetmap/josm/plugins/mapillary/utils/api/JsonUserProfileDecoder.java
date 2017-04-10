package org.openstreetmap.josm.plugins.mapillary.utils.api;

import java.io.IOException;
import java.net.URL;

import javax.imageio.ImageIO;
import javax.json.JsonObject;
import javax.swing.ImageIcon;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.plugins.mapillary.model.UserProfile;
import org.openstreetmap.josm.tools.ImageProvider;

public final class JsonUserProfileDecoder {
  private JsonUserProfileDecoder() {
    // Private constructor to avoid instantiation
  }
  public static UserProfile decodeUserProfile(JsonObject json) {
    if (json != null) {
      String avatar = json.getString("avatar", null);
      String username = json.getString("username", null);
      String key = json.getString("key", null);
      if(avatar != null) {
        ImageIcon icon;
        try {
          icon = new ImageIcon(ImageIO.read(new URL(avatar)));
        } catch (IOException e) {
          Main.debug(e);
          icon = (new ImageProvider("fake-avatar")).get();
        }

        if(key != null && username != null) {
          return new UserProfile(key, username, icon);
        }
      }
     }
     return null;
   }
}
