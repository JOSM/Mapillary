// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.model;

import javax.swing.ImageIcon;

public class UserProfile extends KeyIndexedObject {
  private final String username;
  private final ImageIcon avatar;

  public UserProfile(String key, String username, ImageIcon avatar) {
    super(key);
    this.avatar = avatar;
    this.username = username;
  }

  public String getUsername() {
    return username;
  }

  public ImageIcon getAvatar() {
    return avatar;
  }
}
