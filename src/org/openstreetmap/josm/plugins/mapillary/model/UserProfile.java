// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.model;

import javax.swing.ImageIcon;

public class UserProfile extends KeyIndexedObject {
  private static final long serialVersionUID = 4695718596660412790L;
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
