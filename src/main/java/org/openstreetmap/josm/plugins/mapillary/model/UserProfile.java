// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.model;

import javax.swing.ImageIcon;

import org.openstreetmap.josm.tools.ImageProvider;

public class UserProfile extends KeyIndexedObject {
  private static final long serialVersionUID = -2626823438368139952L;

  /** A default user profile */
  public static final UserProfile NONE = new UserProfile("", "",
    ImageProvider.createBlankIcon(ImageProvider.ImageSizes.DEFAULT));

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
