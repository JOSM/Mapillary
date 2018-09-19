// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.oauth;

/**
 * This interface should be implemented by components that want to get notified when the user
 * logs in or logs out of his Mapillary account.
 * Such listeners can be registered e.g. at a {@link OAuthPortListener}.
 */
public interface MapillaryLoginListener {
  /**
   * Should be called whenever the user logs into a mapillary account.
   * E.g. for updating the GUI to reflect the login status.
   * @param username the username that the user is now logged in with
   */
  void onLogin(final String username);
  /**
   * Should be called whenever the user logs out of a mapillary account.
   * E.g. for updating the GUI to reflect the login status.
   */
  void onLogout();
}
