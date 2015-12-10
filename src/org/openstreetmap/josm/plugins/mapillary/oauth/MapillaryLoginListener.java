package org.openstreetmap.josm.plugins.mapillary.oauth;

public interface MapillaryLoginListener {
  public void onLogin(final String username);
  public void onLogout();
}
