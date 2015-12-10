// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.io.IOException;
import java.net.URL;

import javax.json.Json;
import javax.swing.AbstractAction;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.gui.preferences.PreferenceTabbedPane;
import org.openstreetmap.josm.gui.preferences.SubPreferenceSetting;
import org.openstreetmap.josm.gui.preferences.TabPreferenceSetting;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.io.download.MapillaryDownloader;
import org.openstreetmap.josm.plugins.mapillary.oauth.MapillaryLoginListener;
import org.openstreetmap.josm.plugins.mapillary.oauth.MapillaryUser;
import org.openstreetmap.josm.plugins.mapillary.oauth.OAuthPortListener;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;
import org.openstreetmap.josm.tools.GBC;
import org.openstreetmap.josm.tools.I18n;

/**
 * Creates the preferences panel for the plugin.
 *
 * @author nokutu
 *
 */
public class MapillaryPreferenceSetting implements SubPreferenceSetting, MapillaryLoginListener {

  private JCheckBox reverseButtons = new JCheckBox(
      tr("Reverse buttons position when displaying images."));
  private JComboBox<String> downloadMode = new JComboBox<>(new String[]{
      MapillaryDownloader.MODES.Automatic.toString(),
      MapillaryDownloader.MODES.Semiautomatic.toString(),
      MapillaryDownloader.MODES.Manual.toString()
  });
  private JCheckBox displayHour = new JCheckBox(
      tr("Display hour when the picture was taken"));
  private JCheckBox format24 = new JCheckBox(tr("Use 24 hour format"));
  private JCheckBox moveTo = new JCheckBox(
      tr("Move to picture''s location with next/previous buttons"));
  private JButton login;

  private JButton loginButton = new JButton(new LoginAction(this));
  private JButton logoutButton = new JButton(new LogoutAction());
  private JLabel loginLabel = new JLabel();
  private JPanel loginPanel = new JPanel();

  @Override
  public TabPreferenceSetting getTabPreferenceSetting(PreferenceTabbedPane gui) {
    return gui.getDisplayPreference();
  }

  @Override
  public void addGui(PreferenceTabbedPane gui) {
    JPanel panel = new JPanel();
    this.reverseButtons.setSelected(Main.pref
        .getBoolean("mapillary.reverse-buttons"));
    this.displayHour.setSelected(Main.pref.getBoolean("mapillary.display-hour",
        true));
    this.format24.setSelected(Main.pref.getBoolean("mapillary.format-24"));
    this.moveTo.setSelected(Main.pref.getBoolean("mapillary.move-to-picture",
        true));
    panel.setLayout(new FlowLayout(FlowLayout.LEFT));
    panel.add(this.reverseButtons);
    // Sets the value of the ComboBox.
    if (Main.pref.get("mapillary.download-mode").equals(MapillaryDownloader.MODES.Automatic.toString())
        || Main.pref.get("mapillary.download-mode").equals(MapillaryDownloader.MODES.Semiautomatic.toString())
        || Main.pref.get("mapillary.download-mode").equals(MapillaryDownloader.MODES.Manual.toString())) {
      this.downloadMode.setSelectedItem(Main.pref.get("mapillary.download-mode"));
    }
    JPanel downloadModePanel = new JPanel();
    downloadModePanel.add(new JLabel(tr("Download mode: ")));
    downloadModePanel.add(this.downloadMode);
    panel.add(downloadModePanel);
    panel.add(this.displayHour);
    panel.add(this.format24);
    panel.add(this.moveTo);

    loginPanel.setLayout(new FlowLayout(FlowLayout.LEADING));
    loginPanel.add(loginButton, 0);
    loginPanel.add(loginLabel, 1);
    onLogout();
    panel.add(loginPanel, GBC.eol());
    panel.add(Box.createVerticalGlue(), GBC.eol().fill(GBC.BOTH));

    gui.getDisplayPreference().addSubTab(this, "Mapillary", panel);

    new Thread(new Runnable() {
      @Override
      public void run() {
        String username = MapillaryUser.getUsername();
        if (username != null) {
          SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
              onLogin(MapillaryUser.getUsername());
            }
          });
        }
      }
    }).start();
  }

  /**
   * Should be called whenever the user logs into a mapillary account.
   * This updates the GUI to reflect the login status.
   * @param username the username that the user is now logged in with
   */
  public void onLogin(final String username) {
    loginPanel.add(logoutButton, 1);
    loginLabel.setText(I18n.tr("You are logged in as ''{0}''.", username));
    loginButton.setText(I18n.tr("Re-Login"));
    logoutButton.setText(I18n.tr("Logout"));
  }

  /**
   * Should be called whenever the user logs out of a mapillary account.
   * This updates the GUI to reflect the login status.
   */
  public void onLogout() {
    loginPanel.remove(logoutButton);
    loginLabel.setText(I18n.tr("You are currently not logged in."));
    loginButton.setText(I18n.tr("Login"));
  }

  @Override
  public boolean ok() {
    boolean mod = false;
    Main.pref
        .put("mapillary.reverse-buttons", this.reverseButtons.isSelected());

    MapillaryPlugin.setMenuEnabled(MapillaryPlugin.getDownloadViewMenu(), false);
    if (this.downloadMode.getSelectedItem().equals(MapillaryDownloader.MODES.Automatic.toString()))
      Main.pref.put("mapillary.download-mode", MapillaryDownloader.MODES.Automatic.toString());
    if (this.downloadMode.getSelectedItem().equals(MapillaryDownloader.MODES.Semiautomatic.toString()))
      Main.pref.put("mapillary.download-mode", MapillaryDownloader.MODES.Semiautomatic.toString());
    if (this.downloadMode.getSelectedItem().equals(MapillaryDownloader.MODES.Manual.toString())) {
      Main.pref.put("mapillary.download-mode", MapillaryDownloader.MODES.Manual.toString());
      MapillaryPlugin.setMenuEnabled(MapillaryPlugin.getDownloadViewMenu(), true);
    }
    Main.pref.put("mapillary.display-hour", this.displayHour.isSelected());
    Main.pref.put("mapillary.format-24", this.format24.isSelected());
    Main.pref.put("mapillary.move-to-picture", this.moveTo.isSelected());
    return mod;
  }

  @Override
  public boolean isExpert() {
    return false;
  }

  /**
   * Opens the MapillaryOAuthUI window and lets the user log in.
   *
   * @author nokutu
   *
   */
  public class LoginAction extends AbstractAction {
    private static final long serialVersionUID = -3908477563072057344L;
    final transient MapillaryLoginListener callback;

    public LoginAction(MapillaryLoginListener loginCallback) {
      this.callback = loginCallback;
    }

    @Override
    public void actionPerformed(ActionEvent arg0) {
      OAuthPortListener portListener = new OAuthPortListener(callback);
      portListener.start();
      String url = "http://www.mapillary.com/connect?redirect_uri=http:%2F%2Flocalhost:"+OAuthPortListener.PORT+"%2F&client_id="+MapillaryPlugin.CLIENT_ID+"&response_type=token&scope=user:read%20public:upload%20public:write";
      try {
        MapillaryUtils.browse(new URL(url));
      } catch (IOException e) {
        Main.error(e);
      }
    }
  }

  /**
   * Logs the user out.
   *
   * @author nokutu
   *
   */
  public class LogoutAction extends AbstractAction {

    private static final long serialVersionUID = 3434780936404707219L;

    @Override
    public void actionPerformed(ActionEvent arg0) {
      MapillaryUser.reset();
      Main.pref.put("mapillary.access-token", null);
      onLogout();
    }
  }
}
