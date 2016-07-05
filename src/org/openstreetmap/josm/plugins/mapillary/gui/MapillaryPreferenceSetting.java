// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.io.IOException;
import java.io.InputStream;

import javax.imageio.ImageIO;
import javax.swing.AbstractAction;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
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
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryColorScheme;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryColorScheme.MapillaryButton;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL;
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

  private final JCheckBox reverseButtons = new JCheckBox(I18n.tr("Reverse buttons position when displaying images."));
  private final JComboBox<String> downloadModeComboBox = new JComboBox<>(new String[]{
      MapillaryDownloader.MODES.Automatic.toString(),
      MapillaryDownloader.MODES.Semiautomatic.toString(),
      MapillaryDownloader.MODES.Manual.toString()
  });
  private final JCheckBox displayHour = new JCheckBox(I18n.tr("Display hour when the picture was taken"));
  private final JCheckBox format24 = new JCheckBox(I18n.tr("Use 24 hour format"));
  private final JCheckBox moveTo = new JCheckBox(I18n.tr("Move to picture''s location with next/previous buttons"));
  private final JCheckBox hoverEnabled = new JCheckBox(I18n.tr("Preview images when hovering its icon"));

  private final JButton loginButton = new MapillaryButton(I18n.tr("Login"), new LoginAction(this));
  private final JButton logoutButton = new MapillaryButton(I18n.tr("Logout"), new LogoutAction());
  private final JLabel loginLabel = new JLabel();
  private final JPanel loginPanel = new JPanel();

  @Override
  public TabPreferenceSetting getTabPreferenceSetting(PreferenceTabbedPane gui) {
    return gui.getDisplayPreference();
  }

  @Override
  public void addGui(PreferenceTabbedPane gui) {
    JPanel container = new JPanel(new BorderLayout());

    loginPanel.setLayout(new BoxLayout(loginPanel, BoxLayout.LINE_AXIS));
    loginPanel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
    loginPanel.setBackground(MapillaryColorScheme.TOOLBAR_DARK_GREY);
    JLabel brandImage = new JLabel();
    try (InputStream is = MapillaryPreferenceSetting.class.getResourceAsStream("/images/mapillary-logo-white.png")) {
      if (is != null) {
        brandImage.setIcon(new ImageIcon(ImageIO.read(is)));
      } else {
        Main.warn("Could not load Mapillary brand image!");
      }
    } catch (IOException e) {
      Main.warn("While reading Mapillary brand image, an IO-exception occured!");
    }
    loginPanel.add(brandImage, 0);
    loginPanel.add(Box.createHorizontalGlue(), 1);
    loginLabel.setForeground(Color.WHITE);
    loginLabel.setBorder(BorderFactory.createEmptyBorder(0, 10, 0, 10));
    loginPanel.add(loginLabel, 2);
    loginPanel.add(loginButton, 3);
    onLogout();
    container.add(loginPanel, BorderLayout.NORTH);

    JPanel mainPanel = new JPanel();
    reverseButtons.setSelected(Main.pref.getBoolean("mapillary.reverse-buttons"));
    displayHour.setSelected(Main.pref.getBoolean("mapillary.display-hour", true));
    format24.setSelected(Main.pref.getBoolean("mapillary.format-24"));
    moveTo.setSelected(Main.pref.getBoolean("mapillary.move-to-picture", true));
        this.hoverEnabled.setSelected(Main.pref.getBoolean("mapillary.hover-enabled", true));

    mainPanel.setLayout(new GridBagLayout());
    mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

    // Sets the value of the ComboBox.
    String downloadMode = Main.pref.get("mapillary.download-mode");
    if (MapillaryDownloader.MODES.Automatic.toString().equals(downloadMode)
        || MapillaryDownloader.MODES.Semiautomatic.toString().equals(downloadMode)
        || MapillaryDownloader.MODES.Manual.toString().equals(downloadMode)) {
      downloadModeComboBox.setSelectedItem(Main.pref.get("mapillary.download-mode"));
    }
    JPanel downloadModePanel = new JPanel();
    downloadModePanel.add(new JLabel(I18n.tr("Download mode")));
    downloadModePanel.add(downloadModeComboBox);
    mainPanel.add(downloadModePanel, GBC.eol());

    mainPanel.add(reverseButtons, GBC.eol());
    mainPanel.add(displayHour, GBC.eol());
    mainPanel.add(format24, GBC.eol());
    mainPanel.add(moveTo, GBC.eol());
    mainPanel.add(hoverEnabled, GBC.eol());
    MapillaryColorScheme.styleAsDefaultPanel(
      mainPanel, downloadModePanel, reverseButtons, displayHour, format24, moveTo
    );
    mainPanel.add(Box.createVerticalGlue(), GBC.eol().fill(GridBagConstraints.BOTH));

    container.add(mainPanel, BorderLayout.CENTER);

    synchronized (gui.getDisplayPreference().getTabPane()) {
      gui.getDisplayPreference().addSubTab(this, "Mapillary", new JScrollPane(container));
      gui.getDisplayPreference().getTabPane().setIconAt(gui.getDisplayPreference().getTabPane().getTabCount()-1, MapillaryPlugin.ICON12);
    }

    new Thread( new Runnable() {
      @Override
      public void run() {
        String username = MapillaryUser.getUsername();
        if (username != null) {
          SwingUtilities.invokeLater( new Runnable() {
            @Override
            public void run() {
              onLogin(MapillaryUser.getUsername());
            }
          } );
        }
      }
    }).start();
  }

  @Override
  public void onLogin(final String username) {
    loginPanel.remove(loginButton);
    loginPanel.add(logoutButton, 3);
    loginLabel.setText(I18n.tr("You are logged in as ''{0}''.", username));
    loginPanel.revalidate();
    loginPanel.repaint();
  }

  @Override
  public void onLogout() {
    loginPanel.remove(logoutButton);
    loginPanel.add(loginButton, 3);
    loginLabel.setText(I18n.tr("You are currently not logged in."));
    loginPanel.revalidate();
    loginPanel.repaint();
  }

  @SuppressWarnings("PMD.ShortMethodName")
  @Override
  public boolean ok() {
    boolean mod = false;
    Main.pref.put("mapillary.reverse-buttons", this.reverseButtons.isSelected());

    MapillaryPlugin.setMenuEnabled(MapillaryPlugin.getDownloadViewMenu(), false);
    if (this.downloadModeComboBox.getSelectedItem().equals(MapillaryDownloader.MODES.Automatic.toString()))
      Main.pref.put("mapillary.download-mode", MapillaryDownloader.MODES.Automatic.toString());
    if (this.downloadModeComboBox.getSelectedItem().equals(MapillaryDownloader.MODES.Semiautomatic.toString()))
      Main.pref.put("mapillary.download-mode", MapillaryDownloader.MODES.Semiautomatic.toString());
    if (this.downloadModeComboBox.getSelectedItem().equals(MapillaryDownloader.MODES.Manual.toString())) {
      Main.pref.put("mapillary.download-mode", MapillaryDownloader.MODES.Manual.toString());
      MapillaryPlugin.setMenuEnabled(MapillaryPlugin.getDownloadViewMenu(), true);
    }
    Main.pref.put("mapillary.display-hour", this.displayHour.isSelected());
    Main.pref.put("mapillary.format-24", this.format24.isSelected());
    Main.pref.put("mapillary.move-to-picture", this.moveTo.isSelected());
    Main.pref.put("mapillary.hover-enabled", this.hoverEnabled.isSelected());
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
    private final transient MapillaryLoginListener callback;

    public LoginAction(MapillaryLoginListener loginCallback) {
      this.callback = loginCallback;
    }

    @Override
    public void actionPerformed(ActionEvent arg0) {
      OAuthPortListener portListener = new OAuthPortListener(callback);
      portListener.start();
      try {
        MapillaryUtils.browse(MapillaryURL.connectURL("http://localhost:"+OAuthPortListener.PORT+'/'));
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
