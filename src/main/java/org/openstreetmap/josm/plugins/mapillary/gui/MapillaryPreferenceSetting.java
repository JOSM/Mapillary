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
import javax.swing.JSpinner;
import javax.swing.SpinnerNumberModel;
import javax.swing.SwingUtilities;

import org.openstreetmap.josm.actions.ExpertToggleAction;
import org.openstreetmap.josm.gui.preferences.PreferenceTabbedPane;
import org.openstreetmap.josm.gui.preferences.SubPreferenceSetting;
import org.openstreetmap.josm.gui.preferences.TabPreferenceSetting;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.gui.boilerplate.MapillaryButton;
import org.openstreetmap.josm.plugins.mapillary.io.download.MapillaryDownloader.DOWNLOAD_MODE;
import org.openstreetmap.josm.plugins.mapillary.oauth.MapillaryLoginListener;
import org.openstreetmap.josm.plugins.mapillary.oauth.MapillaryUser;
import org.openstreetmap.josm.plugins.mapillary.oauth.OAuthPortListener;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryColorScheme;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;
import org.openstreetmap.josm.tools.GBC;
import org.openstreetmap.josm.tools.I18n;
import org.openstreetmap.josm.tools.Logging;

/**
 * Creates the preferences panel for the plugin.
 *
 * @author nokutu
 *
 */
public class MapillaryPreferenceSetting implements SubPreferenceSetting, MapillaryLoginListener {
  private final JComboBox<String> downloadModeComboBox = new JComboBox<>(new String[]{
      DOWNLOAD_MODE.VISIBLE_AREA.getLabel(),
      DOWNLOAD_MODE.OSM_AREA.getLabel(),
      DOWNLOAD_MODE.MANUAL_ONLY.getLabel()
  });

  private final JCheckBox displayHour =
    // i18n: Checkbox label in JOSM settings
    new JCheckBox(I18n.tr("Display hour when the picture was taken"), MapillaryProperties.DISPLAY_HOUR.get());
  private final JCheckBox format24 =
    // i18n: Checkbox label in JOSM settings
    new JCheckBox(I18n.tr("Use 24 hour format"), MapillaryProperties.TIME_FORMAT_24.get());
  private final JCheckBox moveTo =
    // i18n: Checkbox label in JOSM settings
    new JCheckBox(I18n.tr("Center view on new image when using the buttons to jump to another image"), MapillaryProperties.MOVE_TO_IMG.get());
  private final JCheckBox hoverEnabled =
    // i18n: Checkbox label in JOSM settings
    new JCheckBox(I18n.tr("Preview images when hovering its icon"), MapillaryProperties.HOVER_ENABLED.get());
  private final JCheckBox darkMode =
    // i18n: Checkbox label in JOSM settings
    new JCheckBox(I18n.tr("Dark mode for image display"), MapillaryProperties.DARK_MODE.get());
  private final JCheckBox cutOffSeq =
    // i18n: Checkbox label in JOSM settings
    new JCheckBox(I18n.tr("Cut off sequences at download bounds"), MapillaryProperties.CUT_OFF_SEQUENCES_AT_BOUNDS.get());
  private final JCheckBox imageLinkToBlurEditor =
    new JCheckBox(
      // i18n: Checkbox label in JOSM settings
      I18n.tr("When opening Mapillary image in web browser, show the blur editor instead of the image viewer"),
      MapillaryProperties.IMAGE_LINK_TO_BLUR_EDITOR.get()
    );
  private final JCheckBox developer =
    // i18n: Checkbox label in JOSM settings
    new JCheckBox(I18n.tr("Enable experimental beta-features (might be unstable)"), MapillaryProperties.DEVELOPER.get());
  private final SpinnerNumberModel preFetchSize = new SpinnerNumberModel(
    MapillaryProperties.PRE_FETCH_IMAGE_COUNT.get().intValue(),
    0,
    Integer.MAX_VALUE,
    1
  );
  private final JButton loginButton = new MapillaryButton(new LoginAction(this));
  private final JButton logoutButton = new MapillaryButton(new LogoutAction());
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
        Logging.warn("Could not load Mapillary brand image!");
      }
    } catch (IOException e) {
      Logging.warn("While reading Mapillary brand image, an IO-exception occured!");
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
    mainPanel.setLayout(new GridBagLayout());
    mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

    downloadModeComboBox.setSelectedItem(DOWNLOAD_MODE.fromPrefId(MapillaryProperties.DOWNLOAD_MODE.get()).getLabel());

    JPanel downloadModePanel = new JPanel();
    downloadModePanel.add(new JLabel(I18n.tr("Download mode")));
    downloadModePanel.add(downloadModeComboBox);
    mainPanel.add(downloadModePanel, GBC.eol());

    mainPanel.add(displayHour, GBC.eol());
    mainPanel.add(format24, GBC.eol());
    mainPanel.add(moveTo, GBC.eol());
    mainPanel.add(hoverEnabled, GBC.eol());
    mainPanel.add(darkMode, GBC.eol());
    mainPanel.add(cutOffSeq, GBC.eol());
    mainPanel.add(imageLinkToBlurEditor, GBC.eol());

    final JPanel preFetchPanel = new JPanel();
    // i18n: Spinner label in JOSM settings
    preFetchPanel.add(new JLabel(I18n.tr("Number of images to be pre-fetched (forwards and backwards)")));
    final JSpinner spinner = new JSpinner(preFetchSize);
    final JSpinner.DefaultEditor editor = new JSpinner.NumberEditor(spinner);
    editor.getTextField().setColumns(3);
    spinner.setEditor(editor);
    preFetchPanel.add(spinner);
    mainPanel.add(preFetchPanel, GBC.eol());

    if (ExpertToggleAction.isExpert() || developer.isSelected()) {
      mainPanel.add(developer, GBC.eol());
    }
    MapillaryColorScheme.styleAsDefaultPanel(
      mainPanel, downloadModePanel, displayHour, format24, moveTo, hoverEnabled, darkMode, cutOffSeq, imageLinkToBlurEditor, developer, preFetchPanel
    );
    mainPanel.add(Box.createVerticalGlue(), GBC.eol().fill(GridBagConstraints.BOTH));

    container.add(mainPanel, BorderLayout.CENTER);

    synchronized (gui.getDisplayPreference().getTabPane()) {
      gui.getDisplayPreference().addSubTab(this, "Mapillary", new JScrollPane(container));
      gui.getDisplayPreference().getTabPane().setIconAt(gui.getDisplayPreference().getTabPane().getTabCount()-1, MapillaryPlugin.LOGO.setSize(12, 12).get());
    }

    new Thread(() -> {
      String username = MapillaryUser.getUsername();
      if (username != null) {
        SwingUtilities.invokeLater(() -> onLogin(MapillaryUser.getUsername()));
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
    MapillaryProperties.DOWNLOAD_MODE.put(DOWNLOAD_MODE.fromLabel(downloadModeComboBox.getSelectedItem().toString()).getPrefId());
    MapillaryProperties.DISPLAY_HOUR.put(displayHour.isSelected());
    MapillaryProperties.TIME_FORMAT_24.put(format24.isSelected());
    MapillaryProperties.MOVE_TO_IMG.put(moveTo.isSelected());
    MapillaryProperties.HOVER_ENABLED.put(hoverEnabled.isSelected());
    MapillaryProperties.DARK_MODE.put(darkMode.isSelected());
    MapillaryProperties.CUT_OFF_SEQUENCES_AT_BOUNDS.put(cutOffSeq.isSelected());
    MapillaryProperties.IMAGE_LINK_TO_BLUR_EDITOR.put(imageLinkToBlurEditor.isSelected());
    MapillaryProperties.DEVELOPER.put(developer.isSelected());
    MapillaryProperties.PRE_FETCH_IMAGE_COUNT.put(preFetchSize.getNumber().intValue());

    //Restart is never required
    return false;
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
  private static class LoginAction extends AbstractAction {
    private static final long serialVersionUID = -3908477563072057344L;
    private final transient MapillaryLoginListener callback;

    LoginAction(MapillaryLoginListener loginCallback) {
      super(I18n.tr("Login"));
      this.callback = loginCallback;
    }

    @Override
    public void actionPerformed(ActionEvent arg0) {
      OAuthPortListener portListener = new OAuthPortListener(callback);
      portListener.start();
      try {
        MapillaryUtils.browse(MapillaryURL.MainWebsite.connect("http://localhost:"+OAuthPortListener.PORT+'/'));
      } catch (IOException e) {
        Logging.error(e);
      }
    }
  }

  /**
   * Logs the user out.
   *
   * @author nokutu
   *
   */
  private final class LogoutAction extends AbstractAction {
    private static final long serialVersionUID = 3434780936404707219L;

    private LogoutAction() {
      super(I18n.tr("Logout"));
    }

    @Override
    public void actionPerformed(ActionEvent arg0) {
      MapillaryUser.reset();
      onLogout();
    }
  }
}
