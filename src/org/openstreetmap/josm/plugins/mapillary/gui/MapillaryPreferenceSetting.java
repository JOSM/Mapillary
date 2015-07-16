package org.openstreetmap.josm.plugins.mapillary.gui;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.FlowLayout;
import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JOptionPane;
import javax.swing.JPanel;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.gui.preferences.PreferenceTabbedPane;
import org.openstreetmap.josm.gui.preferences.SubPreferenceSetting;
import org.openstreetmap.josm.gui.preferences.TabPreferenceSetting;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;

/**
 * Creates the preferences panel for the plugin.
 *
 * @author nokutu
 *
 */
public class MapillaryPreferenceSetting implements SubPreferenceSetting {

  private JCheckBox reverseButtons = new JCheckBox(
      tr("Reverse buttons position when displaying images."));
  private JCheckBox downloadMode = new JCheckBox(tr("Download images manually"));
  private JCheckBox displayHour = new JCheckBox(
      tr("Display hour when the picture was taken"));
  private JCheckBox format24 = new JCheckBox(tr("Use 24 hour format"));
  private JCheckBox moveTo = new JCheckBox(
      tr("Move to picture's location with next/previous buttons"));

  @Override
  public TabPreferenceSetting getTabPreferenceSetting(PreferenceTabbedPane gui) {
    return gui.getDisplayPreference();
  }

  @Override
  public void addGui(PreferenceTabbedPane gui) {
    JPanel panel = new JPanel();

    reverseButtons.setSelected(Main.pref
        .getBoolean("mapillary.reverse-buttons"));
    downloadMode.setSelected(Main.pref
        .getBoolean("mapillary.download-manually"));
    displayHour.setSelected(Main.pref
        .getBoolean("mapillary.display-hour", true));
    format24.setSelected(Main.pref.getBoolean("mapillary.format-24"));
    moveTo.setSelected(Main.pref.getBoolean("mapillary.move-to-picture", true));

    panel.setLayout(new FlowLayout(FlowLayout.LEFT));
    panel.add(reverseButtons);
    panel.add(downloadMode);
    panel.add(displayHour);
    panel.add(format24);
    panel.add(moveTo);
    JButton oauth = new JButton(new OAuthAction());
    oauth.setText("Login");
    panel.add(oauth);
    gui.getDisplayPreference().addSubTab(this, "Mapillary", panel);
  }

  @Override
  public boolean ok() {
    boolean mod = false;
    Main.pref.put("mapillary.reverse-buttons", reverseButtons.isSelected());
    Main.pref.put("mapillary.download-manually", downloadMode.isSelected());
    MapillaryPlugin.setMenuEnabled(MapillaryPlugin.DOWNLOAD_VIEW_MENU,
        downloadMode.isSelected());

    Main.pref.put("mapillary.display-hour", displayHour.isSelected());
    Main.pref.put("mapillary.format-24", format24.isSelected());
    Main.pref.put("mapillary.move-to-picture", moveTo.isSelected());
    return mod;
  }

  @Override
  public boolean isExpert() {
    return false;
  }

  /**
   * Opens the {@link MapillaryoAuthUI} window and lets the user log in.
   *
   * @author nokutu
   *
   */
  public class OAuthAction extends AbstractAction {

    private static final long serialVersionUID = -3908477563072057344L;

    @Override
    public void actionPerformed(ActionEvent arg0) {
      JButton login = new JButton();
      JButton cancel = new JButton();
      JOptionPane pane = new JOptionPane(new MapillaryOAuthUI(),
          JOptionPane.PLAIN_MESSAGE, JOptionPane.DEFAULT_OPTION, null,
          new JButton[] { login, cancel });
      login.setAction(new LoginAction(pane));
      cancel.setAction(new CancelAction(pane));
      JDialog dlg = pane.createDialog(Main.parent, tr("Login"));
      dlg.setVisible(true);
      dlg.dispose();
    }
  }

  private class LoginAction extends AbstractAction {

    private static final long serialVersionUID = -7157028112711343289L;

    private JOptionPane pane;

    public LoginAction(JOptionPane pane) {
      putValue(NAME, tr("Login"));
      this.pane = pane;
    }

    @Override
    public void actionPerformed(ActionEvent e) {
      pane.setValue(JOptionPane.OK_OPTION);
    }
  }

  private class CancelAction extends AbstractAction {

    private static final long serialVersionUID = 2329066472975953270L;

    private JOptionPane pane;

    public CancelAction(JOptionPane pane) {
      putValue(NAME, tr("Cancel"));
      this.pane = pane;
    }

    @Override
    public void actionPerformed(ActionEvent e) {
      pane.setValue(JOptionPane.CANCEL_OPTION);
    }
  }
}
