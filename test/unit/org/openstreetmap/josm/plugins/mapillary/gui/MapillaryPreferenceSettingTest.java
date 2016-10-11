// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.openstreetmap.josm.plugins.mapillary.utils.TestUtil.getPrivateField;

import java.awt.GraphicsEnvironment;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;

import org.junit.Test;
import org.openstreetmap.josm.data.preferences.BooleanProperty;
import org.openstreetmap.josm.data.preferences.StringProperty;
import org.openstreetmap.josm.gui.preferences.PreferenceTabbedPane;
import org.openstreetmap.josm.plugins.mapillary.AbstractTest;
import org.openstreetmap.josm.plugins.mapillary.io.download.MapillaryDownloader.DOWNLOAD_MODE;
import org.openstreetmap.josm.tools.I18n;

public class MapillaryPreferenceSettingTest extends AbstractTest {

  @Test
  public void testAddGui() {
    if (GraphicsEnvironment.isHeadless()) {
      return;
    }
    PreferenceTabbedPane tabs = new PreferenceTabbedPane();
    tabs.buildGui();
    int displayTabs = tabs.getDisplayPreference().getTabPane().getTabCount();
    MapillaryPreferenceSetting setting = new MapillaryPreferenceSetting();
    setting.addGui(tabs);
    assertEquals(displayTabs + 1, tabs.getDisplayPreference().getTabPane().getTabCount());
    assertEquals(tabs.getDisplayPreference(), setting.getTabPreferenceSetting(tabs));
  }

  @Test
  public void testIsExpert() {
    assertFalse(new MapillaryPreferenceSetting().isExpert());
  }

  @Test
  public void testLoginLogout() throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
    if (GraphicsEnvironment.isHeadless()) {
      return;
    }
    PreferenceTabbedPane tabs = new PreferenceTabbedPane();
    tabs.buildGui();
    MapillaryPreferenceSetting setting = new MapillaryPreferenceSetting();
    setting.addGui(tabs);
    setting.onLogout();

    assertEquals(I18n.tr("Login"), ((JButton) getPrivateField(setting, "loginButton")).getText());
    assertEquals(I18n.tr("You are currently not logged in."), ((JLabel) getPrivateField(setting, "loginLabel")).getText());
    assertFalse(((JPanel) getPrivateField(setting, "loginPanel")).isAncestorOf(((JButton) getPrivateField(setting, "logoutButton"))));
    assertTrue(((JPanel) getPrivateField(setting, "loginPanel")).isAncestorOf(((JButton) getPrivateField(setting, "loginButton"))));

    String username = "TheMapillaryUsername";
    setting.onLogin(username);

    assertEquals(I18n.tr("Login"), ((JButton) getPrivateField(setting, "loginButton")).getText());
    assertEquals(I18n.tr("You are logged in as ''{0}''.", username), ((JLabel) getPrivateField(setting, "loginLabel")).getText());
    assertTrue(((JPanel) getPrivateField(setting, "loginPanel")).isAncestorOf(((JButton) getPrivateField(setting, "logoutButton"))));
    assertFalse(((JPanel) getPrivateField(setting, "loginPanel")).isAncestorOf(((JButton) getPrivateField(setting, "loginButton"))));
  }

  @SuppressWarnings("unchecked")
  @Test
  public void testOk() throws SecurityException, IllegalArgumentException, IllegalAccessException, NoSuchFieldException {
    MapillaryPreferenceSetting settings = new MapillaryPreferenceSetting();

    // Initialize the properties with some arbitrary value to make sure they are not unset
    new StringProperty("mapillary.display-hour", "default").put("arbitrary");
    new StringProperty("mapillary.format-24", "default").put("arbitrary");
    new StringProperty("mapillary.move-to-picture", "default").put("arbitrary");
    new StringProperty("mapillary.hover-enabled", "default").put("arbitrary");
    new StringProperty("mapillary.download-mode", "default").put("arbitrary");

    // Test checkboxes
    settings.ok();
    assertPropertyMatchesCheckboxSelection((JCheckBox) getPrivateField(settings, "displayHour"), "mapillary.display-hour");
    assertPropertyMatchesCheckboxSelection((JCheckBox) getPrivateField(settings, "format24"), "mapillary.format-24");
    assertPropertyMatchesCheckboxSelection((JCheckBox) getPrivateField(settings, "moveTo"), "mapillary.move-to-picture");
    assertPropertyMatchesCheckboxSelection((JCheckBox) getPrivateField(settings, "hoverEnabled"), "mapillary.hover-enabled");

    // Toggle state of the checkboxes
    toggleCheckbox((JCheckBox) getPrivateField(settings, "displayHour"));
    toggleCheckbox((JCheckBox) getPrivateField(settings, "format24"));
    toggleCheckbox((JCheckBox) getPrivateField(settings, "moveTo"));
    toggleCheckbox((JCheckBox) getPrivateField(settings, "hoverEnabled"));

    // Test the second state of the checkboxes
    settings.ok();
    assertPropertyMatchesCheckboxSelection((JCheckBox) getPrivateField(settings, "displayHour"), "mapillary.display-hour");
    assertPropertyMatchesCheckboxSelection((JCheckBox) getPrivateField(settings, "format24"), "mapillary.format-24");
    assertPropertyMatchesCheckboxSelection((JCheckBox) getPrivateField(settings, "moveTo"), "mapillary.move-to-picture");
    assertPropertyMatchesCheckboxSelection((JCheckBox) getPrivateField(settings, "hoverEnabled"), "mapillary.hover-enabled");

    // Test combobox
    for (int i = 0; i < ((JComboBox<String>) getPrivateField(settings, "downloadModeComboBox")).getItemCount(); i++) {
      ((JComboBox<String>) getPrivateField(settings, "downloadModeComboBox")).setSelectedIndex(i);
      settings.ok();
      assertEquals(
        new StringProperty("mapillary.download-mode", "default").get(),
        DOWNLOAD_MODE.fromLabel(
          ((JComboBox<String>) getPrivateField(settings, "downloadModeComboBox")).getSelectedItem().toString()
        ).getPrefId()
      );
    }
  }

  /**
   * Checks, if a certain {@link BooleanProperty} (identified by the {@code propName} attribute) matches the selected-state of the given {@link JCheckBox}
   * @param cb the {@link JCheckBox}, which should be checked against the {@link BooleanProperty}
   * @param propName the name of the property against which the selected-state of the given {@link JCheckBox} should be checked
   */
  private static void assertPropertyMatchesCheckboxSelection(JCheckBox cb, String propName) {
    assertEquals(cb.isSelected(), new BooleanProperty(propName, !cb.isSelected()).get());
  }

  private static void toggleCheckbox(JCheckBox jcb) {
    jcb.setSelected(!jcb.isSelected());
  }

}
