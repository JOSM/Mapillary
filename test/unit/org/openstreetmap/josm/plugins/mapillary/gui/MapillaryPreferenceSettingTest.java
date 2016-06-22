package org.openstreetmap.josm.plugins.mapillary.gui;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.awt.GraphicsEnvironment;
import java.lang.reflect.Field;
import java.util.HashMap;
import java.util.Map;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;

import org.junit.Test;
import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.gui.preferences.PreferenceTabbedPane;
import org.openstreetmap.josm.plugins.mapillary.AbstractTest;
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

  /**
   * Helper method for obtaining the value of a private field
   * @param object the object of which you want the private field
   * @param name the name of the private field
   * @return the current value that field has
   */
  private static Object getPrivateField(MapillaryPreferenceSetting object, String name)
      throws IllegalArgumentException, IllegalAccessException, NoSuchFieldException, SecurityException {
    Field field = object.getClass().getDeclaredField(name);
    field.setAccessible(true);
    return field.get(object);
  }

  @SuppressWarnings("unchecked")
  @Test
  public void testOk() throws SecurityException, IllegalArgumentException, IllegalAccessException, NoSuchFieldException {
    MapillaryPreferenceSetting settings = new MapillaryPreferenceSetting();

    // Test checkboxes
    settings.ok();
    assertEquals(Main.pref.get("mapillary.display-hour"),
        Boolean.toString(((JCheckBox) getPrivateField(settings, "displayHour")).isSelected()));
    assertEquals(Main.pref.get("mapillary.format-24"),
        Boolean.toString(((JCheckBox) getPrivateField(settings, "format24")).isSelected()));
    assertEquals(Main.pref.get("mapillary.move-to-picture"),
        Boolean.toString(((JCheckBox) getPrivateField(settings, "moveTo")).isSelected()));
    assertEquals(Main.pref.get("mapillary.hover-enabled"),
        Boolean.toString(((JCheckBox) getPrivateField(settings, "hoverEnabled")).isSelected()));

    // Toggle state of the checkboxes
    toggleCheckbox((JCheckBox) getPrivateField(settings, "displayHour"));
    toggleCheckbox((JCheckBox) getPrivateField(settings, "format24"));
    toggleCheckbox((JCheckBox) getPrivateField(settings, "moveTo"));
    toggleCheckbox((JCheckBox) getPrivateField(settings, "hoverEnabled"));

    // Test the second state of the checkboxes
    settings.ok();
    assertEquals(Main.pref.get("mapillary.display-hour"),
        Boolean.toString(((JCheckBox) getPrivateField(settings, "displayHour")).isSelected()));
    assertEquals(Main.pref.get("mapillary.format-24"),
        Boolean.toString(((JCheckBox) getPrivateField(settings, "format24")).isSelected()));
    assertEquals(Main.pref.get("mapillary.move-to-picture"),
        Boolean.toString(((JCheckBox) getPrivateField(settings, "moveTo")).isSelected()));
    assertEquals(Main.pref.get("mapillary.hover-enabled"),
        Boolean.toString(((JCheckBox) getPrivateField(settings, "hoverEnabled")).isSelected()));

    // Test combobox
    for (int i = 0; i < ((JComboBox<String>) getPrivateField(settings, "downloadModeComboBox")).getItemCount(); i++) {
      ((JComboBox<String>) getPrivateField(settings, "downloadModeComboBox")).setSelectedIndex(i);
      settings.ok();
      assertEquals(Main.pref.get("mapillary.download-mode"),
          ((JComboBox<String>) getPrivateField(settings, "downloadModeComboBox")).getSelectedItem());
    }
  }

  private static void toggleCheckbox(JCheckBox jcb) {
    jcb.setSelected(!jcb.isSelected());
  }

}
