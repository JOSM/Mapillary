package org.openstreetmap.josm.plugins.mapillary.gui;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.awt.GraphicsEnvironment;
import java.lang.reflect.Field;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;

import org.junit.Test;
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

    String username = "TheMapillaryUsername";
    setting.onLogin(username);

    assertEquals(I18n.tr("Re-Login"), ((JButton) getPrivateField(setting, "loginButton")).getText());
    assertEquals(I18n.tr("You are logged in as ''{0}''.", username), ((JLabel) getPrivateField(setting, "loginLabel")).getText());
    assertTrue(((JPanel) getPrivateField(setting, "loginPanel")).isAncestorOf(((JButton) getPrivateField(setting, "logoutButton"))));
  }

  private Object getPrivateField(MapillaryPreferenceSetting object, String name) throws IllegalArgumentException, IllegalAccessException, NoSuchFieldException, SecurityException {
    Field field = object.getClass().getDeclaredField(name);
    field.setAccessible(true);
    return field.get(object);
  }

}
