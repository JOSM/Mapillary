// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui;

import org.awaitility.Awaitility;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.RegisterExtension;
import org.openstreetmap.josm.data.preferences.BooleanProperty;
import org.openstreetmap.josm.data.preferences.StringProperty;
import org.openstreetmap.josm.gui.preferences.PreferenceTabbedPane;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryTestRules;
import org.openstreetmap.josm.testutils.JOSMTestRules;
import org.openstreetmap.josm.tools.I18n;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SpinnerNumberModel;
import java.awt.GraphicsEnvironment;
import java.time.Duration;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeTrue;
import static org.openstreetmap.josm.plugins.mapillary.utils.TestUtil.getPrivateFieldValue;

class MapillaryPreferenceSettingTest {

  @RegisterExtension
  static JOSMTestRules rules = new MapillaryTestRules().main();

  @Test
  void testAddGui() {
    assumeTrue(!GraphicsEnvironment.isHeadless());
    PreferenceTabbedPane tabs = new PreferenceTabbedPane();
    tabs.buildGui();
    int displayTabs = tabs.getDisplayPreference().getTabPane().getTabCount();
    MapillaryPreferenceSetting setting = new MapillaryPreferenceSetting();
    setting.addGui(tabs);
    assertEquals(displayTabs + 1, tabs.getDisplayPreference().getTabPane().getTabCount());
    assertEquals(tabs.getDisplayPreference(), setting.getTabPreferenceSetting(tabs));
  }

  @Test
  void testIsExpert() {
    assertFalse(new MapillaryPreferenceSetting().isExpert());
  }

  @Test
  void testLoginLogout() {
    assumeTrue(!GraphicsEnvironment.isHeadless());
    PreferenceTabbedPane tabs = new PreferenceTabbedPane();
    tabs.buildGui();
    MapillaryPreferenceSetting setting = new MapillaryPreferenceSetting();
    setting.addGui(tabs);
    setting.onLogout();

    final String loginPanel = "loginPanel";
    final String loginButton = "loginButton";

    assertEquals(I18n.tr("Login"), ((JButton) getPrivateFieldValue(setting, loginButton)).getText());
    assertEquals(I18n.tr("You are currently not logged in."),
      ((JLabel) getPrivateFieldValue(setting, "loginLabel")).getText());
    assertFalse(((JPanel) getPrivateFieldValue(setting, loginPanel))
      .isAncestorOf((JButton) getPrivateFieldValue(setting, "logoutButton")));
    assertTrue(((JPanel) getPrivateFieldValue(setting, loginPanel))
      .isAncestorOf((JButton) getPrivateFieldValue(setting, loginButton)));

    String username = "TheMapillaryUsername";
    setting.onLogin(username);

    assertEquals(I18n.tr("Login"), ((JButton) getPrivateFieldValue(setting, loginButton)).getText());
    Awaitility.await().atMost(Duration.ofSeconds(5)).until(() -> I18n.tr("You are logged in as ''{0}''.", username)
      .equals(((JLabel) getPrivateFieldValue(setting, "loginLabel")).getText()));
    assertEquals(I18n.tr("You are logged in as ''{0}''.", username),
      ((JLabel) getPrivateFieldValue(setting, "loginLabel")).getText());
    assertTrue(((JPanel) getPrivateFieldValue(setting, loginPanel))
      .isAncestorOf((JButton) getPrivateFieldValue(setting, "logoutButton")));
    assertFalse(((JPanel) getPrivateFieldValue(setting, loginPanel))
      .isAncestorOf((JButton) getPrivateFieldValue(setting, loginButton)));
  }

  @SuppressWarnings("unchecked")
  @Test
  void testOk() {
    MapillaryPreferenceSetting settings = new MapillaryPreferenceSetting();
    String arbitrary = "arbitrary";

    // Initialize the properties with some arbitrary value to make sure they are not unset
    final String defaultValue = "default";
    new StringProperty("mapillary.display-hour", defaultValue).put(arbitrary);
    new StringProperty("mapillary.format-24", defaultValue).put(arbitrary);
    new StringProperty("mapillary.move-to-picture", defaultValue).put(arbitrary);
    new StringProperty("mapillary.hover-enabled", defaultValue).put(arbitrary);
    new StringProperty("mapillary.dark-mode", defaultValue).put(arbitrary);
    new StringProperty("mapillary.download-mode", defaultValue).put(arbitrary);
    new StringProperty("mapillary.prefetch-image-count", defaultValue).put(arbitrary);
    new StringProperty("mapillary.imageMode", defaultValue).put(arbitrary);

    // Test checkboxes
    settings.ok();
    assertPropertyMatchesCheckboxSelection((JCheckBox) getPrivateFieldValue(settings, "displayHour"),
      "mapillary.display-hour");
    assertPropertyMatchesCheckboxSelection((JCheckBox) getPrivateFieldValue(settings, "moveTo"),
      "mapillary.move-to-picture");
    assertPropertyMatchesCheckboxSelection((JCheckBox) getPrivateFieldValue(settings, "hoverEnabled"),
      "mapillary.hover-enabled");
    assertPropertyMatchesCheckboxSelection((JCheckBox) getPrivateFieldValue(settings, "darkMode"),
      "mapillary.dark-mode");
    assertEquals(
      String.valueOf(((SpinnerNumberModel) getPrivateFieldValue(settings, "preFetchSize")).getNumber().intValue()),
      new StringProperty("mapillary.prefetch-image-count", "default").get());

    // Toggle state of the checkboxes
    toggleCheckbox((JCheckBox) getPrivateFieldValue(settings, "displayHour"));
    toggleCheckbox((JCheckBox) getPrivateFieldValue(settings, "moveTo"));
    toggleCheckbox((JCheckBox) getPrivateFieldValue(settings, "hoverEnabled"));
    toggleCheckbox((JCheckBox) getPrivateFieldValue(settings, "darkMode"));
    ((SpinnerNumberModel) getPrivateFieldValue(settings, "preFetchSize")).setValue(73);

    // Test the second state of the checkboxes
    settings.ok();
    assertPropertyMatchesCheckboxSelection((JCheckBox) getPrivateFieldValue(settings, "displayHour"),
      "mapillary.display-hour");
    assertPropertyMatchesCheckboxSelection((JCheckBox) getPrivateFieldValue(settings, "format24"),
      "mapillary.format-24");
    assertPropertyMatchesCheckboxSelection((JCheckBox) getPrivateFieldValue(settings, "moveTo"),
      "mapillary.move-to-picture");
    assertPropertyMatchesCheckboxSelection((JCheckBox) getPrivateFieldValue(settings, "hoverEnabled"),
      "mapillary.hover-enabled");
    assertPropertyMatchesCheckboxSelection((JCheckBox) getPrivateFieldValue(settings, "darkMode"),
      "mapillary.dark-mode");
    assertEquals(
      String.valueOf(((SpinnerNumberModel) getPrivateFieldValue(settings, "preFetchSize")).getNumber().intValue()),
      new StringProperty("mapillary.prefetch-image-count", defaultValue).get());
  }

  /**
   * Checks, if a certain {@link BooleanProperty} (identified by the {@code propName} attribute) matches the
   * selected-state of the given {@link JCheckBox}
   *
   * @param cb the {@link JCheckBox}, which should be checked against the {@link BooleanProperty}
   * @param propName the name of the property against which the selected-state of the given {@link JCheckBox} should be
   *        checked
   */
  private static void assertPropertyMatchesCheckboxSelection(JCheckBox cb, String propName) {
    assertEquals(cb.isSelected(), new BooleanProperty(propName, !cb.isSelected()).get());
  }

  private static void toggleCheckbox(JCheckBox jcb) {
    jcb.setSelected(!jcb.isSelected());
  }

}
