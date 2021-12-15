// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.junit.jupiter.api.Assumptions.assumeTrue;
import static org.openstreetmap.josm.plugins.mapillary.utils.TestUtil.getPrivateFieldValue;

import java.awt.GraphicsEnvironment;
import java.time.Duration;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SpinnerNumberModel;

import org.awaitility.Awaitility;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.RegisterExtension;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.openstreetmap.josm.data.preferences.AbstractToStringProperty;
import org.openstreetmap.josm.data.preferences.BooleanProperty;
import org.openstreetmap.josm.data.preferences.StringProperty;
import org.openstreetmap.josm.gui.preferences.PreferenceTabbedPane;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.testutils.JOSMTestRules;
import org.openstreetmap.josm.testutils.annotations.BasicPreferences;
import org.openstreetmap.josm.tools.I18n;

@BasicPreferences
class MapillaryPreferenceSettingTest {

    @RegisterExtension
    static JOSMTestRules rules = new JOSMTestRules().main();

    @Test
    void testAddGui() {
        PreferenceTabbedPane tabs = new PreferenceTabbedPane();
        tabs.buildGui();
        int displayTabs = tabs.getPluginPreference().getTabPane().getTabCount();
        MapillaryPreferenceSetting setting = new MapillaryPreferenceSetting();
        setting.addGui(tabs);
        assertEquals(displayTabs + 1, tabs.getPluginPreference().getTabPane().getTabCount());
        assertEquals(tabs.getPluginPreference(), setting.getTabPreferenceSetting(tabs));
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

    /**
     * Get a stream of arguments
     *
     * @return An ordered stream of boolean properties visible in preferences. Second argument is field name.
     */
    static Stream<Arguments> testOkCheckbox() {
        return Stream.of(Arguments.of(MapillaryProperties.DISPLAY_HOUR, "displayHour"),
            Arguments.of(MapillaryProperties.MOVE_TO_IMG, "moveTo"),
            Arguments.of(MapillaryProperties.IMAGE_LINK_TO_BLUR_EDITOR, "imageLinkToBlurEditor"),
            Arguments.of(MapillaryProperties.COLOR_BY_CAPTURE_DATE, "colorImagesByCaptureDate"),
            Arguments.of(MapillaryProperties.USE_COMPUTED_LOCATIONS, "useComputedLocations"),
            Arguments.of(MapillaryProperties.DEVELOPER, "developer"));
    }

    @ParameterizedTest
    @MethodSource
    void testOkCheckbox(final BooleanProperty property, final String fieldName) {
        // Set the pref poorly
        new StringProperty(property.getKey(), "default").put("arbitrary");

        final MapillaryPreferenceSetting settings = new MapillaryPreferenceSetting();

        // Test checkboxes
        settings.ok();
        assertPropertyMatchesCheckboxSelection((JCheckBox) getPrivateFieldValue(settings, fieldName),
            property.getKey());

        // Toggle state
        toggleCheckbox((JCheckBox) getPrivateFieldValue(settings, fieldName));

        // Test the second state of checkboxes
        settings.ok();
        assertPropertyMatchesCheckboxSelection((JCheckBox) getPrivateFieldValue(settings, fieldName),
            property.getKey());
    }

    /**
     * Get a stream of arguments
     *
     * @return An ordered stream of number properties visible in preferences. Second argument is field name.
     */
    static Stream<Arguments> testOkSpinner() {
        return Stream.of(Arguments.of(MapillaryProperties.PRE_FETCH_IMAGE_COUNT, "preFetchSize"),
            Arguments.of(MapillaryProperties.MAXIMUM_DISTANCE_FOR_CHANGESET_SOURCE, "maxDistanceForChangesetSource"),
            Arguments.of(MapillaryProperties.MAXIMUM_DRAW_IMAGES, "maxForImagesToDraw"));
    }

    @ParameterizedTest
    @MethodSource
    void testOkSpinner(final AbstractToStringProperty<? extends Number> property, final String fieldName) {
        MapillaryPreferenceSetting settings = new MapillaryPreferenceSetting();
        // Test defaults
        settings.ok();
        assertFalse(property.isSet());
        assertEquals(((SpinnerNumberModel) getPrivateFieldValue(settings, fieldName)).getNumber().toString(),
            property.getDefaultValue().toString());

        // Change the number
        final Number toSet;
        if (property.getDefaultValue() instanceof Integer) {
            toSet = property.getDefaultValue().intValue() + 1;
        } else if (property.getDefaultValue() instanceof Double) {
            toSet = property.getDefaultValue().doubleValue() + 1;
        } else {
            fail("The number type " + property.get().getClass().getSimpleName() + " is unknown");
            throw new IllegalArgumentException("This shouldn't be hit. Used to avoid compiler warning.");
        }
        ((SpinnerNumberModel) getPrivateFieldValue(settings, fieldName)).setValue(toSet);

        // Test the changed value
        settings.ok();
        assertEquals(((SpinnerNumberModel) getPrivateFieldValue(settings, fieldName)).getNumber().toString(),
            property.get().toString());
    }

    /**
     * This is used to check that all fields are "saved" and tested
     *
     * @return A argument list with [fieldNames from class, fieldName to test]
     */
    static Stream<Arguments> testAllSettingsAreChecked() {
        Collection<String> fieldNames = Stream.concat(testOkCheckbox(), testOkSpinner())
            .map(arguments -> (String) arguments.get()[1]).collect(Collectors.toCollection(HashSet::new));
        // Jacoco can add a field for coverage information
        fieldNames.add("$jacocoData");
        // These should be tested in the testLoginLogout class (non-parameterized)
        fieldNames.addAll(Arrays.asList("loginLabel", "requiresLogin", "loginPanel", "loginButton", "logoutButton"));
        return Stream.of(MapillaryPreferenceSetting.class.getDeclaredFields())
            .map(field -> Arguments.of(fieldNames, field.getName()));
    }

    @ParameterizedTest(name = "[{index}] {1}")
    @MethodSource
    void testAllSettingsAreChecked(final Collection<String> checkedFieldNames, final String fieldName) {
        if (!checkedFieldNames.contains(fieldName) && !fieldName.startsWith("__$") /* __$ vars used for coverage */) {
            fail(fieldName + " is not tested");
        }
    }

    /**
     * Checks, if a certain {@link BooleanProperty} (identified by the {@code propName} attribute) matches the
     * selected-state of the given {@link JCheckBox}
     *
     * @param cb the {@link JCheckBox}, which should be checked against the {@link BooleanProperty}
     * @param propName the name of the property against which the selected-state of the given {@link JCheckBox} should
     *        be
     *        checked
     */
    private static void assertPropertyMatchesCheckboxSelection(JCheckBox cb, String propName) {
        assertEquals(cb.isSelected(), new BooleanProperty(propName, !cb.isSelected()).get());
    }

    private static void toggleCheckbox(JCheckBox jcb) {
        jcb.setSelected(!jcb.isSelected());
    }

}
