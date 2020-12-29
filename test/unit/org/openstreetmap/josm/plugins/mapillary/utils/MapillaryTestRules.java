// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.awt.GraphicsEnvironment;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.logging.Level;

import org.junit.jupiter.api.extension.ExtensionContext;

import org.openstreetmap.josm.data.preferences.AbstractProperty;
import org.openstreetmap.josm.spi.preferences.Config;
import org.openstreetmap.josm.testutils.JOSMTestRules;
import org.openstreetmap.josm.tools.Logging;
import org.openstreetmap.josm.tools.Utils;

/**
 * An extension of JOSMTestRules
 */
public class MapillaryTestRules extends JOSMTestRules {
  private boolean usePreferences;

  @Override
  public void beforeAll(ExtensionContext context) throws Exception {
    Logging.getLogger().setFilter(record -> record.getLevel().intValue() >= Level.WARNING.intValue()
      || record.getSourceClassName().startsWith("org.openstreetmap.josm.plugins.mapillary"));
    Utils.updateSystemProperty("java.util.logging.SimpleFormatter.format", "%1$tF %1$tT.%1$tL %2$s %4$s: %5$s%6$s%n");
    final String isHeadless = Boolean.toString(GraphicsEnvironment.isHeadless());
    super.beforeAll(context);
    System.setProperty("java.awt.headless", isHeadless);

    if (usePreferences) {
      assertNotNull(Config.getPref());
      for (Field field : MapillaryProperties.class.getFields()) {
        Object obj = field.get(MapillaryProperties.class);
        if (obj instanceof AbstractProperty) {
          doSetConfig((AbstractProperty<?>) obj);
        }
      }
    }
  }

  /**
   * Used since CLI tests are failing due to an NPE in a property
   *
   * @param property The property to fix (Config.getPref() is what the preference class is set to)
   */
  private static void doSetConfig(AbstractProperty<?> property) {
    try {
      Method getPreferences = AbstractProperty.class.getDeclaredMethod("getPreferences");
      getPreferences.setAccessible(true);
      if (!Config.getPref().equals(getPreferences.invoke(property))) {
        Field configuration = AbstractProperty.class.getDeclaredField("preferences");
        configuration.setAccessible(true);
        configuration.set(property, Config.getPref());
      }
      assertNotNull(getPreferences.invoke(property));
    } catch (ReflectiveOperationException e) {
      Logging.error(e);
      throw new AssertionError(e);
    }
  }

  @Override
  public MapillaryTestRules preferences() {
    super.preferences();
    usePreferences = true;
    return this;
  }
}
