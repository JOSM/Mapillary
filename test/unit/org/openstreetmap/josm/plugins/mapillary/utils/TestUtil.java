package org.openstreetmap.josm.plugins.mapillary.utils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.data.projection.Projections;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.tools.I18n;

/**
 *
 * Utilities for tests.
 *
 * @author floscher
 */
public class TestUtil {
  private static boolean isInitialized;

  private TestUtil() {
    // Prevent instantiation
  }

  /**
   * Initializes the {@link Main} class of JOSM and the mapillary plugin with
   * the preferences from test/data/preferences.
   *
   * That is needed e.g. to use {@link MapillaryLayer#getInstance()}
   */
  public static final void initPlugin() {
    if (!isInitialized) {
      System.setProperty("josm.home", "test/data/preferences");
      Main.initApplicationPreferences();
      Main.pref.enableSaveOnPut(false);
      I18n.init();
      Main.determinePlatformHook();
      Main.platform.preStartupHook();
      Main.pref.init(false);
      I18n.set(Main.pref.get("language", "en"));
      Main.setProjection(Projections.getProjectionByCode("EPSG:3857")); // Mercator
      isInitialized = true;
    }
  }

  /**
   * This method tests utility classes for common coding standards (exactly one constructor that's private,
   * only static methods, …) and fails the current test if one of those standards is not met.
   * This is inspired by http://stackoverflow.com/questions/4520216 .
   * @param c the class under test
   */
  public static void testUtilityClass(final Class<?> c) {
    try {
      // class must be final
      assertTrue(Modifier.isFinal(c.getModifiers()));
      // with exactly one constructor
      assertEquals(1, c.getDeclaredConstructors().length);
      final Constructor<?> constructor = c.getDeclaredConstructors()[0];
      // constructor has to be private
      assertTrue(!constructor.isAccessible() && Modifier.isPrivate(constructor.getModifiers()));
      constructor.setAccessible(true);
      // Call private constructor for code coverage
      constructor.newInstance();
      for (Method m : c.getMethods()) {
        // Check if all methods are static
        assertTrue(m.getDeclaringClass() != c || Modifier.isStatic(m.getModifiers()));
      }
    } catch (Exception e) {
      assertTrue(e.getMessage(), false);
    }
  }
}
