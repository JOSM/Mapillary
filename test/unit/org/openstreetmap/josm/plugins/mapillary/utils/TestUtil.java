// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.data.projection.Projections;
import org.openstreetmap.josm.gui.preferences.ToolbarPreferences;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.tools.I18n;

/**
 * Utilities for tests.
 */
public final class TestUtil {
  private static boolean isInitialized;

  private TestUtil() {
    // Prevent instantiation
  }

  /**
   * Helper method for obtaining the value of a private field
   * @param object the object of which you want the private field
   * @param name the name of the private field
   * @return the current value that field has
   */
  public static Object getPrivateField(Object object, String name)
      throws IllegalArgumentException, IllegalAccessException, NoSuchFieldException, SecurityException {
    Field field = object.getClass().getDeclaredField(name);
    field.setAccessible(true);
    return field.get(object);
  }

  /**
   * Initializes the {@link Main} class of JOSM and the mapillary plugin with
   * the preferences from test/data/preferences.
   *
   * That is needed e.g. to use {@link MapillaryLayer#getInstance()}
   * @throws IOException
   */
  public static synchronized void initPlugin() throws IOException {
    final String josmHomeForUnitTests = "build/.josm_test";
    if (new File(josmHomeForUnitTests).exists()) {
      Files.walkFileTree(Paths.get(josmHomeForUnitTests), new SimpleFileVisitor<Path>() {
        @Override
        public FileVisitResult postVisitDirectory(Path dir, IOException exc) throws IOException {
          Files.delete(dir);
          return super.postVisitDirectory(dir, exc);
        }
        @Override
        public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
          Files.delete(file);
          return super.visitFile(file, attrs);
        }
      });
    }
    if (!isInitialized) {
      System.setProperty("josm.home", josmHomeForUnitTests);
      Main.pref.enableSaveOnPut(false);
      I18n.init();
      Main.determinePlatformHook();
      Main.platform.preStartupHook();
      Main.pref.init(false);
      I18n.set(Main.pref.get("language", "en"));
      Main.setProjection(Projections.getProjectionByCode("EPSG:3857")); // Mercator
      isInitialized = true;
      if (Main.toolbar == null) {
        Main.toolbar = new ToolbarPreferences();
      }
    }
  }

  /**
   * This method tests utility classes for common coding standards (exactly one constructor that's private,
   * only static methods, …) and fails the current test if one of those standards is not met.
   * This is inspired by https://stackoverflow.com/a/10872497 .
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
    } catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
      fail(e.getLocalizedMessage());
    }
  }
}
