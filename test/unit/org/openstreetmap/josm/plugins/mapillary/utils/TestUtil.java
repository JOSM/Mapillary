// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.awt.GraphicsEnvironment;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.Objects;
import java.util.logging.Level;

import org.junit.runners.model.InitializationError;

import org.openstreetmap.josm.testutils.JOSMTestRules;
import org.openstreetmap.josm.tools.Logging;
import org.openstreetmap.josm.tools.Utils;

/**
 * Utilities for tests.
 */
public final class TestUtil {

  private TestUtil() {
    // Prevent instantiation
  }

  /**
   * @param object the object for which you want the private field, not null
   * @param name the name of the private field
   * @param <T> the type of the object
   * @return the value of the private field
   */
  @SuppressWarnings("unchecked")
  public static <T> Object getPrivateFieldValue(T object, String name) {
    return getPrivateFieldValue((Class<T>) Objects.requireNonNull(object).getClass(), object, name);
  }

  /**
   * Helper method for obtaining the value of a private field
   * @param clazz the class of the object
   * @param object the object of which you want the private field
   * @param name the name of the private field
   * @return the current value that field has
   */
  public static <T> Object getPrivateFieldValue(Class<T> clazz, T object, String name) {
    try {
      final Field field = clazz.getDeclaredField(name);
      field.setAccessible(true);
      return field.get(object);
    } catch (IllegalAccessException | SecurityException | NoSuchFieldException e) {
      Logging.error(e);
      fail(e.getLocalizedMessage());
    }
    return null;
  }

  /**
   * This method tests utility classes for common coding standards (exactly one constructor that's private,
   * only static methods, …) and fails the current test if one of those standards is not met.
   * This is inspired by <a href="https://stackoverflow.com/a/10872497">an answer on StackOverflow.com</a> .
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
      assertTrue(Modifier.isPrivate(constructor.getModifiers()));

      // Call private constructor for code coverage
      constructor.setAccessible(true);
      constructor.newInstance();
      constructor.setAccessible(false);

      for (Method m : c.getMethods()) {
        // Check if all methods are static
        assertTrue(m.getDeclaringClass() != c || Modifier.isStatic(m.getModifiers()));
      }
    } catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
      fail(e.getLocalizedMessage());
    }
  }

  public static class MapillaryTestRules extends JOSMTestRules {
    @Override
    protected void before() throws InitializationError, ReflectiveOperationException {
      Logging.getLogger().setFilter(record -> record.getLevel().intValue() >= Level.WARNING.intValue() || record.getSourceClassName().startsWith("org.openstreetmap.josm.plugins.mapillary"));
      Utils.updateSystemProperty("java.util.logging.SimpleFormatter.format", "%1$tF %1$tT.%1$tL %2$s %4$s: %5$s%6$s%n");
      final String isHeadless = Boolean.toString(GraphicsEnvironment.isHeadless());
      super.before();
      System.setProperty("java.awt.headless", isHeadless);
    }
  }

  public static String getApiV3BaseUrl() {
    return MapillaryURL.APIv3.baseUrl;
  }

  public static void setAPIv3BaseUrl(final String baseUrl) {
    MapillaryURL.APIv3.baseUrl = baseUrl;
  }


  public static String getMainWebsiteBaseUrl() {
    return MapillaryURL.MainWebsite.baseUrl;
  }

  public static void setMainWebsiteBaseUrl(final String baseUrl) {
    MapillaryURL.MainWebsite.baseUrl = baseUrl;
  }
}
