// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.objects;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.function.Function;

import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL.MainWebsite;

public class MapObjectTest {

  private static final MapObject MO_1 = new MapObject(new LatLon(0, 0), "key1", "", "", 0, 0, 0);
  private static final MapObject MO_2 = new MapObject(new LatLon(0, 0), "key2", "", "", 0, 0, 0);
  private static final MapObject MO_3 = new MapObject(new LatLon(0, 0), "key1", "", "", 0, 0, 0);
  private static final MapObject MO_NULL_KEY = new MapObject(new LatLon(0, 0), "", "", "", 0, 0, 0);
  private static final MapObject MO_NULL_KEY2 = new MapObject(new LatLon(0, 0), "", "", "", 0, 0, 0);

  private static Field iconUrlGen;
  private static Object iconUrlGenValue;

  @BeforeClass
  public static void setUp() throws IllegalArgumentException, IllegalAccessException, NoSuchFieldException, SecurityException {
    // Sets the keys of the null-key-constants to null
    Field keyField = MapObject.class.getDeclaredField("key");
    keyField.setAccessible(true);
    keyField.set(MO_NULL_KEY, null);
    keyField.set(MO_NULL_KEY2, null);

    // Replace method for generating icon URL with one that searches the local resources for files
    iconUrlGen = MapObject.class.getDeclaredField("iconUrlGen");
    iconUrlGen.setAccessible(true);
    iconUrlGenValue = iconUrlGen.get(null);
  }

  @Before
  public void beforeTest() throws IllegalArgumentException, IllegalAccessException {
    iconUrlGen.set(null, (Function<String, URL>) (str -> {
      URL result = MapObject.class.getResource(str);
      if (result != null) {
        return result;
      }
      try {
        return new URL("https://invalidURL" + str);
      } catch (MalformedURLException e) {
        return null;
      }
    }));
  }

  @AfterClass
  public static void cleanUp() throws IllegalArgumentException, IllegalAccessException {
    iconUrlGen.set(null, iconUrlGenValue);
  }

  @SuppressWarnings({ "unused", "PMD.AvoidDuplicateLiterals" })
  @Test(expected = IllegalArgumentException.class)
  public void testIllArgEx1() {
    new MapObject(new LatLon(0, 0), null, "", "", 0, 0, 0);
  }

  @SuppressWarnings("unused")
  @Test(expected = IllegalArgumentException.class)
  public void testIllArgEx2() {
    new MapObject(new LatLon(0, 0), "", null, "", 0, 0, 0);
  }

  @SuppressWarnings("unused")
  @Test(expected = IllegalArgumentException.class)
  public void testIllArgEx3() {
    new MapObject(new LatLon(0, 0), "", "", null, 0, 0, 0);
  }

  @SuppressWarnings("unused")
  @Test(expected = IllegalArgumentException.class)
  public void testIllArgEx4() {
    new MapObject(null, "", "", "", 0, 0, 0);
  }

  @Test
  public void testIcon() throws SecurityException, IllegalArgumentException {
    MapObject mo = new MapObject(new LatLon(0, 0), "", "", "/images/mapicon.png", 0, 0, 0);
    assertNull(mo.getIcon(false));
    assertNotNull(mo.getIcon(true));
    assertNotNull(mo.getIcon(true));
    assertNotNull(mo.getIcon(false));
    assertNotNull(new MapObject(new LatLon(0, 0), "", "", "/images/mapicon.png", 0, 0, 0).getIcon(true));
  }

  @Test
  public void testNullCache() throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
    Field mapObjectIconCache = MapObject.class.getDeclaredField("MAP_OBJECT_ICON_CACHE");

    Field modifiers = Field.class.getDeclaredField("modifiers");
    modifiers.setAccessible(true);
    modifiers.setInt(mapObjectIconCache, mapObjectIconCache.getModifiers() & ~Modifier.FINAL);

    mapObjectIconCache.setAccessible(true);
    mapObjectIconCache.set(null, null);

    assertNotNull(new MapObject(new LatLon(0, 0), "", "", "/images/mapicon.png", 0, 0, 0).getIcon(true));
  }

  @Test
  public void testInvalidIconDownloadURL() throws IllegalArgumentException, IllegalAccessException {
    assertNull(new MapObject(new LatLon(0, 0), "", "", "/invalidPathToIcon", 0, 0, 0).getIcon(true));
  }

  @Test
  @SuppressWarnings({ "PMD.EqualsNull", "PMD.PositionLiteralsFirstInComparisons" })
  public void testEquals() throws SecurityException, IllegalArgumentException {
    assertEquals(31, MO_NULL_KEY.hashCode());
    assertTrue(MO_1.equals(MO_1));
    assertFalse(MO_1.equals(null));
    assertFalse(MO_1.equals(""));
    assertFalse(MO_NULL_KEY.equals(MO_1));
    assertTrue(MO_NULL_KEY.equals(MO_NULL_KEY2));
    assertFalse(MO_1.equals(MO_2));
    assertTrue(MO_1.equals(MO_3));
  }

}
