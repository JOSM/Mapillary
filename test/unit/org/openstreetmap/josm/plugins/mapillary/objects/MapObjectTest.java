// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.objects;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.lang.reflect.Field;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.function.Function;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil;

public class MapObjectTest {

  private static final MapObject MO_1 = new MapObject(new LatLon(0, 0), "key1", "", "", 0, 0, 0);
  private static final MapObject MO_2 = new MapObject(new LatLon(0, 0), "key2", "", "", 0, 0, 0);
  private static final MapObject MO_3 = new MapObject(new LatLon(0, 0), "key1", "", "", 0, 0, 0);
  private static final MapObject MO_NULL_KEY = new MapObject(new LatLon(0, 0), "", "", "", 0, 0, 0);
  private static final MapObject MO_NULL_KEY2 = new MapObject(new LatLon(0, 0), "", "", "", 0, 0, 0);

  private static Field iconUrlGen;
  private static Object iconUrlGenValue;

  @BeforeClass
  public static void setUp() throws IllegalArgumentException, IllegalAccessException {
    // Sets the keys of the null-key-constants to null
    Field keyField = TestUtil.getAccessibleField(MapObject.class, "key");
    keyField.set(MO_NULL_KEY, null);
    keyField.set(MO_NULL_KEY2, null);

    // Replace function for generating icon URLs with one that searches the local resources for files
    // If a resource can't be found, return an invalid URL
    iconUrlGen = TestUtil.getAccessibleField(MapObject.class, "iconUrlGen");
    iconUrlGenValue = iconUrlGen.get(null);
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
    assertNotNull(MapObject.getIcon("/images/mapicon.png"));
    assertNotNull(MapObject.getIcon("/images/mapicon.png"));
  }

  @Test
  public void testNullCache() throws IllegalArgumentException, IllegalAccessException {
    TestUtil.getAccessibleField(MapObject.class, "MAP_OBJECT_ICON_CACHE").set(null, null);
    assertNotNull(MapObject.getIcon("/images/mapicon.png"));
  }

  @Test
  public void testInvalidIconDownloadURL() throws IllegalArgumentException, IllegalAccessException {
    assertEquals(TestUtil.getAccessibleField(MapObject.class, "ICON_UNKNOWN_TYPE").get(null), MapObject.getIcon("/invalidPathToIcon"));
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
