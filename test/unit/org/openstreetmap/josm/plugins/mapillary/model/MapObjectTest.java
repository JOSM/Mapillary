// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.model;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.lang.reflect.Field;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.function.Function;

import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;

import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil.MapillaryTestRules;
import org.openstreetmap.josm.testutils.JOSMTestRules;

public class MapObjectTest {

  @Rule
  public JOSMTestRules rules = new MapillaryTestRules().platform();

  private static MapObject mo1;
  private static MapObject mo2;
  private static MapObject mo3;
  private static MapObject moNullKey;
  private static MapObject moNullKey2;

  private static Field iconUrlGen;
  private static Object iconUrlGenValue;

  public static void initMapObjects() {
    mo1 = new MapObject(new LatLon(0, 0), "key1", "", "", 0, 0, 0);
    mo2 = new MapObject(new LatLon(0, 0), "key2", "", "", 0, 0, 0);
    mo3 = new MapObject(new LatLon(0, 0), "key1", "", "", 0, 0, 0);
    moNullKey = new MapObject(new LatLon(0, 0), "", "", "", 0, 0, 0);
    moNullKey2 = new MapObject(new LatLon(0, 0), "", "", "", 0, 0, 0);
  }

  @Before
  public void setUp() throws IllegalArgumentException, IllegalAccessException {
    initMapObjects();
    // Sets the keys of the null-key-constants to null
    Field keyField = TestUtil.getAccessibleField(KeyIndexedObject.class, "key");
    keyField.set(moNullKey, null);
    keyField.set(moNullKey2, null);

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

  @After
  public void cleanUp() throws IllegalArgumentException, IllegalAccessException {
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
  public void testIcon() throws SecurityException, IllegalArgumentException, NoSuchFieldException, IllegalAccessException {
    assertNotNull(MapObject.getIcon("/images/mapicon.png"));
    assertNotNull(MapObject.getIcon("/images/mapicon.png"));

    Field iconUnknownType = MapObject.class.getDeclaredField("ICON_UNKNOWN_TYPE");
    iconUnknownType.setAccessible(true);
    assertEquals(MapObject.getIcon("not-in-set"), iconUnknownType.get(null));
  }

  @Test
  public void testInvalidIconDownloadURL() throws IllegalArgumentException, IllegalAccessException {
    assertEquals(TestUtil.getAccessibleField(MapObject.class, "ICON_UNKNOWN_TYPE").get(null), MapObject.getIcon("/invalidPathToIcon"));
  }

  @Test
  @SuppressWarnings({ "PMD.EqualsNull", "PMD.PositionLiteralsFirstInComparisons" })
  public void testEquals() throws SecurityException, IllegalArgumentException {
    assertEquals(moNullKey2.hashCode(), moNullKey.hashCode());
    assertTrue(mo1.equals(mo1));
    assertFalse(mo1.equals(null));
    assertFalse(mo1.equals(""));
    assertFalse(moNullKey.equals(mo1));
    assertTrue(moNullKey.equals(moNullKey2));
    assertFalse(mo1.equals(mo2));
    assertTrue(mo1.equals(mo3));
    assertEquals(mo1.hashCode(), mo3.hashCode());
  }

}
