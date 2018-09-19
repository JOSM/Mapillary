// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.model;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;

import java.lang.reflect.Field;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.function.Function;

import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;

import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.plugins.mapillary.cache.Caches;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil.MapillaryTestRules;
import org.openstreetmap.josm.testutils.JOSMTestRules;

public class MapObjectTest {

  @Rule
  public JOSMTestRules rules = new MapillaryTestRules();

  private static MapObject mo1;
  private static MapObject mo2;
  private static MapObject mo3;

  private static Field iconUrlGen;
  private static Object iconUrlGenValue;

  private static void initMapObjects() {
    mo1 = new MapObject(new LatLon(0, 0), "key1", "", "", 0, 0, 0);
    mo2 = new MapObject(new LatLon(0, 0), "key2", "", "", 0, 0, 0);
    mo3 = new MapObject(new LatLon(0, 0), "key1", "", "", 0, 0, 0);
  }

  @Before
  public void setUp() throws IllegalArgumentException, IllegalAccessException {
    initMapObjects();
    // Sets the keys of the null-key-constants to null
    Field keyField = TestUtil.getAccessibleField(KeyIndexedObject.class, "key");

    // Replace function for generating icon URLs with one that searches the local resources for files
    // If a resource can't be found, return a URL for example.org, which does not exist.
    iconUrlGen = TestUtil.getAccessibleField(MapObject.class, "iconUrlGen");
    iconUrlGenValue = iconUrlGen.get(null);
    iconUrlGen.set(null, (Function<String, URL>) (str -> {
      URL result = MapObject.class.getResource(str);
      if (result != null) {
        return result;
      }
      try {
        return new URL("https://example.org/nonExistent" + str);
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
    final String mapIconKey = "/images/fake-avatar.png";
    assertNotNull(MapObject.getIcon(mapIconKey));
    assertNotNull(Caches.MapObjectIconCache.getInstance().get(mapIconKey));
    assertNotNull(MapObject.getIcon(mapIconKey)); // To test a cache hit

    Field iconUnknownType = MapObject.class.getDeclaredField("ICON_UNKNOWN_TYPE");
    iconUnknownType.setAccessible(true);
    assertEquals(MapObject.getIcon("not-in-set"), iconUnknownType.get(null));
  }

  @Test
  public void testInvalidIconDownloadURL() throws IllegalArgumentException, IllegalAccessException {
    assertEquals(TestUtil.getAccessibleField(MapObject.class, "ICON_UNKNOWN_TYPE").get(null), MapObject.getIcon("/invalidPathToIcon"));
  }

  @Test
  public void testEquals() throws SecurityException, IllegalArgumentException {
    assertEquals(mo1, mo1);
    assertNotEquals(mo1, null);
    assertNotEquals(mo1, "");
    assertNotEquals(mo1, mo2);
    assertEquals(mo1, mo3);
    assertEquals(mo1.hashCode(), mo3.hashCode());
  }

}
