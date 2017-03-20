// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.objects;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.net.URL;
import java.util.function.Function;

import org.junit.Test;
import org.openstreetmap.josm.data.coor.LatLon;

public class MapObjectTest {

  private static final MapObject MO_1 = new MapObject(new LatLon(0, 0), "key1", "", "", 0, 0, 0);
  private static final MapObject MO_2 = new MapObject(new LatLon(0, 0), "key2", "", "", 0, 0, 0);
  private static final MapObject MO_3 = new MapObject(new LatLon(0, 0), "key1", "", "", 0, 0, 0);
  private static final MapObject MO_NULL_KEY = new MapObject(new LatLon(0, 0), "", "", "", 0, 0, 0);
  private static final MapObject MO_NULL_KEY2 = new MapObject(new LatLon(0, 0), "", "", "", 0, 0, 0);

  static {
    try {
      // Sets the keys of the null-key-constants to null
      Field keyField = MapObject.class.getDeclaredField("key");
      keyField.setAccessible(true);
      keyField.set(MO_NULL_KEY, null);
      keyField.set(MO_NULL_KEY2, null);
    } catch (IllegalArgumentException | IllegalAccessException | NoSuchFieldException | SecurityException e) {
      fail();
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void testIllArgEx1() {
    new MapObject(new LatLon(0, 0), null, "", "", 0, 0, 0);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testIllArgEx2() {
    new MapObject(new LatLon(0, 0), "", null, "", 0, 0, 0);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testIllArgEx3() {
    new MapObject(new LatLon(0, 0), "", "", null, 0, 0, 0);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testIllArgEx4() {
    new MapObject(null, "", "", "", 0, 0, 0);
  }

  @Test
  public void testIcon() throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException, IOException {
    // Replace method for generating icon URL with one that searches the local resources for files
    Field iconUrlGen = MapObject.class.getDeclaredField("iconUrlGen");
    iconUrlGen.setAccessible(true);
    iconUrlGen.set(null, (Function<String, URL>) (str -> {
      return MapObject.class.getResource(str);
    }));

    assertNotNull(new MapObject(new LatLon(0, 0), "", "", "/images/mapicon.png", 0, 0, 0).getIcon());
    assertNotNull(new MapObject(new LatLon(0, 0), "", "", "/images/mapicon.png", 0, 0, 0).getIcon());
  }

  @Test
  public void testNullCache() throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
    Field mapObjectIconCache = MapObject.class.getDeclaredField("MAP_OBJECT_ICON_CACHE");

    Field modifiers = Field.class.getDeclaredField("modifiers");
    modifiers.setAccessible(true);
    modifiers.setInt(mapObjectIconCache, mapObjectIconCache.getModifiers() & ~Modifier.FINAL);
    mapObjectIconCache.setAccessible(true);

    mapObjectIconCache.set(null, null);

    assertNotNull(new MapObject(new LatLon(0, 0), "", "", "", 0, 0, 0).getIcon());
  }

  @Test
  public void testEquals() throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
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
