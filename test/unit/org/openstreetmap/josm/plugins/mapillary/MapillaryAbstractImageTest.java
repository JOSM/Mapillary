// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.TimeZone;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.RegisterExtension;

import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil.MapillaryTestRules;
import org.openstreetmap.josm.spi.preferences.Config;
import org.openstreetmap.josm.testutils.JOSMTestRules;

class MapillaryAbstractImageTest {

  @RegisterExtension
  JOSMTestRules rules = new MapillaryTestRules();

  private MapillaryAbstractImage img1;
  private MapillaryAbstractImage img2;
  private MapillaryAbstractImage img3;
  private MapillaryAbstractImage img4;
  private MapillaryAbstractImage img5;
  final MapillarySequence seq = new MapillarySequence();
  private final Map<MapillaryAbstractImage, Integer> changesHash = new HashMap<>();

  /**
   * Test method for {@link org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage#getDate()}.
   */
  @Test
  void testGetDate() {
    TimeZone.setDefault(TimeZone.getTimeZone("GMT+0745"));

    MapillaryAbstractImage img = new MapillaryImportedImage(new LatLon(0, 0), 0, null, false);
    img.setCapturedAt(1_044_087_606_000L); // in timezone GMT+0745 this is Saturday, February 1, 2003 16:05:06

    testGetDate("01/02/2003", img, false, false, false);
    testGetDate("01/02/2003", img, false, false, true);
    testGetDate("01/02/2003 - 4:05:06 PM (GMT+07:45)", img, false, true, false);
    testGetDate("01/02/2003 - 16:05:06 (GMT+07:45)", img, false, true, true);
    testGetDate("2003-02-01", img, true, false, false);
    testGetDate("2003-02-01", img, true, false, true);
    testGetDate("2003-02-01 - 4:05:06 PM (GMT+07:45)", img, true, true, false);
    testGetDate("2003-02-01 - 16:05:06 (GMT+07:45)", img, true, true, true);

    TimeZone.setDefault(TimeZone.getTimeZone("GMT-0123"));
    img.setCapturedAt(1_440_671_802_000L); // in Timezone GMT-0123 this is Thursday, August 27, 2015 09:13:42 AM

    testGetDate("27/08/2015 - 09:13:42 (GMT-01:23)", img, false, true, true);
  }

  private static void testGetDate(String expected, MapillaryAbstractImage img, boolean isoDates, boolean displayHour,
    boolean format24) {
    Config.getPref().putBoolean("iso.dates", isoDates);
    Config.getPref().putBoolean("mapillary.display-hour", displayHour);
    Config.getPref().putBoolean("mapillary.format-24", format24);
    assertEquals(expected, img.getDate());
  }

  @Test
  void testIsModified() {
    MapillaryImage img = new MapillaryImage("key___________________", new LatLon(0, 0), 0, false, false);
    assertFalse(img.isModified());
    img.turn(1e-4);
    img.stopMoving();
    assertTrue(img.isModified());
    img.turn(-1e-4);
    img.stopMoving();
    assertFalse(img.isModified());
    img.move(1e-4, 1e-4);
    img.stopMoving();
    assertTrue(img.isModified());
    img.move(-1e-4, -1e-4);
    img.stopMoving();
    assertFalse(img.isModified());
  }

  /**
   * Test method for {@link MapillaryAbstractImage#compareTo()}, .
   */
  @Test
  void testCompareTo() {
    img1 = new MapillaryImage("key1__________________", new LatLon(0.1, 0.1), 90, false, false);
    img2 = new MapillaryImage("key2__________________", new LatLon(0.2, 0.2), 90, false, false);
    img3 = new MapillaryImage("key3__________________", new LatLon(0.3, 0.3), 90, false, false);
    img4 = new MapillaryImportedImage(new LatLon(0.4, 0.4), 90, null, false);
    img5 = new MapillaryImportedImage(new LatLon(0.5, 0.5), 90, null, false);
    img1.setCapturedAt(1);
    img2.setCapturedAt(0);
    img3.setCapturedAt(0);
    img4.setCapturedAt(1);
    seq.add(Arrays.asList(img1, img2, img3));

    assertEquals(-1, img1.compareTo(img2));
    assertEquals(-1, img2.compareTo(img3));
    assertNotEquals(0, img1.compareTo(img4));
    assertNotEquals(0, img4.compareTo(img5));
    assertNotEquals(img4.hashCode(), img1.hashCode());
    assertNotEquals(img4.hashCode(), img2.hashCode());
    assertNotEquals(img4.hashCode(), img3.hashCode());
    assertNotEquals(img4.hashCode(), img5.hashCode());
    changesHash.put(img1, img1.getSequence().getImages().indexOf(img1));
    changesHash.put(img2, img2.getSequence().getImages().indexOf(img2));
    changesHash.put(img3, img3.getSequence().getImages().indexOf(img3));

    // We don't create a default sequence anymore for images.
    changesHash.put(img4, 0);
    changesHash.put(img5, 0);
    assertEquals(5, changesHash.size());
  }
}
