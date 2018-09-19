// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.TimeZone;

import org.junit.Rule;
import org.junit.Test;

import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil.MapillaryTestRules;
import org.openstreetmap.josm.spi.preferences.Config;
import org.openstreetmap.josm.testutils.JOSMTestRules;

public class MapillaryAbstractImageTest {

  @Rule
  public JOSMTestRules rules = new MapillaryTestRules();

  /**
   * Test method for {@link org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage#getDate()}.
   */
  @Test
  public void testGetDate() {
    TimeZone.setDefault(TimeZone.getTimeZone("GMT+0745"));

    MapillaryAbstractImage mai = new MapillaryImportedImage(new LatLon(0, 0), 0, null);
    mai.setCapturedAt(1_044_087_606_000L); // in timezone GMT+0745 this is Saturday, February 1, 2003 16:05:06


    testGetDate("01/02/2003", mai, false, false, false);
    testGetDate("01/02/2003", mai, false, false, true);
    testGetDate("01/02/2003 - 4:05:06 PM (GMT+07:45)", mai, false, true, false);
    testGetDate("01/02/2003 - 16:05:06 (GMT+07:45)", mai, false, true, true);
    testGetDate("2003-02-01", mai, true, false, false);
    testGetDate("2003-02-01", mai, true, false, true);
    testGetDate("2003-02-01 - 4:05:06 PM (GMT+07:45)", mai, true, true, false);
    testGetDate("2003-02-01 - 16:05:06 (GMT+07:45)", mai, true, true, true);

    TimeZone.setDefault(TimeZone.getTimeZone("GMT-0123"));
    mai.setCapturedAt(1_440_671_802_000L); // in Timezone GMT-0123 this is Thursday, August 27, 2015 09:13:42 AM

    testGetDate("27/08/2015 - 09:13:42 (GMT-01:23)", mai, false, true, true);
  }

  private static void testGetDate(String expected, MapillaryAbstractImage img,
      boolean isoDates, boolean displayHour, boolean format24) {
    Config.getPref().putBoolean("iso.dates", isoDates);
    Config.getPref().putBoolean("mapillary.display-hour", displayHour);
    Config.getPref().putBoolean("mapillary.format-24", format24);
    assertEquals(expected, img.getDate());
  }

  @Test
  public void testIsModified() {
    MapillaryImage img = new MapillaryImage("key___________________", new LatLon(0, 0), 0, false);
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
}
