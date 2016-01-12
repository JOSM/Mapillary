/**
 *
 */
package org.openstreetmap.josm.plugins.mapillary;

import static org.junit.Assert.assertEquals;

import java.util.TimeZone;

import org.junit.Test;
import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.data.coor.LatLon;

/**
 *
 */
public class MapillaryAbstractImageTest extends AbstractTest{

  /**
   * Test method for {@link org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage#getDate()}.
   */
  @Test
  public void testGetDate() {
    TimeZone.setDefault(TimeZone.getTimeZone("GMT+0745"));

    MapillaryAbstractImage mai = new MapillaryImportedImage(new LatLon(0, 0), 0, null);
    mai.setCapturedAt(1044087606000l); // in timezone GMT+0745 this is Saturday, February 1, 2003 16:05:06


    testGetDate("01/02/2003", mai, false, false, false);
    testGetDate("01/02/2003", mai, false, false, true);
    testGetDate("01/02/2003 - 4:05:06 PM (GMT+07:45)", mai, false, true, false);
    testGetDate("01/02/2003 - 16:05:06 (GMT+07:45)", mai, false, true, true);
    testGetDate("2003-02-01", mai, true, false, false);
    testGetDate("2003-02-01", mai, true, false, true);
    testGetDate("2003-02-01 - 4:05:06 PM (GMT+07:45)", mai, true, true, false);
    testGetDate("2003-02-01 - 16:05:06 (GMT+07:45)", mai, true, true, true);

    TimeZone.setDefault(TimeZone.getTimeZone("GMT-0123"));
    mai.setCapturedAt(1440671802000l); // in Timezone GMT-0123 this is Thursday, August 27, 2015 09:13:42 AM

    testGetDate("27/08/2015 - 09:13:42 (GMT-01:23)", mai, false, true, true);
  }

  private static void testGetDate(String expected, MapillaryAbstractImage img,
      boolean isoDates, boolean displayHour, boolean format24) {
    Main.pref.put("iso.dates", isoDates);
    Main.pref.put("mapillary.display-hour", displayHour);
    Main.pref.put("mapillary.format-24", format24);
    assertEquals(expected, img.getDate());
  }

}
