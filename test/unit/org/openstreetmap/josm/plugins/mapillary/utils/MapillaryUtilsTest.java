// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import static org.junit.Assert.assertEquals;

import org.apache.commons.imaging.common.RationalNumber;
import org.apache.commons.imaging.formats.tiff.constants.GpsTagConstants;
import org.junit.Test;

/**
 * Tests the static methods of the class {@link MapillaryUtils}
 *
 * @see MapillaryUtils
 * @author nokutu
 *
 */
public class MapillaryUtilsTest {

  @Test
  public void testUtilityClass() {
    TestUtil.testUtilityClass(MapillaryUtils.class);
  }

  /**
   * Test {@link MapillaryUtils#degMinSecToDouble(RationalNumber[], String)}
   * method.
   */
  @Test
  public void degMinSecToDoubleTest() {
    RationalNumber[] num = new RationalNumber[3];
    num[0] = new RationalNumber(1, 1);
    num[1] = new RationalNumber(0, 1);
    num[2] = new RationalNumber(0, 1);
    String ref = GpsTagConstants.GPS_TAG_GPS_LATITUDE_REF_VALUE_NORTH;
    assertEquals(1, MapillaryUtils.degMinSecToDouble(num, ref), 0.01);
    ref = GpsTagConstants.GPS_TAG_GPS_LATITUDE_REF_VALUE_SOUTH;
    assertEquals(-1, MapillaryUtils.degMinSecToDouble(num, ref), 0.01);
    num[0] = new RationalNumber(180, 1);
    assertEquals(-180, MapillaryUtils.degMinSecToDouble(num, ref), 0.01);
    num[0] = new RationalNumber(190, 1);
    assertEquals(170, MapillaryUtils.degMinSecToDouble(num, ref), 0.01);
  }
}
