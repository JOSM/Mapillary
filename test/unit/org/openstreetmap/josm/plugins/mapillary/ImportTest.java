package org.openstreetmap.josm.plugins.mapillary;

import static org.junit.Assert.*;

import org.junit.Test;

import java.io.File;
import java.io.IOException;

import javax.imageio.IIOException;

import org.apache.commons.imaging.common.RationalNumber;
import org.apache.commons.imaging.formats.tiff.constants.GpsTagConstants;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.plugins.mapillary.actions.MapillaryImportAction;

/**
 * Test the importation of images.
 *
 * @author nokutu
 *
 */
public class ImportTest extends AbstractTest {

  /**
   * Test the importation of images in PNG format.
   */
  @Test
  public void importNoTagsTest() {
    File image = new File("images/icon16.png");
    MapillaryImportedImage img = new MapillaryImportAction().readNoTags(image,
        new LatLon(0, 0));
    assertEquals(0, img.getCa(), 0.01);
    assertTrue(new LatLon(0, 0).equalsEpsilon(img.getLatLon()));
  }

  /**
   * Test if provided an invalid file, the proper exception is thrown.
   *
   * @throws IOException
   */
  @Test(expected = IIOException.class)
  public void testInvalidFiles() throws IOException {
    MapillaryImportedImage img = new MapillaryImportedImage(0, 0, 0, null);
    assertEquals(null, img.getImage());
    assertEquals(null, img.getFile());

    img = new MapillaryImportedImage(0, 0, 0, new File(""));
    assertEquals(new File(""), img.getFile());
    img.getImage();
  }

  /**
   * Test degMinSecToDouble method.
   */
  @Test
  public void degMinSecToDoubleTest() {
    RationalNumber[] num = new RationalNumber[3];
    num[0] = new RationalNumber(1, 1);
    num[1] = new RationalNumber(0, 1);
    num[2] = new RationalNumber(0, 1);
    String ref = GpsTagConstants.GPS_TAG_GPS_LATITUDE_REF_VALUE_NORTH;
    assertEquals(1, MapillaryImportAction.degMinSecToDouble(num, ref), 0.01);
    ref = GpsTagConstants.GPS_TAG_GPS_LATITUDE_REF_VALUE_SOUTH;
    assertEquals(-1, MapillaryImportAction.degMinSecToDouble(num, ref), 0.01);
    num[0] = new RationalNumber(180, 1);
    assertEquals(-180, MapillaryImportAction.degMinSecToDouble(num, ref), 0.01);
    num[0] = new RationalNumber(190, 1);
    assertEquals(170, MapillaryImportAction.degMinSecToDouble(num, ref), 0.01);
  }

}
