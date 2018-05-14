// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;

import javax.imageio.IIOException;

import org.junit.Test;

import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.plugins.mapillary.utils.ImageImportUtil;

/**
 * Test the importation of images.
 *
 * @author nokutu
 *
 */
public class ImportTest {

  /**
   * Test the importation of images in PNG format.
   */
  @Test
  public void importNoTagsTest() throws IOException {
    File image = new File(getClass().getResource("/exifTestImages/untagged.jpg").getFile());
    MapillaryImportedImage img = ImageImportUtil.readImagesFrom(image, new LatLon(0, 0)).get(0);
    assertEquals(0, img.getMovingCa(), 0.01);
    assertTrue(new LatLon(0, 0).equalsEpsilon(img.getMovingLatLon()));
  }

  /**
   * Test if provided an invalid file, the proper exception is thrown.
   *
   * @throws IOException
   */
  @Test(expected = IIOException.class)
  public void testInvalidFiles() throws IOException {
    MapillaryImportedImage img = new MapillaryImportedImage(new LatLon(0, 0), 0, null);
    assertNull(img.getImage());
    assertNull(img.getFile());

    img = new MapillaryImportedImage(new LatLon(0, 0), 0, new File(""));
    assertEquals(new File(""), img.getFile());
    img.getImage();
  }
}
