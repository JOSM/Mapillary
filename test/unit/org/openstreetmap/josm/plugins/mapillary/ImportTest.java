// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.io.IOException;

import javax.imageio.IIOException;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.RegisterExtension;

import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.plugins.mapillary.data.image.MapillaryImportedImage;
import org.openstreetmap.josm.plugins.mapillary.utils.ImageImportUtil;
import org.openstreetmap.josm.testutils.JOSMTestRules;

/**
 * Test the importation of images.
 *
 * @author nokutu
 */
class ImportTest {
  /*
   * Preferences are required to avoid an NPE in OsmPrimitive#compileDirectionKeys
   * Due to ordering, this also affected many other tests.
   */
  @RegisterExtension
  JOSMTestRules rule = new JOSMTestRules().preferences();

  /**
   * Test the importation of images in PNG format.
   */
  @Test
  void importNoTagsTest() throws IOException {
    File image = new File(getClass().getResource("/exifTestImages/untagged.jpg").getFile());
    MapillaryImportedImage img = ImageImportUtil.readImagesFrom(image, new LatLon(0, 0)).get(0);
    assertEquals(0, img.getMovingCa(), 0.01);
    assertTrue(new LatLon(0, 0).equalsEpsilon(img.getMovingLatLon()));
  }

  /**
   * Test if provided an invalid file, the proper exception is thrown.
   */
  @Test
  void testInvalidFiles() throws IOException {
    MapillaryImportedImage img = new MapillaryImportedImage(new LatLon(0, 0), 0, null, false);
    assertNull(img.getImage());
    assertNull(img.getFile());

    MapillaryImportedImage img2 = new MapillaryImportedImage(new LatLon(0, 0), 0, new File(""), false);
    assertEquals(new File(""), img2.getFile());
    assertThrows(IIOException.class, () -> img2.getImage());
  }
}
