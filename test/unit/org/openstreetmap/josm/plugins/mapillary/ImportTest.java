// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.io.IOException;
import java.util.concurrent.ExecutionException;

import javax.imageio.IIOException;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.RegisterExtension;

import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.data.vector.VectorNode;
import org.openstreetmap.josm.plugins.mapillary.utils.ImageImportUtil;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
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
  static JOSMTestRules rule = new JOSMTestRules().preferences();

  /**
   * Test the importation of images in PNG format.
   */
  @Test
  void importNoTagsTest() throws IOException {
    File image = new File(getClass().getResource("/exifTestImages/untagged.jpg").getFile());
    INode img = ImageImportUtil.readImagesFrom(image, new LatLon(0, 0)).get(0);
    assertEquals(0, MapillaryImageUtils.getAngle(img), 0.01);
    assertTrue(new LatLon(0, 0).equalsEpsilon(img.getCoor()));
  }

  /**
   * Test if provided an invalid file, the proper exception is thrown.
   */
  @Test
  void testInvalidFiles() throws IOException, ExecutionException, InterruptedException {
    INode img = new VectorNode("test");
    img.setCoor(new LatLon(0, 0));
    assertNull(MapillaryImageUtils.getImage(img).get());
    assertNull(MapillaryImageUtils.getFile(img));

    INode img2 = new VectorNode("test");
    img2.setCoor(new LatLon(0, 0));
    img2.put(MapillaryImageUtils.IMPORTED_KEY, "///////////");
    assertEquals(new File("///////////"), MapillaryImageUtils.getFile(img2));
    ExecutionException executionException = assertThrows(ExecutionException.class,
      () -> MapillaryImageUtils.getImage(img2).get());
    assertTrue(executionException.getCause() instanceof IIOException);
  }
}
