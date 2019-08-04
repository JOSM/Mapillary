// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.net.URISyntaxException;

import org.junit.Test;


public class ImageMetaDataUtilTest {
  @Test
  public void testXmpXmlParse() throws FileNotFoundException, URISyntaxException {
    final File xmpXmlFile = new File(ImageImportUtil.class.getResource("/xmp.xml").toURI());
    boolean pano = ImageMetaDataUtil.getPanorama(new FileReader(xmpXmlFile));
    assertTrue(pano);
  }
}
