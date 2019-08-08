// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Paths;

import org.junit.Test;


public class ImageMetaDataUtilTest {
  @Test
  public void testXmpXmlParse() throws IOException, URISyntaxException {
    boolean pano = ImageMetaDataUtil.checkXmpProjectionType(new String(Files.readAllBytes(Paths.get(ImageMetaDataUtil.class.getResource(
      "/xmpTestImages/xmpProjectionOnly.xml").toURI()))), "equirectangular");
    assertTrue(pano);
  }

  @Test
  public void testImageFileXMP() throws URISyntaxException {
    final File xmpImageFile = new File(ImageMetaDataUtil.class.getResource("/xmpTestImages/xmpProjectionOnly.jpg").toURI());
    boolean pano = ImageMetaDataUtil.isPanorama(xmpImageFile);
    assertTrue(pano);
  }

  @Test
  public void testImageStreamingXMP() throws URISyntaxException, FileNotFoundException {
    final FileInputStream xmpImageStream = new FileInputStream(new File(ImageMetaDataUtil.class.getResource("/xmpTestImages/xmpProjectionOnly.jpg").toURI()));
    boolean pano = ImageMetaDataUtil.isPanorama(xmpImageStream);
    assertTrue(pano);
  }
}
