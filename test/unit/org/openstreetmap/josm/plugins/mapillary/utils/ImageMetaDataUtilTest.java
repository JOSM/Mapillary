// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;

import org.junit.jupiter.api.Test;

class ImageMetaDataUtilTest {
    @Test
    void testXmpXmlParse() throws IOException, URISyntaxException {
        boolean pano = ImageMetaDataUtil.checkXmpProjectionType(new String(
            Files.readAllBytes(
                Paths.get(ImageMetaDataUtil.class.getResource("/xmpTestImages/xmpProjectionOnly.xml").toURI())),
            StandardCharsets.UTF_8), "equirectangular");
        assertTrue(pano);
    }

    @Test
    void testImageFileXMP() throws URISyntaxException {
        final File xmpImageFile = new File(
            ImageMetaDataUtil.class.getResource("/xmpTestImages/xmpProjectionOnly.jpg").toURI());
        boolean pano = ImageMetaDataUtil.isPanorama(xmpImageFile);
        assertTrue(pano);
    }

    @Test
    void testImageStreamingXMP() throws URISyntaxException, IOException {
        try (FileInputStream xmpImageStream = new FileInputStream(
            new File(ImageMetaDataUtil.class.getResource("/xmpTestImages/xmpProjectionOnly.jpg").toURI()))) {
            boolean pano = ImageMetaDataUtil.isPanorama(xmpImageStream);
            assertTrue(pano);
        }
    }
}
