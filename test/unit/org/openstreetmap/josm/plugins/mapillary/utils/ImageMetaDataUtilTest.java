// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Paths;

import org.junit.jupiter.api.Test;
import org.openstreetmap.josm.TestUtils;

class ImageMetaDataUtilTest {
    @Test
    void testXmpXmlParse() throws IOException {
        boolean pano = ImageMetaDataUtil.checkXmpProjectionType(
            Files.readString(Paths.get(TestUtils.getTestDataRoot(), "xmpTestImages", "xmpProjectionOnly.xml")),
            "equirectangular");
        assertTrue(pano);
    }

    @Test
    void testImageFileXMP() {
        final File xmpImageFile = Paths.get(TestUtils.getTestDataRoot(), "xmpTestImages", "xmpProjectionOnly.jpg")
            .toFile();
        boolean pano = ImageMetaDataUtil.isPanorama(xmpImageFile);
        assertTrue(pano);
    }

    @Test
    void testImageStreamingXMP() throws IOException {
        try (InputStream xmpImageStream = Files
            .newInputStream(Paths.get(TestUtils.getTestDataRoot(), "xmpTestImages", "xmpProjectionOnly.jpg"))) {
            boolean pano = ImageMetaDataUtil.isPanorama(xmpImageStream);
            assertTrue(pano);
        }
    }
}
