package org.openstreetmap.josm.plugins.mapillary;

import java.io.IOException;

import org.junit.Before;
import org.junit.Test;

public class ImportedImageTest {

    @Before
    public void setUp() {
        MapillaryData.TEST_MODE = true;
    }

    @Test(expected=IllegalArgumentException.class)
    public void testNullFile() throws IOException {
        MapillaryImportedImage img = new MapillaryImportedImage(0,0,0, null);
        img.getImage();
    }
}
