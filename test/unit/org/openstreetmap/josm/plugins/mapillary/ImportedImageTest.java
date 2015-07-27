package org.openstreetmap.josm.plugins.mapillary;

import static org.junit.Assert.assertEquals;

import java.io.File;
import java.io.IOException;

import javax.imageio.IIOException;

import org.junit.Before;
import org.junit.Test;

public class ImportedImageTest {

    @Before
    public void setUp() {
        MapillaryData.TEST_MODE = true;
    }

    @Test(expected=IIOException.class)
    public void testInvalidFiles() throws IOException {
        MapillaryImportedImage img = new MapillaryImportedImage(0,0,0, null);
        assertEquals(null, img.getImage());
        assertEquals(null, img.getFile());

        img = new MapillaryImportedImage(0, 0, 0, new File(""));
        assertEquals(new File(""), img.getFile());
        img.getImage();
    }
}
