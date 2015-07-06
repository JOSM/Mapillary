package org.openstreetmap.josm.plugins.mapillary;

import static org.junit.Assert.*;

import java.io.IOException;

import org.junit.Before;
import org.junit.Test;

public class ImportTest {

	@Before
	public void setUp() {
		MapillaryData.TEST_MODE = true;

	}
	

    @Test(expected=IllegalArgumentException.class)
    public void test() throws IOException {
        MapillaryImportedImage img = new MapillaryImportedImage(0,0,0, null);
        img.getImage();
    }

}
