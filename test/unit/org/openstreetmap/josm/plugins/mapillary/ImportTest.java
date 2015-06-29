package org.openstreetmap.josm.plugins.mapillary;

import static org.junit.Assert.*;

import org.junit.Before;
import org.junit.Test;

public class ImportTest {

	@Before
	public void setUp() {
		MapillaryData.TEST_MODE = true;

	}
	
	@Test
	public void test() {
		MapillaryImportedImage img = new MapillaryImportedImage(0,0,0, null);
		assert(true);
	}

}
