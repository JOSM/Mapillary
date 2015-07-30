package org.openstreetmap.josm.plugins.mapillary;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

/**
 * Runs all tests.
 *
 * @author nokutu
 *
 */
@RunWith(Suite.class)
@SuiteClasses({ ImportTest.class, MapillarySequenceDownloadThreadTest.class })
public class AllTests {
}
