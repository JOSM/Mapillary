package org.openstreetmap.josm.plugins.mapillary;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;
import org.openstreetmap.josm.plugins.mapillary.util.TestUtil;

@RunWith(Suite.class)
@SuiteClasses({ ImportTest.class,
    MapillarySequenceDownloadThreadTest.class})
public class AllTests {
}
