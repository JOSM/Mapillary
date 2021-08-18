package org.openstreetmap.josm.plugins.mapillary.testutils.annotations;

import org.junit.jupiter.api.Tag;

/**
 * Mark tests as integration tests.
 *
 * @author Taylor Smock
 */
@Tag(IntegrationTest.TAG)
public interface IntegrationTest {
    /** This is the actual tag string added to the test */
    String TAG = "integration";
}
