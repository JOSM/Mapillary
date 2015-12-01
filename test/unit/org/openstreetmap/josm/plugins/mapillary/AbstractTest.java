package org.openstreetmap.josm.plugins.mapillary;

import org.junit.BeforeClass;
import org.junit.Ignore;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil;

/**
 * Abstract class for tests that require JOSM's preferences running.
 *
 * @author nokutu
 *
 */
@Ignore
public abstract class AbstractTest {

  /**
   * Initiates the basic parts of JOSM.
   */
  @BeforeClass
  public static void setUpBeforeClass() {
    TestUtil.initPlugin();
  }
}
