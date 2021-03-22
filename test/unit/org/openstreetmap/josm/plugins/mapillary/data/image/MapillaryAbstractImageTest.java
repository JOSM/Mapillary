// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.data.image;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.awt.Color;
import java.awt.Image;

import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.testutils.JOSMTestRules;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.RegisterExtension;

/**
 * Test class for common functionality/issues for {@link MapillaryAbstractImage}.
 */
class MapillaryAbstractImageTest {
  @RegisterExtension
  JOSMTestRules rule = new JOSMTestRules();

  /**
   * A minimal class that only implements the abstract methods, and overrides nothing else.
   */
  private static class MinimalMapillaryImage extends MapillaryAbstractImage {
    MinimalMapillaryImage(LatLon latLon, double ca, boolean pano) {
      super(latLon, ca, pano);
    }

    @Override
    public Color paintHighlightedAngleColour() {
      return null;
    }

    @Override
    public Color paintSelectedAngleColour() {
      return null;
    }

    @Override
    public Color paintUnselectedAngleColour() {
      return null;
    }

    @Override
    public Image getDefaultImage() {
      return null;
    }
  }

  /**
   * Non-regression test for JOSM 20622 and 20640.
   */
  @Test
  void testNpeLatLon() {
    MapillaryAbstractImage image = new MinimalMapillaryImage(LatLon.ZERO, 0, false);
    assertEquals(LatLon.ZERO.lat(), image.getLatLon().lat());
    assertEquals(LatLon.ZERO.lon(), image.getLatLon().lat());
  }
}
