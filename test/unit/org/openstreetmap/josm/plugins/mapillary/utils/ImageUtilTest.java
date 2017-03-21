// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import static org.junit.Assert.assertEquals;

import java.awt.image.BufferedImage;

import javax.swing.ImageIcon;

import org.junit.Test;

public class ImageUtilTest {

  @Test
  public void testUtilityClass() {
    TestUtil.testUtilityClass(ImageUtil.class);
  }

  @Test
  public void testScaleImageIcon() {
    ImageIcon largeLandscape = new ImageIcon(new BufferedImage(72, 60, BufferedImage.TYPE_4BYTE_ABGR));
    ImageIcon largePortrait = new ImageIcon(new BufferedImage(56, 88, BufferedImage.TYPE_4BYTE_ABGR));
    ImageIcon smallLandscape = ImageUtil.scaleImageIcon(largeLandscape, 24);
    ImageIcon smallPortrait = ImageUtil.scaleImageIcon(largePortrait, 22);

    assertEquals(24, smallLandscape.getIconWidth());
    assertEquals(20, smallLandscape.getIconHeight());

    assertEquals(22, smallPortrait.getIconHeight());
    assertEquals(14, smallPortrait.getIconWidth());
  }
}
