// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui;

import static org.junit.Assert.assertEquals;

import java.awt.GraphicsEnvironment;
import java.awt.event.MouseEvent;
import java.awt.event.MouseWheelEvent;
import java.awt.image.BufferedImage;

import javax.swing.JFrame;

import org.junit.Rule;
import org.junit.Test;

import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil.MapillaryTestRules;
import org.openstreetmap.josm.testutils.JOSMTestRules;

/**
 * Tests {@link MapillaryImageDisplay}
 */
public class ImageDisplayTest {

  @Rule
  public JOSMTestRules rules = new MapillaryTestRules().preferences();

  private static final BufferedImage DUMMY_IMAGE = new BufferedImage(1, 1, BufferedImage.TYPE_INT_RGB);

  @Test
  public void testImagePersistence() {
    MapillaryImageDisplay display = new MapillaryImageDisplay();
    display.setImage(DUMMY_IMAGE, null, false);
    assertEquals(DUMMY_IMAGE, display.getImage());
  }

  /**
   * This test does not check if the scroll events result in the correct changes in the {@link MapillaryImageDisplay},
   * it only checks if the tested method runs through.
   */
  @Test
  public void testMouseWheelMoved() {
    if (GraphicsEnvironment.isHeadless()) {
      return;
    }
    MapillaryImageDisplay display = new MapillaryImageDisplay();
    final MouseWheelEvent dummyScroll = new MouseWheelEvent(display, 42, System.currentTimeMillis(), 0, 0, 0, 0, false, MouseWheelEvent.WHEEL_UNIT_SCROLL, 1, 3);
    display.getMouseWheelListeners()[0].mouseWheelMoved(dummyScroll);

    display.setImage(DUMMY_IMAGE, null, false);

    display.getMouseWheelListeners()[0].mouseWheelMoved(dummyScroll);

    // This is necessary to make the size of the component > 0. If you know a more elegant solution, feel free to change it.
    JFrame frame = new JFrame();
    frame.setSize(42, 42);
    frame.getContentPane().add(display);
    frame.pack();

    display.getMouseWheelListeners()[0].mouseWheelMoved(dummyScroll);
  }

  /**
   * This test does not check if the scroll events result in the correct changes in the {@link MapillaryImageDisplay},
   * it only checks if the tested method runs through.
   */
  @Test
  public void testMouseClicked() {
    if (GraphicsEnvironment.isHeadless()) {
      return;
    }
    for (int button = 1; button <= 3; button++) {
      MapillaryImageDisplay display = new MapillaryImageDisplay();
      final MouseEvent dummyClick = new MouseEvent(display, 42, System.currentTimeMillis(), 0, 0, 0, 1, false, button);
      display.getMouseListeners()[0].mouseClicked(dummyClick);

      display.setImage(DUMMY_IMAGE, null, false);

      display.getMouseListeners()[0].mouseClicked(dummyClick);

      // This is necessary to make the size of the component > 0. If you know a more elegant solution, feel free to change it.
      JFrame frame = new JFrame();
      frame.setSize(42, 42);
      frame.getContentPane().add(display);
      frame.pack();

      display.getMouseListeners()[0].mouseClicked(dummyClick);
    }
  }
}
