// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.Arrays;

import org.junit.Before;
import org.junit.Test;

import org.openstreetmap.josm.data.coor.LatLon;

/**
 * Tests for the {@link MapillarySequence} class.
 *
 * @author nokutu
 * @see MapillarySequence
 */
public class MapillarySequenceTest {

  private final MapillaryImage img1 = new MapillaryImage("key1", new LatLon(0.1, 0.1), 90, false);
  private final MapillaryImage img2 = new MapillaryImage("key2", new LatLon(0.2, 0.2), 90, false);
  private final MapillaryImage img3 = new MapillaryImage("key3", new LatLon(0.3, 0.3), 90, false);
  private final MapillaryImage img4 = new MapillaryImage("key4", new LatLon(0.4, 0.4), 90, false);
  private final MapillaryImage imgWithoutSeq = new MapillaryImage("key5", new LatLon(0.5, 0.5), 90, false);
  private final MapillarySequence seq = new MapillarySequence();

  /**
   * Creates 4 {@link MapillaryImage} objects and puts them in a
   * {@link MapillarySequence} object.
   */
  @Before
  public void setUp() {
    seq.add(Arrays.asList(img1, img2, img3, img4));
  }

  /**
   * Tests the {@link MapillarySequence#next(MapillaryAbstractImage)} and
   * {@link MapillarySequence#previous(MapillaryAbstractImage)}.
   */
  @Test
  public void nextAndPreviousTest() {
    assertEquals(img2, img1.next());
    assertEquals(img1, img2.previous());
    assertEquals(img3, img2.next());
    assertEquals(img2, img3.previous());
    assertEquals(img4, img3.next());
    assertEquals(img3, img4.previous());


    assertNull(img4.next());
    assertNull(img1.previous());

    assertNull(imgWithoutSeq.next());
    assertNull(imgWithoutSeq.previous());

    // Test IllegalArgumentException when asking for the next image of an image
    // that is not in the sequence.
    try {
      seq.next(imgWithoutSeq);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(true);
    }
    // Test IllegalArgumentException when asking for the previous image of an
    // image that is not in the sequence.
    try {
      seq.previous(imgWithoutSeq);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(true);
    }
  }
}
