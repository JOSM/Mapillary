package org.openstreetmap.josm.plugins.mapillary;

import static org.junit.Assert.assertEquals;
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

  private MapillaryImage img1;
  private MapillaryImage img2;
  private MapillaryImage img3;
  private MapillaryImage img4;
  private MapillarySequence seq;

  /**
   * Creates 4 {@link MapillaryImage} objects and puts them in a
   * {@link MapillarySequence} object.
   */
  @Before
  public void setUp() {
    img1 = new MapillaryImage("key1__________________", new LatLon(0.1, 0.1), 90);
    img2 = new MapillaryImage("key2__________________", new LatLon(0.2, 0.2), 90);
    img3 = new MapillaryImage("key3__________________", new LatLon(0.3, 0.3), 90);
    img4 = new MapillaryImage("key4__________________", new LatLon(0.4, 0.4), 90);
    seq = new MapillarySequence();

    seq.add(Arrays.asList(new MapillaryAbstractImage[] { img1, img2, img3, img4 }));
    img1.setSequence(seq);
    img2.setSequence(seq);
    img3.setSequence(seq);
    img4.setSequence(seq);
  }

  /**
   * Tests the {@link MapillarySequence#next(MapillaryAbstractImage)} and
   * {@link MapillarySequence#previous(MapillaryAbstractImage)}.
   */
  @Test
  public void nextAndPreviousTest() {
    assertEquals(this.img2, this.img1.next());
    assertEquals(this.img2, this.seq.next(this.img1));

    assertEquals(this.img1, this.img2.previous());
    assertEquals(this.img1, this.seq.previous(this.img2));

    assertEquals(null, this.img4.next());
    assertEquals(null, this.seq.next(this.img4));
    assertEquals(null, this.img1.previous());
    assertEquals(null, this.seq.previous(this.img1));

    // Test IllegalArgumentException when asking for the next image of an image
    // that is not in the sequence.
    try {
      this.seq.next(new MapillaryImage("key5__________________", new LatLon(0.5, 0.5), 90));
      fail();
    } catch (IllegalArgumentException e) {
    }
    // Test IllegalArgumentException when asking for the previous image of an
    // image that is not in the sequence.
    try {
      this.seq.previous(new MapillaryImage("key5", new LatLon(0.5, 0.5), 90));
      fail();
    } catch (IllegalArgumentException e) {
    }
  }
}
