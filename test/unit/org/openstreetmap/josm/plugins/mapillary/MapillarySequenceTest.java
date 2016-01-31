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

  MapillaryImage img1;
  MapillaryImage img2;
  MapillaryImage img3;
  MapillaryImage img4;
  MapillarySequence seq;

  /**
   * Creates 4 {@link MapillaryImage} objects and puts them in a
   * {@link MapillarySequence} object.
   */
  @Before
  public void setUp() {
    this.img1 = new MapillaryImage("key1", new LatLon(0.1, 0.1), 90);
    this.img2 = new MapillaryImage("key2", new LatLon(0.2, 0.2), 90);
    this.img3 = new MapillaryImage("key3", new LatLon(0.3, 0.3), 90);
    this.img4 = new MapillaryImage("key4", new LatLon(0.4, 0.4), 90);
    this.seq = new MapillarySequence();

    this.seq.add(Arrays.asList(new MapillaryAbstractImage[] { this.img1,
        this.img2, this.img3, this.img4 }));
    this.img1.setSequence(this.seq);
    this.img2.setSequence(this.seq);
    this.img3.setSequence(this.seq);
    this.img4.setSequence(this.seq);
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
      this.seq.next(new MapillaryImage("key5", new LatLon(0.5, 0.5), 90));
      fail();
    } catch (IllegalArgumentException e) {
    } catch (Exception e) {
      fail();
    }
    // Test IllegalArgumentException when asking for the previous image of an
    // image that is not in the sequence.
    try {
      this.seq.previous(new MapillaryImage("key5", new LatLon(0.5, 0.5), 90));
      fail();
    } catch (IllegalArgumentException e) {
    } catch (Exception e) {
      fail();
    }
  }
}
