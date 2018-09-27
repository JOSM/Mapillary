// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import java.util.Arrays;
import java.util.concurrent.ConcurrentSkipListSet;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;

import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil.MapillaryTestRules;
import org.openstreetmap.josm.testutils.JOSMTestRules;

/**
 * Tests for {@link MapillaryData} class.
 *
 * @author nokutu
 * @see MapillaryData
 */
public class MapillaryDataTest {

  @Rule
  public JOSMTestRules rules = new MapillaryTestRules();

  private MapillaryData data;
  private MapillaryImage img1;
  private MapillaryImage img2;
  private MapillaryImage img3;
  private MapillaryImage img4;

  /**
   * Creates a sample {@link MapillaryData} objects, 4 {@link MapillaryImage}
   * objects and a {@link MapillarySequence} object.
   */
  @Before
  public void setUp() {
    this.img1 = new MapillaryImage("key1__________________", new LatLon(0.1, 0.1), 90, false);
    this.img2 = new MapillaryImage("key2__________________", new LatLon(0.2, 0.2), 90, false);
    this.img3 = new MapillaryImage("key3__________________", new LatLon(0.3, 0.3), 90, false);
    this.img4 = new MapillaryImage("key4__________________", new LatLon(0.4, 0.4), 90, false);
    final MapillarySequence seq = new MapillarySequence();

    seq.add(Arrays.asList(img1, img2, img3, img4));

    this.data = new MapillaryData();
    this.data.addAll(new ConcurrentSkipListSet<>(seq.getImages()));
  }

  /**
   * Tests the addition of new images. If a second image with the same key as
   * another one in the database, the one that is being added should be ignored.
   */
  @Test
  public void addTest() {
    this.data = new MapillaryData();
    assertEquals(0, this.data.getImages().size());
    this.data.add(this.img1);
    assertEquals(1, this.data.getImages().size());
    this.data.add(this.img1);
    assertEquals(1, this.data.getImages().size());
    this.data.addAll(new ConcurrentSkipListSet<>(Arrays.asList(this.img2, this.img3)));
    assertEquals(3, this.data.getImages().size());
    this.data.addAll(new ConcurrentSkipListSet<>(Arrays.asList(this.img3, this.img4)));
    assertEquals(4, this.data.getImages().size());
  }

  /**
   * Test that the size is properly calculated.
   */
  @Test
  public void sizeTest() {
    assertEquals(4, this.data.getImages().size());
    this.data.add(new MapillaryImage("key5__________________", new LatLon(0.1, 0.1), 90, false));
    assertEquals(5, this.data.getImages().size());
  }

  /**
   * Test the {@link MapillaryData#setHighlightedImage(MapillaryAbstractImage)}
   * and {@link MapillaryData#getHighlightedImage()} methods.
   */
  @Test
  public void highlighTest() {
    this.data.setHighlightedImage(this.img1);
    assertEquals(this.img1, this.data.getHighlightedImage());

    this.data.setHighlightedImage(null);
    assertNull(this.data.getHighlightedImage());
  }

  /**
   * Tests the selection of images.
   */
  @Test
  public void selectTest() {
    this.data.setSelectedImage(this.img1);
    assertEquals(this.img1, this.data.getSelectedImage());

    this.data.setSelectedImage(this.img4);
    assertEquals(this.img4, this.data.getSelectedImage());

    this.data.setSelectedImage(null);
    assertNull(this.data.getSelectedImage());
  }

  /**
   * Test the multiselection of images. When a new image is selected, the
   * multiselected List should reset.
   */
  @Test
  public void multiSelectTest() {
    assertEquals(0, this.data.getMultiSelectedImages().size());
    this.data.setSelectedImage(this.img1);
    assertEquals(1, this.data.getMultiSelectedImages().size());
    this.data.addMultiSelectedImage(this.img2);
    assertEquals(2, this.data.getMultiSelectedImages().size());
    this.data.setSelectedImage(this.img1);
    assertEquals(1, this.data.getMultiSelectedImages().size());
  }
}
