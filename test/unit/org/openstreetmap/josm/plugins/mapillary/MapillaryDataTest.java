package org.openstreetmap.josm.plugins.mapillary;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.util.Arrays;

import org.junit.Before;
import org.junit.Test;

/**
 * Tests for {@link MapillaryData} class.
 *
 * @author nokutu
 * @see MapillaryData
 */
public class MapillaryDataTest extends AbstractTest {

  MapillaryData data;
  MapillaryImage img1;
  MapillaryImage img2;
  MapillaryImage img3;
  MapillaryImage img4;
  MapillarySequence seq;

  /**
   * Creates a sample {@link MapillaryData} objects, 4 {@link MapillaryImage}
   * objects and a {@link MapillarySequence} object.
   */
  @Before
  public void setUp() {
    this.img1 = new MapillaryImage("key1", 0.1, 0.1, 90);
    this.img2 = new MapillaryImage("key2", 0.2, 0.2, 90);
    this.img3 = new MapillaryImage("key3", 0.3, 0.3, 90);
    this.img4 = new MapillaryImage("key4", 0.4, 0.4, 90);
    this.seq = new MapillarySequence();

    this.seq.add(Arrays.asList(new MapillaryAbstractImage[] { this.img1,
        this.img2, this.img3, this.img4 }));
    this.img1.setSequence(this.seq);
    this.img2.setSequence(this.seq);
    this.img3.setSequence(this.seq);
    this.img4.setSequence(this.seq);

    this.data = new MapillaryData();
    this.data.add(this.seq.getImages());
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
    this.data.add(Arrays.asList(new MapillaryAbstractImage[] { this.img2,
        this.img3 }));
    assertEquals(3, this.data.getImages().size());
    this.data.add(Arrays.asList(new MapillaryAbstractImage[] { this.img3,
        this.img4 }));
    assertEquals(4, this.data.getImages().size());
  }

  /**
   * Test that the size is properly calculated.
   */
  @Test
  public void sizeTest() {
    assertEquals(4, this.data.size());
    this.data.add(new MapillaryImage("key5", 0.1, 0.1, 90));
    assertEquals(5, this.data.size());
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
    assertEquals(null, this.data.getHighlightedImage());
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
    assertEquals(null, this.data.getSelectedImage());
  }

  /**
   * Tests the {@link MapillaryData#selectNext()} and
   * {@link MapillaryData#selectPrevious()} methods.
   */
  @Test
  public void NextAndPreviousTest() {
    this.data.setSelectedImage(this.img1);

    this.data.selectNext();
    assertEquals(this.img2, this.data.getSelectedImage());
    this.data.selectNext();
    assertEquals(this.img3, this.data.getSelectedImage());
    this.data.selectPrevious();
    assertEquals(this.img2, this.data.getSelectedImage());

    this.data.setSelectedImage(null);
    // Test IllegalStateException thrown by selectNext() when the selectedImage
    // is null.
    try {
      this.data.selectNext();
      fail();
    } catch (IllegalStateException e) {
    } catch (Exception e) {
      fail();
    }
    // Test IllegalStateException thrown by selectPrevious() when the
    // selectedImage is null.
    try {
      this.data.selectPrevious();
      fail();
    } catch (IllegalStateException e) {
    } catch (Exception e) {
      fail();
    }
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
