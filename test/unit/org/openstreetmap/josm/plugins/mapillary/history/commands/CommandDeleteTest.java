// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.history.commands;

import static org.junit.Assert.assertEquals;

import java.util.Arrays;
import java.util.concurrent.ConcurrentSkipListSet;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;

import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryData;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImportedImage;
import org.openstreetmap.josm.plugins.mapillary.MapillarySequence;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil;
import org.openstreetmap.josm.testutils.JOSMTestRules;

/**
 * Tests for {@link CommandDelete} class.
 * @author Kishan
 */
public class CommandDeleteTest {

  @Rule
  public JOSMTestRules rules = new TestUtil.MapillaryTestRules().main().projection();

  private MapillaryData data;
  private MapillaryAbstractImage img1;
  private MapillaryAbstractImage img2;
  private MapillaryAbstractImage img3;
  private MapillaryAbstractImage img4;
  private MapillaryAbstractImage img5;
  private CommandDelete delete;
  final MapillarySequence seq = new MapillarySequence();
  private MapillarySequence orignalSeq;

  /**
   * Creates a sample {@link MapillaryData} objects, 5 {@link MapillaryImage}
   * objects and a {@link MapillarySequence} object.
   */
  @Before
  public void setUp() {

    img1 = new MapillaryImage("key1__________________", new LatLon(0.1, 0.1), 90, false, false);
    img2 = new MapillaryImage("key2__________________", new LatLon(0.2, 0.2), 90, false, false);
    img3 = new MapillaryImage("key3__________________", new LatLon(0.3, 0.3), 90, false, false);
    img4 = new MapillaryImportedImage(new LatLon(0.4, 0.4), 90, null, false);
    img5 = new MapillaryImportedImage(new LatLon(0.5, 0.5), 90, null, false);
    img1.setCapturedAt(1);
    img2.setCapturedAt(0);
    img3.setCapturedAt(0);
    img4.setCapturedAt(1);
    seq.add(Arrays.asList(img1, img2, img3));
    orignalSeq = seq; 
    data = MapillaryLayer.getInstance().getData();
    data.addAll(Arrays.asList(img1, img2, img3, img4, img5));
    delete = new CommandDelete(new ConcurrentSkipListSet<>(Arrays.asList(img1,img3,img4,img5)));
    delete.execute();
  }

  /**
   * Test the deletion of images. Index of all deleted Image should be -1.
   */
  @Test
  public void executeTest() {
    assertEquals(4, delete.images.size());
    assertEquals(-1, img1.getSequence().getImages().indexOf(img1));
    assertEquals(0, img2.getSequence().getImages().indexOf(img2));
    assertEquals(-1, img3.getSequence().getImages().indexOf(img3));
    assertEquals(-1, img4.getSequence().getImages().indexOf(img4));
    assertEquals(-1, img5.getSequence().getImages().indexOf(img5));
  }

  /**
   * Test the {@link CommandDelete} command.
   */
  @Test
  public void undoTest() {
    assertEquals(1, data.getImages().size());
    delete.undo();
    assertEquals(5, data.getImages().size());
    for(MapillaryAbstractImage img : seq.getImages()) {
      assertEquals(orignalSeq.getImages().indexOf(img), seq.getImages().indexOf(img));
    }
  }
}
