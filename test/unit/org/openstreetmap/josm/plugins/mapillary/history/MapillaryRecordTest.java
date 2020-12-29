// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.history;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.util.Arrays;
import java.util.Collections;
import java.util.concurrent.ConcurrentSkipListSet;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.RegisterExtension;

import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.plugins.mapillary.MapillaryData;
import org.openstreetmap.josm.plugins.mapillary.data.image.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.data.image.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.data.image.MapillarySequence;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.history.commands.CommandDelete;
import org.openstreetmap.josm.plugins.mapillary.history.commands.CommandImport;
import org.openstreetmap.josm.plugins.mapillary.history.commands.CommandJoin;
import org.openstreetmap.josm.plugins.mapillary.history.commands.CommandMove;
import org.openstreetmap.josm.plugins.mapillary.history.commands.CommandTurn;
import org.openstreetmap.josm.plugins.mapillary.history.commands.CommandUnjoin;
import org.openstreetmap.josm.plugins.mapillary.history.commands.MapillaryCommand;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryTestRules;
import org.openstreetmap.josm.testutils.JOSMTestRules;

/**
 * Tests the command record system.
 *
 * @author nokutu
 */
class MapillaryRecordTest {

  @RegisterExtension
  static JOSMTestRules rules = new MapillaryTestRules().main().projection();

  private MapillaryRecord record;
  private MapillaryImage img1;
  private MapillaryImage img2;
  private MapillaryImage img3;

  /**
   * Creates a new {@link MapillaryRecord} object and 3 {@link MapillaryImage}
   * objects.
   */
  @BeforeEach
  void setUp() {
    record = new MapillaryRecord();
    img1 = new MapillaryImage("key1__________________", new LatLon(0.1, 0.1), 0.1, false, false);
    MapillarySequence one = new MapillarySequence(null, null, null, img1.getCapturedAt());
    one.add(img1);
    img2 = new MapillaryImage("key2__________________", new LatLon(0.2, 0.2), 0.2, false, false);
    MapillarySequence two = new MapillarySequence(null, null, null, img2.getCapturedAt());
    two.add(img2);
    img3 = new MapillaryImage("key3__________________", new LatLon(0.3, 0.3), 0.3, false, false);
    MapillarySequence three = new MapillarySequence(null, null, null, img3.getCapturedAt());
    three.add(img3);
    if (MapillaryLayer.hasInstance()) {
      MapillaryLayer.getInstance().getData().clear();
    }
  }

  /**
   * Test commands in general.
   */
  @Test
  void commandTest() {
    MapillaryCommand cmd12 = new CommandMove(new ConcurrentSkipListSet<>(Arrays.asList(this.img1, this.img2)), 0.1,
      0.1);
    MapillaryCommand cmd23 = new CommandMove(new ConcurrentSkipListSet<>(Arrays.asList(this.img2, this.img3)), 0.1,
      0.1);
    MapillaryCommand cmd13 = new CommandMove(new ConcurrentSkipListSet<>(Arrays.asList(this.img1, this.img3)), 0.1,
      0.1);
    MapillaryCommand cmd1 = new CommandMove(new ConcurrentSkipListSet<>(Collections.singletonList(this.img1)), 0.1,
      0.1);
    MapillaryCommand cmd31 = new CommandMove(new ConcurrentSkipListSet<>(Arrays.asList(this.img3, this.img1)), 0.2,
      0.2);
    this.record.addCommand(cmd12);
    this.record.addCommand(cmd23);

    assertEquals(1, this.record.position);
    assertEquals(2, this.record.commandList.size());

    this.record.undo();

    assertEquals(0, this.record.position);
    assertEquals(2, this.record.commandList.size());

    this.record.addCommand(cmd1);

    assertEquals(1, this.record.position);

    this.record.addCommand(cmd13);

    assertEquals(2, this.record.position);
    assertEquals(3, this.record.commandList.size());

    this.record.undo();
    this.record.redo();

    assertEquals(2, this.record.position);
    assertEquals(3, this.record.commandList.size());

    this.record.addCommand(cmd31);

    assertEquals(2, this.record.position);
    assertEquals(3, this.record.commandList.size());

    this.record.addCommand(cmd1);

    assertEquals(3, this.record.position);
    assertEquals(4, this.record.commandList.size());
  }

  /**
   * Tests {@link CommandMove} class.
   */
  @Test
  void commandMoveTest() {
    CommandMove cmd1 = new CommandMove(new ConcurrentSkipListSet<>(Arrays.asList(img1, img2)), 0.1, 0.1);
    CommandMove cmd2 = new CommandMove(new ConcurrentSkipListSet<>(Arrays.asList(img1, img2)), 0.1, 0.1);

    this.record.addCommand(cmd1);

    assertEquals(0.1, this.img1.getMovingLatLon().lat(), 0.01);

    this.record.undo();

    assertEquals(0.0, this.img1.getMovingLatLon().lat(), 0.01);

    this.record.redo();

    assertEquals(0.1, this.img1.getMovingLatLon().lat(), 0.01);

    this.record.addCommand(cmd2);
    this.record.undo();

    assertEquals(-0.1, this.img1.getMovingLatLon().lat(), 0.01);

    this.record.redo();

    assertEquals(0.1, this.img1.getMovingLatLon().lat(), 0.01);
  }

  /**
   * Tests {@link CommandTurn} class.
   */
  @Test
  void commandTurnTest() {
    CommandTurn cmd1 = new CommandTurn(new ConcurrentSkipListSet<>(Arrays.asList(this.img1, this.img2)), 0.2);
    CommandTurn cmd2 = new CommandTurn(new ConcurrentSkipListSet<>(Arrays.asList(this.img1, this.img2)), 0.1);

    this.record.addCommand(cmd1);
    this.record.undo();

    assertEquals(-0.1, this.img1.getMovingCa(), 0.01);

    this.record.redo();

    assertEquals(0.1, this.img1.getMovingCa(), 0.01);

    this.record.addCommand(cmd2);
    this.record.undo();

    assertEquals(-0.2, this.img1.getMovingCa(), 0.01);

    this.record.redo();

    assertEquals(0.1, this.img1.getMovingCa(), 0.01);
  }

  /**
   * Tests {@link CommandJoin} class.
   */
  @Test
  void commandJoinClass() {
    CommandJoin cmd1 = new CommandJoin(img1, img2);
    CommandJoin cmd2 = new CommandJoin(img2, img3);

    this.record.addCommand(cmd1);
    assertEquals(2, img1.getSequence().getImages().size());
    assertEquals(img2, img1.next());
    this.record.undo();
    assertEquals(1, img1.getSequence().getImages().size());
    this.record.redo();
    this.record.addCommand(cmd2);
    assertEquals(3, img1.getSequence().getImages().size());
    assertEquals(img3, img1.next().next());
  }

  @Test
  void commandJoinNull1() {
    assertThrows(NullPointerException.class, () -> new CommandJoin(img1, null));
  }

  @Test
  void commandJoinNull2() {
    assertThrows(NullPointerException.class, () -> new CommandJoin(null, img1));
  }

  /**
   * Tests {@link CommandUnjoin} class.
   */
  @Test
  void commandUnjoinClass() {
    CommandJoin join1 = new CommandJoin(this.img1, this.img2);
    CommandJoin join2 = new CommandJoin(this.img2, this.img3);

    CommandUnjoin cmd1 = new CommandUnjoin(Arrays.asList(new MapillaryAbstractImage[] { this.img1, this.img2 }));
    CommandUnjoin cmd2 = new CommandUnjoin(Arrays.asList(new MapillaryAbstractImage[] { this.img2, this.img3 }));

    this.record.addCommand(join1);
    this.record.addCommand(join2);

    this.record.addCommand(cmd1);
    assertEquals(1, this.img1.getSequence().getImages().size());
    this.record.undo();
    assertEquals(3, this.img1.getSequence().getImages().size());
    this.record.redo();
    this.record.addCommand(cmd2);
    assertEquals(1, this.img1.getSequence().getImages().size());
    assertEquals(1, this.img2.getSequence().getImages().size());

    try {
      this.record.addCommand(new CommandUnjoin(Arrays.asList(new MapillaryAbstractImage[] { img1, img2, img3 })));
      fail();
    } catch (IllegalArgumentException e) {
      // Expected output.
    }
  }

  /**
   * Test {@link CommandDelete} class.
   */
  @Test
  void commandDeleteTest() {
    CommandJoin join1 = new CommandJoin(img1, img2);
    CommandJoin join2 = new CommandJoin(img2, img3);

    CommandDelete deleteImg1 = new CommandDelete(new ConcurrentSkipListSet<>(Collections.singletonList(img1)));
    CommandDelete deleteImg2Img3 = new CommandDelete(new ConcurrentSkipListSet<>(Arrays.asList(img2, img3)));

    this.record.addCommand(join1);
    this.record.addCommand(join2);

    MapillaryLayer.getInstance().getData().addAll(new ConcurrentSkipListSet<>(Arrays.asList(img1, img2, img3)));

    this.record.addCommand(deleteImg1);
    assertFalse(MapillaryLayer.getInstance().getData().getImages().contains(this.img1));
    assertNull(this.img2.previous());
    this.record.undo();
    assertTrue(MapillaryLayer.getInstance().getData().getImages().contains(this.img1));
    this.record.redo();
    this.record.addCommand(deleteImg2Img3);
    assertEquals(0, MapillaryLayer.getInstance().getData().getImages().size());
  }

  /**
   * Test {@link CommandImport} class.
   */
  @Test
  void commandImportTest() {
    CommandImport importImg1 = new CommandImport(new ConcurrentSkipListSet<>(Collections.singletonList(this.img1)));
    CommandImport importImg2Img3 = new CommandImport(new ConcurrentSkipListSet<>(Arrays.asList(this.img2, this.img3)));

    MapillaryData data = MapillaryLayer.getInstance().getData();
    this.record.addCommand(importImg1);
    assertEquals(1, data.getImages().size());
    this.record.undo();
    assertEquals(0, data.getImages().size());
    this.record.redo();
    this.record.addCommand(importImg2Img3);
    assertEquals(3, data.getImages().size());
  }
}
