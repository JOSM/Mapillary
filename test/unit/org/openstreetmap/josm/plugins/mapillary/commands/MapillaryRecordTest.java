package org.openstreetmap.josm.plugins.mapillary.commands;

import static org.junit.Assert.assertEquals;

import java.util.Arrays;

import org.junit.Before;
import org.junit.Test;
import org.openstreetmap.josm.plugins.mapillary.AbstractTest;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;

/**
 * Tests the command record system.
 *
 * @author nokutu
 *
 */
public class MapillaryRecordTest extends AbstractTest {

  MapillaryRecord record;
  MapillaryImage img1;
  MapillaryImage img2;
  MapillaryImage img3;

  /**
   * Creates a new {@link MapillaryRecord} object and 3 {@link MapillaryImage}
   * objects.
   */
  @Before
  public void setUp() {
    this.record = new MapillaryRecord();
    this.img1 = new MapillaryImage("key1", 0.1, 0.1, 0.1);
    this.img2 = new MapillaryImage("key2", 0.2, 0.2, 0.2);
    this.img3 = new MapillaryImage("key3", 0.3, 0.3, 0.3);
  }

  /**
   * Test commands in general.
   */
  @Test
  public void commandTest() {
    MapillaryCommand cmd1 = new CommandMoveImage(
        Arrays.asList(new MapillaryAbstractImage[] { this.img1, this.img2 }),
        0.1, 0.1);
    MapillaryCommand cmd2 = new CommandMoveImage(
        Arrays.asList(new MapillaryAbstractImage[] { this.img2, this.img3 }),
        0.1, 0.1);
    MapillaryCommand cmd3 = new CommandMoveImage(
        Arrays.asList(new MapillaryAbstractImage[] { this.img1, this.img3 }),
        0.1, 0.1);
    MapillaryCommand cmd31 = new CommandMoveImage(
        Arrays.asList(new MapillaryAbstractImage[] { this.img3, this.img1 }),
        0.2, 0.2);
    this.record.addCommand(cmd1);
    this.record.addCommand(cmd2);

    assertEquals(1, this.record.position);
    assertEquals(2, this.record.commandList.size());

    this.record.undo();

    assertEquals(0, this.record.position);
    assertEquals(2, this.record.commandList.size());

    this.record.addCommand(cmd3);

    assertEquals(1, this.record.position);
    assertEquals(2, this.record.commandList.size());

    this.record.undo();
    this.record.redo();

    assertEquals(1, this.record.position);
    assertEquals(2, this.record.commandList.size());

    this.record.addCommand(cmd31);

    assertEquals(1, this.record.position);
    assertEquals(2, this.record.commandList.size());
  }

  /**
   * Tests CommandMoveImage class.
   */
  @Test
  public void commandMoveTest() {
    CommandMoveImage cmd1 = new CommandMoveImage(
        Arrays.asList(new MapillaryAbstractImage[] { this.img1, this.img2 }),
        0.1, 0.1);
    CommandMoveImage cmd2 = new CommandMoveImage(
        Arrays.asList(new MapillaryAbstractImage[] { this.img1, this.img2 }),
        0.1, 0.1);

    this.record.addCommand(cmd1);

    assertEquals(0.1, this.img1.getLatLon().lat(), 0.01);

    this.record.undo();

    assertEquals(0.0, this.img1.getLatLon().lat(), 0.01);

    this.record.redo();

    assertEquals(0.1, this.img1.getLatLon().lat(), 0.01);

    this.record.addCommand(cmd2);
    this.record.undo();

    assertEquals(-0.1, this.img1.getLatLon().lat(), 0.01);

    this.record.redo();

    assertEquals(0.1, this.img1.getLatLon().lat(), 0.01);
  }

  /**
   * Tests CommandTurnImage class.
   */
  @Test
  public void commandTurnTest() {
    CommandTurnImage cmd1 = new CommandTurnImage(
        Arrays.asList(new MapillaryAbstractImage[] { this.img1, this.img2 }),
        0.2);
    CommandTurnImage cmd2 = new CommandTurnImage(
        Arrays.asList(new MapillaryAbstractImage[] { this.img1, this.img2 }),
        0.1);

    this.record.addCommand(cmd1);
    this.record.undo();

    assertEquals(-0.1, this.img1.getCa(), 0.01);

    this.record.redo();

    assertEquals(0.1, this.img1.getCa(), 0.01);

    this.record.addCommand(cmd2);
    this.record.undo();

    assertEquals(-0.2, this.img1.getCa(), 0.01);

    this.record.redo();

    assertEquals(0.1, this.img1.getCa(), 0.01);
  }
}
