package org.openstreetmap.josm.plugins.mapillary.commands;

import java.util.ArrayList;

import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;

/**
 * History record system in order to let the user undo and redo commands.
 *
 * @author nokutu
 *
 */
public class MapillaryRecord {
  /** The unique instance of the class. */
  private static MapillaryRecord INSTANCE;

  private ArrayList<MapillaryRecordListener> listeners;

  /** The set of commands that have taken place or that have been undone. */
  public ArrayList<MapillaryCommand> commandList;
  /** Last written command. */
  public int position;

  /**
   * Main constructor.
   */
  public MapillaryRecord() {
    this.commandList = new ArrayList<>();
    this.position = -1;
    this.listeners = new ArrayList<>();
  }

  /**
   * Returns the unique instance of the class.
   *
   * @return The unique instance of the class.
   */
  public static synchronized MapillaryRecord getInstance() {
    if (MapillaryRecord.INSTANCE == null)
      MapillaryRecord.INSTANCE = new MapillaryRecord();
    return MapillaryRecord.INSTANCE;
  }

  /**
   * Adds a listener.
   *
   * @param lis
   *          The listener to be added.
   */
  public void addListener(MapillaryRecordListener lis) {
    this.listeners.add(lis);
  }

  /**
   * Removes the given listener.
   *
   * @param lis
   *          The listener to be removed.
   */
  public void removeListener(MapillaryRecordListener lis) {
    this.listeners.remove(lis);
  }

  /**
   * Adds a new command to the list.
   *
   * @param command
   *          The command to be added.
   */
  public void addCommand(MapillaryCommand command) {
    // Checks if it is a continuation of last command
    if (this.position != -1) {
      boolean equalSets = true;
      for (MapillaryAbstractImage img : this.commandList.get(this.position).images)
        if (!command.images.contains(img))
          equalSets = false;
      for (MapillaryAbstractImage img : command.images)
        if (!this.commandList.get(this.position).images.contains(img))
          equalSets = false;
      if (equalSets
          && this.commandList.get(this.position).getClass() == command
              .getClass()) {
        this.commandList.get(this.position).sum(command);
        fireRecordChanged();
        return;
      }
    }
    // Adds the command to the last position of the list.
    this.commandList.add(this.position + 1, command);
    this.position++;
    while (this.commandList.size() > this.position + 1) {
      this.commandList.remove(this.position + 1);
    }
    fireRecordChanged();
  }

  /**
   * Undo latest command.
   */
  public void undo() {
    if (this.position == -1)
      throw new IllegalStateException();
    this.commandList.get(this.position).undo();
    this.position--;
    fireRecordChanged();
  }

  /**
   * Redoes latest undone action.
   */
  public void redo() {
    if (this.position + 1 >= this.commandList.size())
      throw new IllegalStateException();
    this.position++;
    this.commandList.get(this.position).redo();
    fireRecordChanged();
  }

  private void fireRecordChanged() {
    for (MapillaryRecordListener lis : this.listeners)
      if (lis != null)
        lis.recordChanged();
  }
}
