package org.openstreetmap.josm.plugins.mapillary.commands;

import java.util.ArrayList;

import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;

/**
 * History record system in order to let the user undo and redo commands
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
    commandList = new ArrayList<>();
    position = -1;
    listeners = new ArrayList<>();
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
   */
  public void addListener(MapillaryRecordListener lis) {
    this.listeners.add(lis);
  }

  /**
   * Removes the given listener.
   * 
   * @param lis
   */
  public void removeListener(MapillaryRecordListener lis) {
    this.listeners.remove(lis);
  }

  /**
   * Adds a new command to the list.
   * 
   * @param command
   */
  public void addCommand(MapillaryCommand command) {
    // Checks if it is a continuation of last command
    if (position != -1) {
      boolean equalSets = true;
      for (MapillaryAbstractImage img : commandList.get(position).images)
        if (!command.images.contains(img))
          equalSets = false;
      if (equalSets
          && commandList.get(position).getClass() == command.getClass()) {
        commandList.get(position).sum(command);
        fireRecordChanged();
        return;
      }
    }
    // Adds the command to the last position of the list.
    commandList.add(position + 1, command);
    position++;
    while (commandList.size() > position + 1) {
      commandList.remove(position + 1);
    }
    fireRecordChanged();
  }

  /**
   * Undo latest command.
   */
  public void undo() {
    if (position == -1)
      return;
    commandList.get(position).undo();
    position--;
    fireRecordChanged();
  }

  /**
   * Redo latest undoed action.
   */
  public void redo() {
    if (position + 1 >= commandList.size())
      return;
    position++;
    commandList.get(position).redo();
    fireRecordChanged();
  }

  private void fireRecordChanged() {
    for (MapillaryRecordListener lis : listeners)
      if (lis != null)
        lis.recordChanged();
  }
}
