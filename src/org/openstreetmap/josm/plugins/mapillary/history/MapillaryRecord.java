// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.history;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.history.commands.MapillaryCommand;
import org.openstreetmap.josm.plugins.mapillary.history.commands.MapillaryExecutableCommand;

/**
 * History record system in order to let the user undo and redo commands.
 *
 * @author nokutu
 *
 */
public class MapillaryRecord {
  /** The unique instance of the class. */
  private static MapillaryRecord instance;

  private final List<MapillaryRecordListener> listeners = new ArrayList<>();

  /** The set of commands that have taken place or that have been undone. */
  public final List<MapillaryCommand> commandList = new ArrayList<>();
  /** Last written command. */
  public int position;

  /**
   * Main constructor.
   */
  public MapillaryRecord() {
    this.position = -1;
  }

  /**
   * Returns the unique instance of the class.
   *
   * @return The unique instance of the class.
   */
  public static synchronized MapillaryRecord getInstance() {
    if (MapillaryRecord.instance == null)
      MapillaryRecord.instance = new MapillaryRecord();
    return MapillaryRecord.instance;
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
  public void addCommand(final MapillaryCommand command) {

    if (command instanceof MapillaryExecutableCommand)
      ((MapillaryExecutableCommand) command).execute();
    // Checks if it is a continuation of last command
    if (this.position != -1) {
      boolean equalSets = true;
      for (MapillaryAbstractImage img : this.commandList.get(this.position).images) {
        equalSets = command.images.contains(img) && equalSets;
      }
      for (MapillaryAbstractImage img : command.images) {
        equalSets = this.commandList.get(this.position).images.contains(img) && equalSets;
      }
      if (equalSets && this.commandList.get(this.position).getClass() == command.getClass()) {
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
    if (this.position <= -1) {
      return;
    }
    this.commandList.get(this.position).undo();
    this.position--;
    fireRecordChanged();
  }

  /**
   * Redoes latest undone action.
   */
  public void redo() {
    if (position + 1 >= commandList.size()) {
      return;
    }
    this.position++;
    this.commandList.get(this.position).redo();
    fireRecordChanged();
  }

  private void fireRecordChanged() {
    this.listeners.stream().filter(Objects::nonNull).forEach(MapillaryRecordListener::recordChanged);
  }

  /**
   * Resets the object to its start state.
   */
  public void reset() {
    this.commandList.clear();
    this.position = -1;
  }
}
