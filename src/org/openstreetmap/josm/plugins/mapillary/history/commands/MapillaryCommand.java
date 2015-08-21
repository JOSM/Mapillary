package org.openstreetmap.josm.plugins.mapillary.history.commands;

import java.util.ArrayList;
import java.util.List;

import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;

/**
 * Abstract class for any Mapillary command.
 *
 * @author nokutu
 *
 */
public abstract class MapillaryCommand {
  /** Set of {@link MapillaryAbstractImage} objects affected by the command */
  public List<MapillaryAbstractImage> images;

  /**
   * Main constructor.
   *
   * @param images
   *          The images that are affected by the command.
   */
  public MapillaryCommand(List<MapillaryAbstractImage> images) {
    this.images = new ArrayList<MapillaryAbstractImage>(images);
  }

  /**
   * Undoes the action.
   */
  public abstract void undo();

  /**
   * Redoes the action.
   */
  public abstract void redo();

  /**
   * If two equal commands are applied consecutively to the same set of images,
   * they are summed in order to reduce them to just one command.
   *
   * @param command
   *          The command to be summed to last command.
   */
  public abstract void sum(MapillaryCommand command);

  @Override
  public abstract String toString();
}
