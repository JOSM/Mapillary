package org.openstreetmap.josm.plugins.mapillary.history.commands;

import java.util.List;

import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;

/**
 * Superclass for those commands that must be executed after creation.
 *
 * @author nokutu
 *
 */
public abstract class MapillaryExecutableCommand extends MapillaryCommand {

  /**
   * Main constructor.
   *
   * @param images
   *          The set of images affected by the command.
   */
  public MapillaryExecutableCommand(List<MapillaryAbstractImage> images) {
    super(images);
  }

  /**
   * Executes the command. It is run when the command is added to the history
   * record.
   */
  public abstract void execute();
}
