// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.history.commands;

import static org.openstreetmap.josm.tools.I18n.trn;

import java.util.Set;

import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;

/**
 * Command created when an image's direction is changed.
 *
 * @author nokutu
 *
 */
public class CommandTurn extends MapillaryCommand {
  private double ca;

  /**
   * Main constructor.
   *
   * @param images
   *          Set of images that is turned.
   * @param ca
   *          How much the images turn.
   */
  public CommandTurn(Set<MapillaryAbstractImage> images, double ca) {
    super(images);
    this.ca = ca;
  }

  @Override
  public void undo() {
    for (MapillaryAbstractImage image : this.images) {
      image.turn(-this.ca);
      image.stopMoving();
    }
    MapillaryLayer.invalidateInstance();
  }

  @Override
  public void redo() {
    for (MapillaryAbstractImage image : this.images) {
      image.turn(this.ca);
      image.stopMoving();
    }
    MapillaryLayer.invalidateInstance();
  }

  @Override
  public String toString() {
    return trn("Turned {0} image", "Turned {0} images", this.images.size(),
        this.images.size());
  }

  @Override
  public void sum(MapillaryCommand command) {
    if (command instanceof CommandTurn) {
      this.ca += ((CommandTurn) command).ca;
    }
  }
}
