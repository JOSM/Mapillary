package org.openstreetmap.josm.plugins.mapillary.commands;

import static org.openstreetmap.josm.tools.I18n.trn;

import java.util.ArrayList;
import java.util.List;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;

/**
 * Command created when an image's direction is changed.
 *
 * @author nokutu
 *
 */
public class CommandTurnImage extends MapillaryCommand {
  private double ca;

  /**
   * Main constructor.
   *
   * @param images
   *          Set of images that is turned.
   * @param ca
   *          How much the images turn.
   */
  public CommandTurnImage(List<MapillaryAbstractImage> images, double ca) {
    this.images = new ArrayList<>(images);
    this.ca = ca;
  }

  @Override
  public void undo() {
    for (MapillaryAbstractImage image : this.images) {
      image.turn(-this.ca);
      image.stopMoving();
    }
    checkModified();
    Main.map.repaint();
  }

  @Override
  public void redo() {
    for (MapillaryAbstractImage image : this.images) {
      image.turn(this.ca);
      image.stopMoving();
    }
    checkModified();
    Main.map.repaint();
  }

  @Override
  public String toString() {
    return trn("Turned {0} image", "Turned {0} images", this.images.size(),
        this.images.size());
  }

  @Override
  public void sum(MapillaryCommand command) {
    if (command instanceof CommandTurnImage) {
      this.ca += ((CommandTurnImage) command).ca;
    }
  }
}
