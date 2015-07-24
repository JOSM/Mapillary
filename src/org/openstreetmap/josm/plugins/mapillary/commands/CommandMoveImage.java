package org.openstreetmap.josm.plugins.mapillary.commands;

import static org.openstreetmap.josm.tools.I18n.trn;

import java.util.ArrayList;
import java.util.List;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;

/**
 * Command created when an image's position is changed.
 *
 * @author nokutu
 *
 */
public class CommandMoveImage extends MapillaryCommand {
  private double x;
  private double y;

  /**
   * Main constructor.
   * 
   * @param images
   *          Set of images that are going to be moved.
   * @param x
   *          How much the x coordinate increases.
   * @param y
   *          How much the y coordinate increases.
   */
  public CommandMoveImage(List<MapillaryAbstractImage> images, double x,
      double y) {
    this.images = new ArrayList<>(images);
    this.x = x;
    this.y = y;
  }

  @Override
  public void undo() {
    for (MapillaryAbstractImage image : images) {
      image.move(-x, -y);
      image.stopMoving();
    }
    checkModified();
    Main.map.repaint();
  }

  @Override
  public void redo() {
    for (MapillaryAbstractImage image : images) {
      image.move(x, y);
      image.stopMoving();
    }
    checkModified();
    Main.map.repaint();
  }

  @Override
  public String toString() {
    return trn("Moved {0} image", "Moved {0} images", images.size(),
        images.size());
  }

  @Override
  public void sum(MapillaryCommand command) {
    if (command instanceof CommandMoveImage) {
      this.x += ((CommandMoveImage) command).x;
      this.y += ((CommandMoveImage) command).y;
    }
  }
}
