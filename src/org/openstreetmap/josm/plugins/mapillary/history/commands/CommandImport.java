// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.history.commands;

import static org.openstreetmap.josm.tools.I18n.trn;

import java.util.Set;

import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;

/**
 * Imports a set of images stored locally.
 *
 * @author nokutu
 *
 */
public class CommandImport extends MapillaryExecutableCommand {

  /**
   * Main constructor.
   *
   * @param images
   *          The set of images that are going to be added. Might be in the same
   *          sequence or not.
   */
  public CommandImport(Set<MapillaryAbstractImage> images) {
    super(images);
  }

  @Override
  public void execute() {
    MapillaryLayer.getInstance().getData().addAll(this.images);
  }

  @Override
  public void undo() {
    for (MapillaryAbstractImage img : this.images) {
      MapillaryLayer.getInstance().getData().getImages().remove(img);
    }
    MapillaryLayer.invalidateInstance();
  }

  @Override
  public void redo() {
    this.execute();
  }

  @Override
  public void sum(MapillaryCommand command) {
  }

  @Override
  public String toString() {
    return trn("Imported {0} image", "Imported {0} images", this.images.size(),
        this.images.size());
  }
}
