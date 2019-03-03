// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.history.commands;

import static org.openstreetmap.josm.tools.I18n.trn;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;

/**
 * Command used to delete a set of images.
 *
 * @author nokutu
 *
 */
public class CommandDelete extends MapillaryExecutableCommand {

  private final Map<MapillaryAbstractImage, Integer> changesHash = new HashMap<>();

  /**
   * Main constructor.
   *
   * @param images
   *          The set of images that are going to be deleted.
   */
  public CommandDelete(final Set<? extends MapillaryAbstractImage> images) {
    super(images);
  }

  @Override
  public void sum(MapillaryCommand command) {
    // TODO: Implement
  }

  @Override
  public void execute() {
    for (MapillaryAbstractImage img : this.images) {
      this.changesHash.put(img, img.getSequence().getImages().indexOf(img));
      MapillaryLayer.getInstance().getData().remove(img);
    }
  }

  @Override
  public String toString() {
    return trn("Deleted {0} image", "Deleted {0} images", this.images.size(),
        this.images.size());
  }

  @Override
  public void undo() {
    for (MapillaryAbstractImage img : images) {
      MapillaryLayer.getInstance().getData().add(img);
      img.getSequence().getImages().add(this.changesHash.get(img), img);
    }
  }

  @Override
  public void redo() {
    MapillaryLayer.getInstance().getData().remove(this.images);
  }
}
