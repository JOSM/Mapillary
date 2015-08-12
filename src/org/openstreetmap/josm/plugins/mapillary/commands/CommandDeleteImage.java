package org.openstreetmap.josm.plugins.mapillary.commands;

import static org.openstreetmap.josm.tools.I18n.trn;

import java.util.HashMap;
import java.util.List;

import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;

/**
 * @author nokutu
 *
 */
public class CommandDeleteImage extends MapillaryExecutableCommand {

  private HashMap<MapillaryAbstractImage, Integer> changesHash;

  /**
   * @param images
   */
  public CommandDeleteImage(List<MapillaryAbstractImage> images) {
    super(images);
    this.changesHash = new HashMap<>();
  }

  @Override
  public void sum(MapillaryCommand command) {
    // Ignored
  }

  @Override
  public void execute() {
    for (MapillaryAbstractImage img : this.images) {
      this.changesHash.put(img, img.getSequence().getImages().indexOf(img));
      MapillaryLayer.getInstance().getData().delete(img);
    }
  }

  @Override
  public String toString() {
    return trn("Deleted {0} image", "Deleted {0} images", this.images.size(),
        this.images.size());
  }

  @Override
  public void undo() {
    for (int i = this.images.size() - 1; i >= 0; i--) {
      MapillaryAbstractImage img = this.images.get(i);
      MapillaryLayer.getInstance().getData().add(img);
      img.getSequence().getImages().add(this.changesHash.get(img), img);
    }
  }

  @Override
  public void redo() {
    MapillaryLayer.getInstance().getData().delete(this.images);
  }
}
