package org.openstreetmap.josm.plugins.mapillary.history.commands;

import static org.openstreetmap.josm.tools.I18n.trn;

import java.util.List;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryData;
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
   */
  public CommandImport(List<MapillaryAbstractImage> images) {
    super(images);
  }

  @Override
  public void execute() {
    this.redo();
  }

  @Override
  public void undo() {
    for (MapillaryAbstractImage img : this.images)
      MapillaryLayer.getInstance().getData().getImages().remove(img);
    if (Main.main != null)
      MapillaryData.dataUpdated();
  }

  @Override
  public void redo() {
    MapillaryLayer.getInstance().getData().add(this.images);
  }

  @Override
  public void sum(MapillaryCommand command) {
    // IGNORE
  }

  @Override
  public String toString() {
    return trn("Imported {0} image", "Imported {0} images", this.images.size(),
        this.images.size());
  }
}
