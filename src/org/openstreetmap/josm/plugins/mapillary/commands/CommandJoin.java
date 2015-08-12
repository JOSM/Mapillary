package org.openstreetmap.josm.plugins.mapillary.commands;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.util.List;

import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImportedImage;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;

/**
 * Command joined when joining two images into the same sequence.
 *
 * @author nokutu
 *
 */
public class CommandJoin extends MapillaryExecutableCommand {

  /**
   * Main constructor.
   *
   * @param images
   */
  public CommandJoin(List<MapillaryAbstractImage> images) {
    super(images);
  }

  @Override
  public void execute() {
    this.redo();
  }

  @Override
  public void undo() {
    MapillaryUtils.unjoin((MapillaryImportedImage) this.images.get(0),
        (MapillaryImportedImage) this.images.get(1));
  }

  @Override
  public void redo() {
    MapillaryUtils.join((MapillaryImportedImage) this.images.get(0),
        (MapillaryImportedImage) this.images.get(1));
  }

  @Override
  public void sum(MapillaryCommand command) {
    // TODO Auto-generated method stub

  }

  @Override
  public String toString() {
    return tr("2 images joined");
  }
}
