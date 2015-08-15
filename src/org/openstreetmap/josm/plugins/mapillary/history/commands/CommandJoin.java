package org.openstreetmap.josm.plugins.mapillary.history.commands;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.util.List;

import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
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
   *          The two images that are going to be joined. Must be of exactly
   *          size 2. The first one joins to the second one.
   * @throws IllegalArgumentException
   *           if the List size is different from 2.
   */
  public CommandJoin(List<MapillaryAbstractImage> images) {
    super(images);
    if (images.size() != 2)
      throw new IllegalArgumentException();
  }

  @Override
  public void execute() {
    this.redo();
  }

  @Override
  public void undo() {
    MapillaryUtils.unjoin(this.images.get(0), this.images.get(1));
  }

  @Override
  public void redo() {
    MapillaryUtils.join(this.images.get(0), this.images.get(1));
  }

  @Override
  public void sum(MapillaryCommand command) {
  }

  @Override
  public String toString() {
    return tr("2 images joined");
  }
}
