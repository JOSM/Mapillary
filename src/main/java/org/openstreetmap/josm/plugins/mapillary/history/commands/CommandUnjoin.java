// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.history.commands;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.util.List;
import java.util.concurrent.ConcurrentSkipListSet;

import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;

/**
 * Command joined when joining two images into the same sequence.
 *
 * @author nokutu
 *
 */
public class CommandUnjoin extends MapillaryExecutableCommand {

  private final MapillaryAbstractImage a;
  private final MapillaryAbstractImage b;

  /**
   * Main constructor.
   *
   * @param images
   *          The two images that are going to be unjoined. Must be of exactly
   *          size 2.
   * @throws IllegalArgumentException
   *           if the List size is different from 2.
   */
  public CommandUnjoin(List<MapillaryAbstractImage> images) {
    super(new ConcurrentSkipListSet<>(images));
    a = images.get(0);
    b = images.get(1);
    if (images.size() != 2)
      throw new IllegalArgumentException();
  }

  @Override
  public void execute() {
    this.redo();
  }

  @Override
  public void undo() {
    MapillaryUtils.join(a, b);
  }

  @Override
  public void redo() {
    MapillaryUtils.unjoin(a, b);
  }

  @Override
  public void sum(MapillaryCommand command) {
  }

  @Override
  public String toString() {
    return tr("2 images unjoined");
  }
}
