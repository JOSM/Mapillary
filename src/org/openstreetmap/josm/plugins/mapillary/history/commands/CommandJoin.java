// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.history.commands;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.util.Arrays;
import java.util.concurrent.ConcurrentSkipListSet;

import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;

/**
 * Command joined when joining two images into the same sequence.
 *
 * @author nokutu
 *
 */
public class CommandJoin extends MapillaryExecutableCommand {

  private final MapillaryAbstractImage a;
  private final MapillaryAbstractImage b;

  /**
   * Main constructor.
   *
   * @param images
   *          The two images that are going to be joined. Must be of exactly
   *          size 2. The first one joins to the second one.
   * @throws IllegalArgumentException
   *           if the List size is different from 2.
   */
  public CommandJoin(final MapillaryAbstractImage a, final MapillaryAbstractImage b) {
    super(new ConcurrentSkipListSet<>(Arrays.asList(new MapillaryAbstractImage[]{a, b}))); // throws NPE if a or b is null
    if (a.getSequence() == b.getSequence()) {
      throw new IllegalArgumentException("Both images must be in different sequences for joining.");
    }
    this.a = a;
    this.b = b;
  }

  @Override
  public void execute() {
    redo();
  }

  @Override
  public void undo() {
    MapillaryUtils.unjoin(a, b);
  }

  @Override
  public void redo() {
    MapillaryUtils.join(a, b);
  }

  @Override
  public void sum(MapillaryCommand command) {
  }

  @Override
  public String toString() {
    return tr("2 images joined");
  }
}
