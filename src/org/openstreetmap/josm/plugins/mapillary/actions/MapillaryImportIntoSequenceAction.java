// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.actions;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.event.ActionEvent;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillarySequence;

/**
 * Imports a set of images and puts them in a single {@link MapillarySequence}.
 *
 * @author nokutu
 *
 */
public class MapillaryImportIntoSequenceAction extends MapillaryImportAction {
  private static final long serialVersionUID = 7892328318806455809L;

  public MapillaryImportIntoSequenceAction() {
    super(
      tr("Import pictures into sequence"),
      tr("Import local pictures"),
      "Import Mapillary Sequence",
      tr("Import pictures into Mapillary layer in a sequence"),
      "mapillaryImportSequence"
    );
  }

  @Override
  public void actionPerformed(ActionEvent arg0) {
    List<MapillaryAbstractImage> images = chooseImages();
    joinImages(images);
    recordChanges(images);
  }

  /**
   * Joins all the images into a new {@link MapillarySequence} (in chronological order).
   * @param images the images that should be joined
   */
  public void joinImages(List<MapillaryAbstractImage> images) {
    Collections.sort(images, new MapillaryEpochComparator());
    MapillarySequence seq = new MapillarySequence();
    for (MapillaryAbstractImage img : images) {
      seq.add(img);
      img.setSequence(seq);
    }
  }

  /**
   * Comparator that compares two {@link MapillaryAbstractImage} objects
   * depending on the time they were taken.
   */
  public static class MapillaryEpochComparator implements Comparator<MapillaryAbstractImage> {
    @Override
    public int compare(MapillaryAbstractImage arg0, MapillaryAbstractImage arg1) {
      return arg0.getCapturedAt() == arg1.getCapturedAt()
        ? 0
        : (int) Math.copySign(1, arg0.getCapturedAt() - arg1.getCapturedAt());
    }
  }
}
