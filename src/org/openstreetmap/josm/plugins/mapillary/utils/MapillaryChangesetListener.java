// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryData;

/**
 * Interface for listeners of the class {@link MapillaryData}.
 *
 * @author nokutu
 *
 */
public interface MapillaryChangesetListener {

  /**
   * Fired when the an image is added or removed from the changeset
   *
   */
  void changesetChanged();
}
