// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.history;

/**
 * Interface for the listener of the {@link MapillaryRecord} class
 *
 * @author nokutu
 * @see MapillaryRecord
 */
@FunctionalInterface
public interface MapillaryRecordListener {

  /**
   * Fired when any command is undone or redone.
   */
  void recordChanged();
}
