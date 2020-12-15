// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary;

/**
 * An interface for keyed objects
 */
public interface Keyed {
  /**
   * Returns the unique identifier of the object.
   *
   * @return A {@code String} containing the unique identifier of the object.
   */
  String getKey();
}
