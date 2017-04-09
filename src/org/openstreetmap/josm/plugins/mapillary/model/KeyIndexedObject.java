// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.model;

/**
 * An object that is identified amongst objects of the same class by a {@link String} key.
 */
public class KeyIndexedObject {
  private final String key;

  protected KeyIndexedObject(final String key) {
    this.key = key;
  }

  public String getKey() {
    return key;
  }
}
