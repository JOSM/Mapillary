// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.model;

import java.io.Serializable;

/**
 * An object that is identified amongst objects of the same class by a {@link String} key.
 */
public class KeyIndexedObject implements Serializable {
  private static final long serialVersionUID = 7762862623109416405L;
  private final String key;

  protected KeyIndexedObject(final String key) {
    this.key = key;
  }

  public String getKey() {
    return key;
  }
}
