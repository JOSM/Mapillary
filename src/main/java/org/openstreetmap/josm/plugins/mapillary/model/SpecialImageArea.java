// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.model;

import java.awt.geom.Path2D;

public class SpecialImageArea extends KeyIndexedObject {
  private final String imageKey;
  private final Path2D shape;

  protected SpecialImageArea(final Path2D shape, final String imageKey, final String key) {
    super(key);
    this.shape = shape;
    this.imageKey = imageKey;
  }

  public String getImageKey() {
    return imageKey;
  }

  public Path2D getShape() {
    return shape;
  }
}
