// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.model;

import java.awt.geom.Path2D;

public class SpecialImageArea {

  private final String imageKey;
  private final String key;
  private final Path2D shape;

  protected SpecialImageArea(final Path2D shape, final String imageKey, final String key) {
    this.shape = shape;
    this.imageKey = imageKey;
    this.key = key;
  }

  public String getImageKey() {
    return imageKey;
  }

  public String getKey() {
    return key;
  }

  public Path2D getShape() {
    return shape;
  }
}
