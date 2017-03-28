// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.detection;

import java.awt.Shape;

public class SpecialImageArea {

  private final String imageKey;
  private final String key;
  private final Shape shape;

  protected SpecialImageArea(final Shape shape, final String imageKey, final String key) {
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

  public Shape getShape() {
    return shape;
  }
}
