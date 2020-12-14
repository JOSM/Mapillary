// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.model;

import java.awt.Shape;
import java.awt.geom.Path2D;

import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;

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

  public Shape getShape() {
    if (Boolean.TRUE.equals(MapillaryProperties.SMART_EDIT.get())) {
      return shape.getBounds2D();
    }
    return shape;
  }
}
