// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.model;

import java.awt.Shape;
import java.io.Serializable;
import java.util.Objects;

import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;

public class SpecialImageArea<T extends Shape & Serializable> extends KeyIndexedObject {
  private final String imageKey;
  private final T shape;

  protected SpecialImageArea(final T shape, final String imageKey, final String key) {
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

  @Override
  public boolean equals(Object object) {
    if (super.equals(object) && object instanceof SpecialImageArea) {
      SpecialImageArea<?> other = (SpecialImageArea<?>) object;
      return Objects.equals(this.shape, other.shape) && Objects.equals(this.imageKey, other.imageKey);
    }
    return false;
  }

  @Override
  public int hashCode() {
    return Objects.hash(super.hashCode(), this.imageKey, this.shape);
  }
}
