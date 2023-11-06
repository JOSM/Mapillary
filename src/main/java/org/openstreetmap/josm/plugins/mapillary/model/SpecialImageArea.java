// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.model;

import java.awt.Shape;
import java.util.Objects;

import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;

/**
 * Special image areas
 *
 * @param <S> The key type
 * @param <T> The shape type
 */
public class SpecialImageArea<S, T extends Shape> implements KeyIndexedObject<S> {
    private final S key;
    private final S imageKey;
    private final T shape;

    protected SpecialImageArea(final T shape, final S imageKey, final S key) {
        this.key = key;
        this.shape = shape;
        this.imageKey = imageKey;
    }

    public S getImageKey() {
        return imageKey;
    }

    public Shape getShape() {
        if (Boolean.TRUE.equals(MapillaryProperties.SMART_EDIT.get())) {
            return shape.getBounds2D();
        }
        return shape;
    }

    @Override
    public S key() {
        return this.key;
    }

    @Override
    public boolean equals(Object object) {
        if (super.equals(object) && this.getClass().equals(object.getClass())) {
            SpecialImageArea<?, ?> other = (SpecialImageArea<?, ?>) object;
            return Objects.equals(this.shape, other.shape) && Objects.equals(this.imageKey, other.imageKey);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), this.imageKey, this.shape);
    }
}
