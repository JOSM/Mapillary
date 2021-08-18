// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.model;

import java.io.Serializable;

/**
 * An object that is identified amongst objects of the same class by a {@link String} key.
 */
public class KeyIndexedObject<T> implements Serializable {
    private final T key;

    protected KeyIndexedObject(final T key) {
        if (key == null) {
            throw new IllegalArgumentException();
        }
        this.key = key;
    }

    /**
     * Get the key for the object
     *
     * @return the unique key that identifies this object among other instances of the same class
     */
    public T getKey() {
        return key;
    }

    /*
     * (non-Javadoc)
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        return prime * (prime + getClass().getName().hashCode()) + key.hashCode();
    }

    /*
     * (non-Javadoc)
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (!(obj instanceof KeyIndexedObject)) {
            return false;
        }
        KeyIndexedObject<?> other = (KeyIndexedObject<?>) obj;
        return key.equals(other.key);
    }

}
