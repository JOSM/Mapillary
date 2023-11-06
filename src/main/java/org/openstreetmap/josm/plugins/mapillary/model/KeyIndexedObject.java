// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.model;

import java.io.Serializable;

/**
 * An object that is identified amongst objects of the same class by a {@link String} key.
 *
 * @param <T> The key type
 */
public interface KeyIndexedObject<T> extends Serializable {
    /**
     * Get the key for the object
     *
     * @return the unique key that identifies this object among other instances of the same class
     */
    T key();
}
