// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import java.lang.reflect.Field;
import java.util.Optional;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import org.openstreetmap.josm.tools.Logging;

/**
 * A class for getting objects from JOSM code
 */
public final class ReflectionUtils {
    private ReflectionUtils() {
        // Hide constructor
    }

    /**
     * Get a declared field
     *
     * @param parentClass The class with the declared field
     * @param fieldInParentClass The field to get
     * @param objectWithField The object with the field (may be {@code null})
     * @param returnClass The return type
     * @param <T> The parent class
     * @param <C> The class to search
     * @param <R> The return class
     * @return An optional with the object (may be empty)
     */
    @Nonnull
    public static <T, C extends T, R> Optional<R> getDeclaredField(@Nonnull Class<T> parentClass,
        @Nonnull String fieldInParentClass, @Nullable C objectWithField, @Nonnull Class<R> returnClass) {
        try {
            Field tagMenuField = parentClass.getDeclaredField(fieldInParentClass);
            org.openstreetmap.josm.tools.ReflectionUtils.setObjectsAccessible(tagMenuField);
            final Object object = tagMenuField.get(objectWithField);
            if (returnClass.isInstance(object)) {
                return Optional.of(returnClass.cast(object));
            }
        } catch (ReflectiveOperationException e) {
            Logging.error(e);
        }
        return Optional.empty();
    }
}
