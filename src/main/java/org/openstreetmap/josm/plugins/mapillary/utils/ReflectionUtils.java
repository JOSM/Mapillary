package org.openstreetmap.josm.plugins.mapillary.utils;

import java.lang.reflect.Field;
import java.security.AccessController;
import java.security.PrivilegedActionException;
import java.security.PrivilegedExceptionAction;
import java.util.Optional;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import org.openstreetmap.josm.gui.layer.geoimage.ImageViewerDialog;
import org.openstreetmap.josm.tools.Logging;

/**
 * A class for getting objects from JOSM code
 */
public final class ReflectionUtils {
    private ReflectionUtils() {
        // Hide constructor
    }

    /**
     * Check if the image viewer dialog has been instantiated
     *
     * @return {@code true} if the image viewer dialog is present
     */
    public static boolean hasImageViewerDialog() {
        try {
            final Field f = ImageViewerDialog.class.getDeclaredField("dialog");
            org.openstreetmap.josm.tools.ReflectionUtils.setObjectsAccessible(f);
            return f.get(null) != null;
        } catch (ReflectiveOperationException e) {
            Logging.error(e);
        }
        return false;
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
            AccessController.doPrivileged((PrivilegedExceptionAction<Void>) () -> {
                tagMenuField.setAccessible(true);
                return null;
            });
            final Object object = tagMenuField.get(objectWithField);
            if (returnClass.isInstance(object)) {
                return Optional.of(returnClass.cast(object));
            }
        } catch (PrivilegedActionException | ReflectiveOperationException e) {
            Logging.error(e);
        }
        return Optional.empty();
    }
}
