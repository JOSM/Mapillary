package org.openstreetmap.josm.plugins.mapillary.utils;

import java.lang.reflect.Field;
import java.util.Optional;

import org.openstreetmap.josm.gui.layer.geoimage.ImageDisplay;
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
     * Get the JOSM image viewer
     * TODO: Make it accessible upstream
     *
     * @return The ImageDisplay object, if available and present
     */
    public static Optional<ImageDisplay> getImageViewer() {
        try {
            final Field imgDisplayField = ImageViewerDialog.class.getDeclaredField("imgDisplay");
            imgDisplayField.setAccessible(true);
            Object imgDisplay = imgDisplayField.get(ImageViewerDialog.getInstance());
            if (imgDisplay instanceof ImageDisplay) {
                return Optional.of((ImageDisplay) imgDisplay);
            }
        } catch (ReflectiveOperationException reflectiveOperationException) {
            Logging.error(reflectiveOperationException);
        }
        return Optional.empty();
    }
}
