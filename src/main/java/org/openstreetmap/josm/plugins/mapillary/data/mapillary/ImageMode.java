// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.data.mapillary;

import jakarta.annotation.Nonnull;

/**
 * Possible image modes
 *
 * @author Taylor Smock
 */
public enum ImageMode {
    /** The default mode */
    NORMAL,
    /** The mode used when walking */
    WALK,
    /** When smart edits are being done */
    SMART_EDIT;

    private static ImageMode currentMode = NORMAL;

    /**
     * Get the current mode
     *
     * @return The current mode
     */
    public static ImageMode getMode() {
        return currentMode;
    }

    /**
     * Set the mode
     *
     * @param mode The new mode
     */
    public static void setMode(@Nonnull ImageMode mode) {
        currentMode = mode;
    }
}
