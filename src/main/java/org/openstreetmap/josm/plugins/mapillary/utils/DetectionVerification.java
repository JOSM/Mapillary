// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import java.util.Locale;

import org.openstreetmap.josm.plugins.mapillary.model.ImageDetection;

/**
 * Vote for/against a detection
 */
public final class DetectionVerification {
    /** The type of vote to make */
    public enum TYPE {
        /** Approve the detection */
        APPROVE,
        /** Reject the detection */
        REJECT;

        @Override
        public String toString() {
            return this.name().toLowerCase(Locale.ENGLISH);
        }
    }

    /**
     * Vote for a detection
     *
     * @param detection The detection to vote for
     * @param type The type of vote to make
     * @return {@code true} if successful
     */
    public static boolean vote(ImageDetection<?> detection, TYPE type) {
        throw new UnsupportedOperationException("Mapillary: v4 API does not currently support voting");
    }

    private DetectionVerification() {
        // Don't instantiate
    }
}
