// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.data.mapillary;

/**
 * Base detection types for Mapillary object detections
 */
public enum DetectionType {
    /** Traffic sign type/class */
    TRAFFIC_SIGN("signs"),
    /** Point object features like fire hydrants */
    POINT("objects"),
    /** Line object features */
    LINE(null),
    /** Segmentation features */
    SEGMENTATION(null);

    private final String imageLocationString;

    /**
     * Create a new detection type
     *
     * @param imageLocationString {@code "mapillary_sprite_source/package_" + imageLocationString}
     */
    DetectionType(String imageLocationString) {
        this.imageLocationString = "mapillary_sprite_source/package_" + imageLocationString;
    }

    /**
     * The directory for the image location
     *
     * @return A path to be used as a directory when getting an image
     */
    public String getImageLocationString() {
        return this.imageLocationString;
    }
}
