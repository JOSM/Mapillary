// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import java.util.Arrays;

import org.openstreetmap.josm.data.preferences.BooleanProperty;
import org.openstreetmap.josm.data.preferences.DoubleProperty;
import org.openstreetmap.josm.data.preferences.EnumProperty;
import org.openstreetmap.josm.data.preferences.IntegerProperty;
import org.openstreetmap.josm.data.preferences.LongProperty;
import org.openstreetmap.josm.data.preferences.StringProperty;
import org.openstreetmap.josm.plugins.mapillary.gui.imageinfo.ImageInfoPanel;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayerDrawTypes;
import org.openstreetmap.josm.spi.preferences.Config;

/**
 * A class for sharing properties between classes
 */
public final class MapillaryProperties {
    public static final BooleanProperty DEVELOPER = new BooleanProperty("mapillary.developer", false);
    public static final BooleanProperty DISPLAY_HOUR = new BooleanProperty("mapillary.display-hour", true);
    public static final BooleanProperty MOVE_TO_IMG = new BooleanProperty("mapillary.move-to-picture", true);
    public static final BooleanProperty IMAGE_LINK_TO_BLUR_EDITOR = new BooleanProperty(
        "mapillary.image-link-to-blur-editor", false);
    public static final BooleanProperty SHOW_DETECTED_SIGNS = new BooleanProperty("mapillary.show-detected-signs",
        true);
    public static final BooleanProperty SHOW_DETECTION_OUTLINES = new BooleanProperty(
        "mapillary.show-detection-outlines", false);

    /** The access token for authorization */
    public static final StringProperty ACCESS_TOKEN = new StringProperty("mapillary.access-token", null);
    /** The time (in seconds since epoch) that the token expires at */
    public static final LongProperty ACCESS_TOKEN_EXPIRES_AT = new LongProperty("mapillary.access-token.expires-at",
        Long.MIN_VALUE);
    /** The time (in seconds since epoch) that we should try to refresh the token after */
    public static final LongProperty ACCESS_TOKEN_REFRESH_IN = new LongProperty("mapillary.access-token.refresh-in",
        Long.MAX_VALUE);

    /** The assumed HDOP for Mapillary images. VDOP is assumed to be 3x this. Meters. */
    public static final IntegerProperty ASSUMED_HDOP = new IntegerProperty("mapillary.assumed_hdop", 6);

    /**
     * The number of times the help popup for the {@link ImageInfoPanel} will be displayed.
     * But regardless of this number, the popup will only show up at most once between two (re)starts of JOSM.
     * Or opening the {@link ImageInfoPanel} immediately brings this number down to zero.
     */
    public static final IntegerProperty IMAGEINFO_HELP_COUNTDOWN = new IntegerProperty(
        "mapillary.imageInfo.helpDisplayedCountdown", 4);

    /**
     * The number of images to be prefetched when a mapillary image is selected
     */
    public static final IntegerProperty PRE_FETCH_IMAGE_COUNT = new IntegerProperty("mapillary.prefetch-image-count",
        4);

    /**
     * The opacity for unselected images when an image is selected
     */
    public static final DoubleProperty UNSELECTED_OPACITY = new DoubleProperty("mapillary.unselectedimageopacity",
        0.50);

    /**
     * The minimum distance per pixel in order to show the smart add panel
     */
    public static final DoubleProperty SMART_ADD_MIN_DIST_PER_PIXEL = new DoubleProperty(
        "mapillary.smart-add.min-dist-per-pixel", 25);
    /**
     * Smart edit preference setting
     */
    public static final BooleanProperty SMART_EDIT = new BooleanProperty("mapillary.smartedit", false);
    /**
     * The number of times to click before deselecting image objects (all the time), and detection objects in smart mode
     */
    public static final IntegerProperty DESELECT_CLICK_COUNT = new IntegerProperty(
        "mapillary.image.deselect.click.count", 3);
    /** Use computed locations instead of the original locations */
    public static final BooleanProperty USE_COMPUTED_LOCATIONS = new BooleanProperty("mapillary.computed_locations",
        true);

    /** The maximum distance for us to automatically add Mapillary to the changeset source */
    public static final DoubleProperty MAXIMUM_DISTANCE_FOR_CHANGESET_SOURCE = new DoubleProperty(
        "mapillary.source.maxdistance", 30.0);

    /**
     * The maximum number of images to show in the map view
     */
    public static final IntegerProperty MAXIMUM_DRAW_IMAGES = new IntegerProperty("mapillary.images.max_draw", 10_000);

    /**
     * The draw type
     */
    public static final EnumProperty<MapillaryLayerDrawTypes> MAPILLARY_LAYER_DRAW_TYPE = new EnumProperty<>(
        "mapillary.layer.draw.type", MapillaryLayerDrawTypes.class, MapillaryLayerDrawTypes.DEFAULT);

    static {
        if (Boolean.TRUE.equals(Config.getPref().getBoolean("mapillary.color_by_capture_date", false))) {
            MAPILLARY_LAYER_DRAW_TYPE.put(MapillaryLayerDrawTypes.DATE);
        }
        for (String conf : Arrays.asList("mapillary.delete-after-upload", "mapillary.mapobjects.iconsize",
            "mapillary.mapobjects.maximum-number", "mapillary.picture-drag-button", "mapillary.picture-option-button",
            "mapillary.picture-zoom-button", "mapillary.sequence-max-jump-distance", "mapillary.start-directory",
            "mapillary.color_by_capture_date")) {
            Config.getPref().put(conf, null);
        }
    }

    private MapillaryProperties() {
        // Private constructor to avoid instantiation
    }
}
