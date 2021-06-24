// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import org.openstreetmap.josm.data.preferences.BooleanProperty;
import org.openstreetmap.josm.data.preferences.DoubleProperty;
import org.openstreetmap.josm.data.preferences.IntegerProperty;
import org.openstreetmap.josm.data.preferences.StringProperty;
import org.openstreetmap.josm.plugins.mapillary.gui.imageinfo.ImageInfoPanel;

public final class MapillaryProperties {
  public static final BooleanProperty DELETE_AFTER_UPLOAD = new BooleanProperty("mapillary.delete-after-upload", true);
  public static final BooleanProperty DEVELOPER = new BooleanProperty("mapillary.developer", false);
  public static final BooleanProperty DISPLAY_HOUR = new BooleanProperty("mapillary.display-hour", true);
  public static final BooleanProperty MOVE_TO_IMG = new BooleanProperty("mapillary.move-to-picture", true);
  public static final BooleanProperty IMAGE_LINK_TO_BLUR_EDITOR = new BooleanProperty(
    "mapillary.image-link-to-blur-editor", false);

  public static final IntegerProperty MAPOBJECT_ICON_SIZE = new IntegerProperty("mapillary.mapobjects.iconsize", 32);
  public static final IntegerProperty MAX_MAPOBJECTS = new IntegerProperty("mapillary.mapobjects.maximum-number", 200);
  public static final BooleanProperty SHOW_DETECTED_SIGNS = new BooleanProperty("mapillary.show-detected-signs", true);
  public static final BooleanProperty SHOW_DETECTION_OUTLINES = new BooleanProperty("mapillary.show-detection-outlines",
    true);

  public static final IntegerProperty PICTURE_DRAG_BUTTON = new IntegerProperty("mapillary.picture-drag-button", 3);
  public static final IntegerProperty PICTURE_OPTION_BUTTON = new IntegerProperty("mapillary.picture-option-button", 2);
  public static final IntegerProperty PICTURE_ZOOM_BUTTON = new IntegerProperty("mapillary.picture-zoom-button", 1);
  public static final IntegerProperty SEQUENCE_MAX_JUMP_DISTANCE = new IntegerProperty(
    "mapillary.sequence-max-jump-distance", 100);

  public static final StringProperty ACCESS_TOKEN = new StringProperty("mapillary.access-token", null);
  public static final StringProperty START_DIR = new StringProperty("mapillary.start-directory",
    System.getProperty("user.home"));

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
  public static final IntegerProperty PRE_FETCH_IMAGE_COUNT = new IntegerProperty("mapillary.prefetch-image-count", 4);

  /**
   * Download point features (fire hydrants, trees, and so on)
   */
  public static final BooleanProperty POINT_FEATURES = new BooleanProperty("mapillary.pointfeatures", false);

  /**
   * The opacity for unselected images when an image is selected
   */
  public static final DoubleProperty UNSELECTED_OPACITY = new DoubleProperty("mapillary.unselectedimageopacity", 0.50);

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
  public static final IntegerProperty DESELECT_CLICK_COUNT = new IntegerProperty("mapillary.image.deselect.click.count",
    3);
  /** Use computed locations instead of the original locations */
  public static final BooleanProperty USE_COMPUTED_LOCATIONS = new BooleanProperty("mapillary.computed_locations",
    true);
  /** Use capture date to color image sequences and locations */
  public static final BooleanProperty COLOR_BY_CAPTURE_DATE = new BooleanProperty("mapillary.color_by_capture_date",
    false);

  /** The maximum distance for us to automatically add Mapillary to the changeset source */
  public static final DoubleProperty MAXIMUM_DISTANCE_FOR_CHANGESET_SOURCE = new DoubleProperty(
    "mapillary.source.maxdistance", 30.0);

  /**
   * The maximum number of images to show in the map view
   */
  public static final IntegerProperty MAXIMUM_DRAW_IMAGES = new IntegerProperty("mapillary.images.max_draw", 10_000);

  private MapillaryProperties() {
    // Private constructor to avoid instantiation
  }
}
