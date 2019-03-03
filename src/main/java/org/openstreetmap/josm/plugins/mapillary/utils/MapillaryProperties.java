// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import java.awt.Color;

import org.openstreetmap.josm.data.preferences.BooleanProperty;
import org.openstreetmap.josm.data.preferences.DoubleProperty;
import org.openstreetmap.josm.data.preferences.IntegerProperty;
import org.openstreetmap.josm.data.preferences.NamedColorProperty;
import org.openstreetmap.josm.data.preferences.StringProperty;
import org.openstreetmap.josm.gui.layer.OsmDataLayer;
import org.openstreetmap.josm.plugins.mapillary.gui.imageinfo.ImageInfoPanel;
import org.openstreetmap.josm.plugins.mapillary.io.download.MapillaryDownloader;

public final class MapillaryProperties {
  public static final BooleanProperty DELETE_AFTER_UPLOAD = new BooleanProperty("mapillary.delete-after-upload", true);
  public static final BooleanProperty DEVELOPER = new BooleanProperty("mapillary.developer", false);
  public static final BooleanProperty DISPLAY_HOUR = new BooleanProperty("mapillary.display-hour", true);
  public static final BooleanProperty HOVER_ENABLED = new BooleanProperty("mapillary.hover-enabled", true);
  public static final BooleanProperty DARK_MODE = new BooleanProperty("mapillary.dark-mode", true);
  public static final BooleanProperty MOVE_TO_IMG = new BooleanProperty("mapillary.move-to-picture", true);
  public static final BooleanProperty TIME_FORMAT_24 = new BooleanProperty("mapillary.format-24", true);
  public static final BooleanProperty IMAGE_LINK_TO_BLUR_EDITOR = new BooleanProperty("mapillary.image-link-to-blur-editor", false);
  /**
   * If false, all sequences that cross the download bounds are put completely into the MapillaryData object.
   * Otherwise only all images (!) inside the download bounds are added, the others are discarded.
   */
  public static final BooleanProperty CUT_OFF_SEQUENCES_AT_BOUNDS =
    new BooleanProperty("mapillary.cut-off-sequences-at-bounds", false);
  public static final IntegerProperty MAPOBJECT_ICON_SIZE = new IntegerProperty("mapillary.mapobjects.iconsize", 32);
  public static final IntegerProperty MAX_MAPOBJECTS = new IntegerProperty("mapillary.mapobjects.maximum-number", 200);
  public static final BooleanProperty SHOW_DETECTED_SIGNS = new BooleanProperty("mapillary.show-detected-signs", true);

  /**
   * @see OsmDataLayer#PROPERTY_BACKGROUND_COLOR
   */
  public static final NamedColorProperty BACKGROUND = new NamedColorProperty("background", Color.BLACK);
  /**
   * @see OsmDataLayer#PROPERTY_OUTSIDE_COLOR
   */
  public static final NamedColorProperty OUTSIDE_DOWNLOADED_AREA = new NamedColorProperty("outside downloaded area", Color.YELLOW);

  public static final DoubleProperty MAX_DOWNLOAD_AREA = new DoubleProperty("mapillary.max-download-area", 0.015);

  public static final IntegerProperty PICTURE_DRAG_BUTTON = new IntegerProperty("mapillary.picture-drag-button", 3);
  public static final IntegerProperty PICTURE_OPTION_BUTTON = new IntegerProperty("mapillary.picture-option-button", 2);
  public static final IntegerProperty PICTURE_ZOOM_BUTTON = new IntegerProperty("mapillary.picture-zoom-button", 1);
  public static final IntegerProperty SEQUENCE_MAX_JUMP_DISTANCE =
    new IntegerProperty("mapillary.sequence-max-jump-distance", 100);

  public static final StringProperty ACCESS_TOKEN = new StringProperty("mapillary.access-token", null);
  public static final StringProperty DOWNLOAD_MODE =
    new StringProperty("mapillary.download-mode", MapillaryDownloader.DOWNLOAD_MODE.DEFAULT.getPrefId());
  public static final StringProperty START_DIR =
    new StringProperty("mapillary.start-directory", System.getProperty("user.home"));

  /**
   * The number of times the help popup for the {@link ImageInfoPanel} will be displayed.
   * But regardless of this number, the popup will only show up at most once between two (re)starts of JOSM.
   * Or opening the {@link ImageInfoPanel} immediately brings this number down to zero.
   */
  public static final IntegerProperty IMAGEINFO_HELP_COUNTDOWN =
    new IntegerProperty("mapillary.imageInfo.helpDisplayedCountdown", 4);

  /**
   * The number of images to be prefetched when a mapillary image is selected
   */
  public static final IntegerProperty PRE_FETCH_IMAGE_COUNT = new IntegerProperty("mapillary.prefetch-image-count", 2);

  private MapillaryProperties() {
    // Private constructor to avoid instantiation
  }
}
