// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import java.awt.Color;
import java.util.Map;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.data.preferences.BooleanProperty;
import org.openstreetmap.josm.data.preferences.ColorProperty;
import org.openstreetmap.josm.data.preferences.DoubleProperty;
import org.openstreetmap.josm.data.preferences.IntegerProperty;
import org.openstreetmap.josm.data.preferences.Setting;
import org.openstreetmap.josm.data.preferences.StringProperty;
import org.openstreetmap.josm.gui.layer.OsmDataLayer;
import org.openstreetmap.josm.plugins.mapillary.gui.imageinfo.ImageInfoPanel;
import org.openstreetmap.josm.plugins.mapillary.io.download.MapillaryDownloader;

public final class MapillaryProperties {
  public static final BooleanProperty DELETE_AFTER_UPLOAD = new BooleanProperty("mapillary.delete-after-upload", true);
  public static final BooleanProperty DEVELOPER = new BooleanProperty("mapillary.developer", false);
  public static final BooleanProperty DISPLAY_HOUR = new BooleanProperty("mapillary.display-hour", true);
  public static final BooleanProperty HOVER_ENABLED = new BooleanProperty("mapillary.hover-enabled", true);
  public static final BooleanProperty MOVE_TO_IMG = new BooleanProperty("mapillary.move-to-picture", true);
  public static final BooleanProperty TIME_FORMAT_24 = new BooleanProperty("mapillary.format-24", true);
  /**
   * If false, all sequences that cross the download bounds are put completely into the MapillaryData object.
   * Otherwise only all images (!) inside the download bounds are added, the others are discarded.
   */
  public static final BooleanProperty CUT_OFF_SEQUENCES_AT_BOUNDS =
    new BooleanProperty("mapillary.cut-off-sequences-at-bounds", false);
  public static final IntegerProperty MAPOBJECT_ICON_SIZE = new IntegerProperty("mapillary.mapobjects.iconsize", 32);
  public static final IntegerProperty MAX_MAPOBJECTS = new IntegerProperty("mapillary.mapobjects.maximum-number", 500);

  /**
   * @see OsmDataLayer#PROPERTY_BACKGROUND_COLOR
   */
  public static final ColorProperty BACKGROUND = new ColorProperty("background", Color.BLACK);
  /**
   * @see OsmDataLayer#PROPERTY_OUTSIDE_COLOR
   */
  public static final ColorProperty OUTSIDE_DOWNLOADED_AREA = new ColorProperty("outside downloaded area", Color.YELLOW);

  public static final DoubleProperty MAX_DOWNLOAD_AREA = new DoubleProperty("mapillary.max-download-area", 0.015);

  public static final IntegerProperty PICTURE_DRAG_BUTTON = new IntegerProperty("mapillary.picture-drag-button", 3);
  public static final IntegerProperty PICTURE_OPTION_BUTTON = new IntegerProperty("mapillary.picture-option-button", 2);
  public static final IntegerProperty PICTURE_ZOOM_BUTTON = new IntegerProperty("mapillary.picture-zoom-button", 1);
  public static final IntegerProperty SEQUENCE_MAX_JUMP_DISTANCE =
    new IntegerProperty("mapillary.sequence-max-jump-distance", 100);

  public static final StringProperty ACCESS_TOKEN = new StringProperty("mapillary.access-token", null);
  public static final StringProperty DOWNLOAD_MODE =
    new StringProperty("mapillary.download-mode", MapillaryDownloader.DOWNLOAD_MODE.getDefault().getPrefId());
  public static final StringProperty START_DIR =
    new StringProperty("mapillary.start-directory", System.getProperty("user.home"));

  /**
   * The number of times the help popup for the {@link ImageInfoPanel} will be displayed.
   * But regardless of this number, the popup will only show up at most once between two (re)starts of JOSM.
   * Or opening the {@link ImageInfoPanel} immediately brings this number down to zero.
   */
  public static final IntegerProperty IMAGEINFO_HELP_COUNTDOWN =
    new IntegerProperty("mapillary.imageInfo.helpDisplayedCountdown", 4);

  static {
    // Convert old settings (e.g. mapillary-filter.svg) for ToggleDialogs with new ones without ".svg" at the end (e.g. mapillary-filter)
    // TODO: Remove this again after a few versions, when most users have updated their versions of the plugin
    for (Map.Entry<String, Setting<?>> e : Main.pref.getAllSettings().entrySet()) {
      if (e.getKey() != null && e.getKey().matches(".*mapillary\\-[a-z]+\\.svg.*")) {
        Main.pref.put(e.getKey().replaceAll("mapillary\\-([a-z]+)\\.svg", "mapillary-$1"), Main.pref.get(e.getKey()));
        Main.pref.removeFromCollection(e.getKey(), Main.pref.get(e.getKey()));
      }
    }
  }

  private MapillaryProperties() {
    // Private constructor to avoid instantiation
  }
}
