package org.openstreetmap.josm.plugins.mapillary;

import java.io.File;
import java.io.IOException;

import javax.swing.ImageIcon;
import javax.swing.JMenuItem;

import org.apache.commons.jcs.access.CacheAccess;
import org.openstreetmap.josm.data.cache.BufferedImageCacheEntry;
import org.openstreetmap.josm.data.cache.JCSCacheManager;
import org.openstreetmap.josm.gui.MainMenu;
import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.gui.MapFrame;
import org.openstreetmap.josm.gui.preferences.PreferenceSetting;
import org.openstreetmap.josm.plugins.Plugin;
import org.openstreetmap.josm.plugins.PluginInformation;
import org.openstreetmap.josm.plugins.mapillary.downloads.MapillaryDownloader;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryFilterDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryHistoryDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryPreferenceSetting;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryMainDialog;
import org.openstreetmap.josm.plugins.mapillary.actions.*;
import org.openstreetmap.josm.tools.ImageProvider;

/**
 * This is the main class of the Mapillary plugin.
 *
 * @author nokutu
 *
 */
public class MapillaryPlugin extends Plugin {

  /** Plugin's directory */
  public static String directory;
  /** OS route separator */
  public static final String SEPARATOR = System.getProperty("file.separator");
  /** 24x24 icon. */
  public static ImageIcon ICON24;
  /** 16x16 icon. */
  public static ImageIcon ICON16;
  /** Icon representing an image in the map. */
  public static ImageIcon MAP_ICON;
  /** Icon representing a selected image in the map. */
  public static ImageIcon MAP_ICON_SELECTED;
  /** Icon representing an imported image in the map. */
  public static ImageIcon MAP_ICON_IMPORTED;
  /** Icon used to identify which images have signs on them */
  public static ImageIcon MAP_SIGN;

  /** Cache that stores the pictures the downloaded pictures. */
  public static CacheAccess<String, BufferedImageCacheEntry> CACHE;

  private final MapillaryDownloadAction downloadAction;
  private final MapillaryExportAction exportAction;
  /** Import action */
  public static MapillaryImportAction importAction;
  private final MapillaryZoomAction zoomAction;
  private final MapillaryDownloadViewAction downloadViewAction;
  private final MapillaryImportIntoSequenceAction importIntoSequenceAction;
  private final MapillaryJoinAction joinAction;
  /** Walk action */
  public static MapillaryWalkAction walkAction;

  /** Menu button for the {@link MapillaryDownloadAction} action. */
  public static JMenuItem DOWNLOAD_MENU;
  /** Menu button for the {@link MapillaryExportAction} action. */
  public static JMenuItem EXPORT_MENU;
  /** Menu button for the {@link MapillaryImportAction} action. */
  public static JMenuItem IMPORT_MENU;
  /** Menu button for the {@link MapillaryZoomAction} action. */
  public static JMenuItem ZOOM_MENU;
  /** Menu button for the {@link MapillaryDownloadViewAction} action. */
  public static JMenuItem DOWNLOAD_VIEW_MENU;
  /** Menu button for the {@link MapillaryImportIntoSequenceAction} action. */
  public static JMenuItem IMPORT_INTO_SEQUENCE_MENU;
  /** Menu button for the {@link MapillaryJoinAction} action. */
  public static JMenuItem JOIN_MENU;
  /** Menu button for the {@link MapillaryWalkAction} action. */
  public static JMenuItem WALK_MENU;

  /**
   * Main constructor.
   *
   * @param info
   */
  public MapillaryPlugin(PluginInformation info) {
    super(info);


    directory = new File("").getAbsolutePath() + SEPARATOR;
    ICON24 = new ImageProvider(directory + "images" + SEPARATOR + "icon24.png").get();
    ICON16 = new ImageProvider(directory + "images" + SEPARATOR + "icon16.png").get();
    MAP_ICON = new ImageProvider(directory + "images" + SEPARATOR + "mapicon.png").get();
    MAP_ICON_SELECTED = new ImageProvider(directory
        + "images" + SEPARATOR + "mapiconselected.png").get();
    MAP_ICON_IMPORTED = new ImageProvider(directory
        + "images/mapiconimported.png").get();
    MAP_SIGN = new ImageProvider(directory + "images" + SEPARATOR + "sign.png").get();

    downloadAction = new MapillaryDownloadAction();
    walkAction = new MapillaryWalkAction();
    exportAction = new MapillaryExportAction();
    importAction = new MapillaryImportAction();
    zoomAction = new MapillaryZoomAction();
    downloadViewAction = new MapillaryDownloadViewAction();
    importIntoSequenceAction = new MapillaryImportIntoSequenceAction();
    joinAction = new MapillaryJoinAction();

    if (Main.main != null) { // important for headless mode
      DOWNLOAD_MENU = MainMenu.add(Main.main.menu.imageryMenu, downloadAction,
          false);
      EXPORT_MENU = MainMenu.add(Main.main.menu.fileMenu, exportAction, false,
          14);
      IMPORT_INTO_SEQUENCE_MENU = MainMenu.add(Main.main.menu.fileMenu,
          importIntoSequenceAction, false, 14);
      IMPORT_MENU = MainMenu.add(Main.main.menu.fileMenu, importAction, false,
          14);
      ZOOM_MENU = MainMenu.add(Main.main.menu.viewMenu, zoomAction, false, 15);
      DOWNLOAD_VIEW_MENU = MainMenu.add(Main.main.menu.fileMenu,
          downloadViewAction, false, 14);
      JOIN_MENU = MainMenu.add(Main.main.menu.dataMenu, joinAction, false);
      WALK_MENU = MainMenu.add(Main.main.menu.moreToolsMenu, walkAction, false);

      EXPORT_MENU.setEnabled(false);
      DOWNLOAD_MENU.setEnabled(false);
      IMPORT_MENU.setEnabled(false);
      IMPORT_INTO_SEQUENCE_MENU.setEnabled(false);
      ZOOM_MENU.setEnabled(false);
      DOWNLOAD_VIEW_MENU.setEnabled(false);
      JOIN_MENU.setEnabled(false);
      WALK_MENU.setEnabled(false);
    }

    try {
      CACHE = JCSCacheManager.getCache("mapillary", 10, 10000,
          this.getPluginDir() + "/cache/");
    } catch (IOException e) {
      Main.error(e);
    }
  }

  /**
   * Called when the JOSM map frame is created or destroyed.
   */
  @Override
  public void mapFrameInitialized(MapFrame oldFrame, MapFrame newFrame) {
    if (oldFrame == null && newFrame != null) { // map frame added
      Main.map.addToggleDialog(MapillaryMainDialog.getInstance(), false);
      Main.map.addToggleDialog(MapillaryHistoryDialog.getInstance(), false);
      Main.map.addToggleDialog(MapillaryFilterDialog.getInstance(), false);
      setMenuEnabled(DOWNLOAD_MENU, true);
      if (MapillaryDownloader.getMode() == MapillaryDownloader.MANUAL)
        setMenuEnabled(DOWNLOAD_VIEW_MENU, true);
      setMenuEnabled(IMPORT_MENU, true);
      setMenuEnabled(IMPORT_INTO_SEQUENCE_MENU, true);
    }
    if (oldFrame != null && newFrame == null) { // map frame destroyed
      MapillaryMainDialog.destroyInstance();
      MapillaryHistoryDialog.destroyInstance();
      MapillaryFilterDialog.destroyInstance();
      setMenuEnabled(DOWNLOAD_MENU, false);
      setMenuEnabled(DOWNLOAD_VIEW_MENU, false);
      setMenuEnabled(IMPORT_MENU, false);
      setMenuEnabled(IMPORT_INTO_SEQUENCE_MENU, false);
    }
  }

  /**
   * Enables/disables a JMenuItem.
   *
   * @param menu
   *          The JMenuItem object that is going to be enabled or disabled.
   * @param value
   *          true to enable the JMenuItem; false to disable it.
   */
  public static void setMenuEnabled(JMenuItem menu, boolean value) {
    menu.setEnabled(value);
    menu.getAction().setEnabled(value);
  }

  @Override
  public PreferenceSetting getPreferenceSetting() {
    return new MapillaryPreferenceSetting();
  }
}
