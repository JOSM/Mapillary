package org.openstreetmap.josm.plugins.mapillary;

import java.io.IOException;

import javax.swing.ImageIcon;
import javax.swing.JMenuItem;
import javax.swing.SwingUtilities;

import org.apache.commons.jcs.access.CacheAccess;
import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.data.cache.BufferedImageCacheEntry;
import org.openstreetmap.josm.data.cache.JCSCacheManager;
import org.openstreetmap.josm.gui.MainMenu;
import org.openstreetmap.josm.gui.MapFrame;
import org.openstreetmap.josm.gui.preferences.PreferenceSetting;
import org.openstreetmap.josm.plugins.Plugin;
import org.openstreetmap.josm.plugins.PluginInformation;
import org.openstreetmap.josm.plugins.mapillary.actions.MapillaryDownloadAction;
import org.openstreetmap.josm.plugins.mapillary.actions.MapillaryDownloadViewAction;
import org.openstreetmap.josm.plugins.mapillary.actions.MapillaryExportAction;
import org.openstreetmap.josm.plugins.mapillary.actions.MapillaryImportAction;
import org.openstreetmap.josm.plugins.mapillary.actions.MapillaryImportIntoSequenceAction;
import org.openstreetmap.josm.plugins.mapillary.actions.MapillaryJoinAction;
import org.openstreetmap.josm.plugins.mapillary.actions.MapillaryUploadAction;
import org.openstreetmap.josm.plugins.mapillary.actions.MapillaryWalkAction;
import org.openstreetmap.josm.plugins.mapillary.actions.MapillaryZoomAction;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryFilterDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryHistoryDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryMainDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryPreferenceSetting;
import org.openstreetmap.josm.plugins.mapillary.io.download.MapillaryDownloader;
import org.openstreetmap.josm.plugins.mapillary.oauth.MapillaryUser;
import org.openstreetmap.josm.tools.ImageProvider;

/**
 * This is the main class of the Mapillary plugin.
 *
 * @author nokutu
 *
 */
public class MapillaryPlugin extends Plugin {

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
  /** Zoom action */
  public static MapillaryZoomAction zoomAction;
  private final MapillaryDownloadViewAction downloadViewAction;
  private final MapillaryImportIntoSequenceAction importIntoSequenceAction;
  private final MapillaryJoinAction joinAction;
  /** Walk action */
  public static MapillaryWalkAction walkAction;
  /** Upload action */
  public static MapillaryUploadAction uploadAction;

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
  /** Menu button for the {@link MapillaryUploadAction} action. */
  public static JMenuItem UPLOAD_MENU;

  /**
   * Main constructor.
   *
   * @param info
   *          Required information of the plugin. Obtained from the jar file.
   */
  public MapillaryPlugin(PluginInformation info) {
    super(info);

    ICON24 = new ImageProvider("icon24.png").get();
    ICON16 = new ImageProvider("icon16.png").get();
    MAP_ICON = new ImageProvider("mapicon.png").get();
    MAP_ICON_SELECTED = new ImageProvider("mapiconselected.png").get();
    MAP_ICON_IMPORTED = new ImageProvider("mapiconimported.png").get();
    MAP_SIGN = new ImageProvider("sign.png").get();

    this.downloadAction = new MapillaryDownloadAction();
    walkAction = new MapillaryWalkAction();
    this.exportAction = new MapillaryExportAction();
    importAction = new MapillaryImportAction();
    zoomAction = new MapillaryZoomAction();
    this.downloadViewAction = new MapillaryDownloadViewAction();
    this.importIntoSequenceAction = new MapillaryImportIntoSequenceAction();
    this.joinAction = new MapillaryJoinAction();
    uploadAction = new MapillaryUploadAction();

    if (Main.main != null) { // important for headless mode
      DOWNLOAD_MENU = MainMenu.add(Main.main.menu.imageryMenu,
          this.downloadAction, false);
      EXPORT_MENU = MainMenu.add(Main.main.menu.fileMenu, this.exportAction,
          false, 14);
      IMPORT_INTO_SEQUENCE_MENU = MainMenu.add(Main.main.menu.fileMenu,
          this.importIntoSequenceAction, false, 14);
      IMPORT_MENU = MainMenu.add(Main.main.menu.fileMenu, importAction, false,
          14);
      UPLOAD_MENU = MainMenu.add(Main.main.menu.fileMenu, uploadAction, false,
          14);
      ZOOM_MENU = MainMenu.add(Main.main.menu.viewMenu, zoomAction, false, 15);
      DOWNLOAD_VIEW_MENU = MainMenu.add(Main.main.menu.fileMenu,
          this.downloadViewAction, false, 14);
      JOIN_MENU = MainMenu.add(Main.main.menu.dataMenu, this.joinAction, false);
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

    if (Main.pref.get("mapillary.access-token") == null)
      MapillaryUser.isTokenValid = false;
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
   * Enables/disables a {@link JMenuItem}.
   *
   * @param menu
   *          The JMenuItem object that is going to be enabled or disabled.
   * @param value
   *          true to enable the JMenuItem; false to disable it.
   */
  public static void setMenuEnabled(final JMenuItem menu, final boolean value) {
    if (!SwingUtilities.isEventDispatchThread()) {
      SwingUtilities.invokeLater(new Runnable() {
        @Override
        public void run() {
          setMenuEnabled(menu, value);
        }
      });
    } else {
      menu.setEnabled(value);
      menu.getAction().setEnabled(value);
    }
  }

  @Override
  public PreferenceSetting getPreferenceSetting() {
    return new MapillaryPreferenceSetting();
  }

  /**
   * Returns a ImageProvider for the given string or null if in headless mode.
   *
   * @param s
   *          The name of the file where the picture is.
   * @return A ImageProvider object for the given string or null if in headless
   *         mode.
   */
  public static ImageProvider getProvider(String s) {
    if (Main.main == null)
      return null;
    else
      return new ImageProvider(s);
  }
}
