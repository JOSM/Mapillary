// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary;

import java.io.File;
import java.io.IOException;

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
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryChangesetDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryFilterDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryHistoryDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryMainDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryPreferenceSetting;
import org.openstreetmap.josm.plugins.mapillary.gui.imageinfo.ImageInfoHelpPopup;
import org.openstreetmap.josm.plugins.mapillary.gui.imageinfo.ImageInfoPanel;
import org.openstreetmap.josm.plugins.mapillary.io.download.MapillaryDownloader;
import org.openstreetmap.josm.plugins.mapillary.io.download.MapillaryDownloader.DOWNLOAD_MODE;
import org.openstreetmap.josm.plugins.mapillary.oauth.MapillaryUser;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.tools.ImageProvider;

/**
 * This is the main class of the Mapillary plugin.
 *
 * @author nokutu
 *
 */
public class MapillaryPlugin extends Plugin {

  public static final ImageProvider LOGO = new ImageProvider("mapillary-logo");

  /** Cache that stores the pictures the downloaded pictures. */
  private static CacheAccess<String, BufferedImageCacheEntry> cache;

  private static final MapillaryDownloadAction DOWNLOAD_ACTION = new MapillaryDownloadAction();
  private static final MapillaryExportAction EXPORT_ACTION = new MapillaryExportAction();
  /** Import action */
  private static final MapillaryImportAction IMPORT_ACTION = new MapillaryImportAction();
  /** Zoom action */
  private static final MapillaryZoomAction ZOOM_ACTION = new MapillaryZoomAction();
  private static final MapillaryDownloadViewAction DOWNLOAD_VIEW_ACTION = new MapillaryDownloadViewAction();
  private static final MapillaryImportIntoSequenceAction IMPORT_INTO_SEQUENCE_ACTION = new MapillaryImportIntoSequenceAction();
  private static final MapillaryJoinAction JOIN_ACTION = new MapillaryJoinAction();
  /** Walk action */
  private static final MapillaryWalkAction WALK_ACTION = new MapillaryWalkAction();
  /** Upload action */
  private static final MapillaryUploadAction UPLOAD_ACTION = new MapillaryUploadAction();

  /** Menu button for the {@link MapillaryDownloadAction} action. */
  private static final JMenuItem DOWNLOAD_MENU;
  /** Menu button for the {@link MapillaryExportAction} action. */
  private static final JMenuItem EXPORT_MENU;
  /** Menu button for the {@link MapillaryImportAction} action. */
  private static final JMenuItem IMPORT_MENU;
  /** Menu button for the {@link MapillaryZoomAction} action. */
  private static final JMenuItem ZOOM_MENU;
  /** Menu button for the {@link MapillaryDownloadViewAction} action. */
  private static final JMenuItem DOWNLOAD_VIEW_MENU;
  /** Menu button for the {@link MapillaryImportIntoSequenceAction} action. */
  private static final JMenuItem IMPORT_INTO_SEQUENCE_MENU;
  /** Menu button for the {@link MapillaryJoinAction} action. */
  private static final JMenuItem JOIN_MENU;
  /** Menu button for the {@link MapillaryWalkAction} action. */
  private static final JMenuItem WALK_MENU;
  /** Menu button for the {@link MapillaryUploadAction} action. */
  private static final JMenuItem UPLOAD_MENU;

  static {
    if (Main.main == null) {
      EXPORT_MENU = null;
      DOWNLOAD_MENU = null;
      IMPORT_MENU = null;
      ZOOM_MENU = null;
      DOWNLOAD_VIEW_MENU = null;
      IMPORT_INTO_SEQUENCE_MENU = null;
      JOIN_MENU = null;
      WALK_MENU = null;
      UPLOAD_MENU = null;
    } else {
      EXPORT_MENU = MainMenu.add(Main.main.menu.fileMenu, EXPORT_ACTION, false, 14);
      EXPORT_MENU.setEnabled(false);
      DOWNLOAD_MENU = MainMenu.add(Main.main.menu.imageryMenu, DOWNLOAD_ACTION, false);
      DOWNLOAD_MENU.setEnabled(false);
      IMPORT_MENU = MainMenu.add(Main.main.menu.fileMenu, IMPORT_ACTION, false, 14);
      IMPORT_MENU.setEnabled(false);
      ZOOM_MENU = MainMenu.add(Main.main.menu.viewMenu, ZOOM_ACTION, false, 15);
      ZOOM_MENU.setEnabled(false);
      DOWNLOAD_VIEW_MENU = MainMenu.add(Main.main.menu.fileMenu, DOWNLOAD_VIEW_ACTION, false, 14);
      DOWNLOAD_VIEW_MENU.setEnabled(false);
      IMPORT_INTO_SEQUENCE_MENU = MainMenu.add(Main.main.menu.fileMenu, IMPORT_INTO_SEQUENCE_ACTION, false, 14);
      IMPORT_INTO_SEQUENCE_MENU.setEnabled(false);
      JOIN_MENU = MainMenu.add(Main.main.menu.dataMenu, JOIN_ACTION, false);
      JOIN_MENU.setEnabled(false);
      WALK_MENU = MainMenu.add(Main.main.menu.moreToolsMenu, WALK_ACTION, false);
      WALK_MENU.setEnabled(false);
      UPLOAD_MENU = MainMenu.add(Main.main.menu.fileMenu, UPLOAD_ACTION, false, 14);
      UPLOAD_MENU.setEnabled(false);
    }
  }

  /**
   * Main constructor.
   *
   * @param info
   *          Required information of the plugin. Obtained from the jar file.
   * @throws IOException if the mapillary cache directory is not found
   */
  public MapillaryPlugin(PluginInformation info) throws IOException {
    super(info);

    if (cache == null) {
      cache = JCSCacheManager.getCache("mapillary", 10, 10000, getCacheDirectory().getPath());
    }

    if (MapillaryProperties.ACCESS_TOKEN.get() == null) {
      MapillaryUser.setTokenValid(false);
    }
  }

  /**
   * @return the menu-item associated with the {@link MapillaryDownloadViewAction}
   */
  public static JMenuItem getDownloadViewMenu() {
    return DOWNLOAD_VIEW_MENU;
  }

  /**
   * @return the menu-item associated with the {@link MapillaryExportAction}
   */
  public static JMenuItem getExportMenu() {
    return EXPORT_MENU;
  }

  /**
   * @return the menu-item associated with the {@link MapillaryJoinAction}
   */
  public static JMenuItem getJoinMenu() {
    return JOIN_MENU;
  }

  /**
   * @return the {@link MapillaryUploadAction} for the plugin
   */
  public static MapillaryDataListener getUploadAction() {
    return UPLOAD_ACTION;
  }

  /**
   * @return the menu-item associated with the {@link MapillaryUploadAction}
   */
  public static JMenuItem getUploadMenu() {
    return UPLOAD_MENU;
  }

  /**
   * @return the {@link MapillaryWalkAction} for the plugin
   */
  public static MapillaryWalkAction getWalkAction() {
    return WALK_ACTION;
  }

  /**
   * @return the menu-item associated with the {@link MapillaryWalkAction}
   */
  public static JMenuItem getWalkMenu() {
    return WALK_MENU;
  }

  /**
   * @return the {@link MapillaryZoomAction} for the plugin
   */
  public static MapillaryDataListener getZoomAction() {
    return ZOOM_ACTION;
  }

  /**
   * @return the menu-item associated with the {@link MapillaryZoomAction}
   */
  public static JMenuItem getZoomMenu() {
    return ZOOM_MENU;
  }

  /**
   * Called when the JOSM map frame is created or destroyed.
   */
  @Override
  public void mapFrameInitialized(MapFrame oldFrame, MapFrame newFrame) {
    if (oldFrame == null && newFrame != null) { // map frame added
      Main.map.addToggleDialog(MapillaryMainDialog.getInstance(), false);
      MapillaryMainDialog.getInstance().setImageInfoHelp(new ImageInfoHelpPopup(
        Main.map.addToggleDialog(ImageInfoPanel.getInstance(), false)
      ));
      Main.map.addToggleDialog(MapillaryHistoryDialog.getInstance(), false);
      Main.map.addToggleDialog(MapillaryChangesetDialog.getInstance(), false);
      Main.map.addToggleDialog(MapillaryFilterDialog.getInstance(), false);
      setMenuEnabled(DOWNLOAD_MENU, true);
      if (MapillaryDownloader.getMode() == DOWNLOAD_MODE.MANUAL_ONLY) {
        setMenuEnabled(DOWNLOAD_VIEW_MENU, true);
      }
      setMenuEnabled(IMPORT_MENU, true);
      setMenuEnabled(IMPORT_INTO_SEQUENCE_MENU, true);
    }
    if (oldFrame != null && newFrame == null) { // map frame destroyed
      MapillaryMainDialog.destroyInstance();
      MapillaryHistoryDialog.destroyInstance();
      MapillaryChangesetDialog.destroyInstance();
      MapillaryFilterDialog.destroyInstance();
      ImageInfoPanel.destroyInstance();
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
    if (menu == null) { // In headless mode the menu items are initialized to null
      return;
    }
    if (!SwingUtilities.isEventDispatchThread()) {
      SwingUtilities.invokeLater(() -> setMenuEnabled(menu, value));
    } else {
      menu.setEnabled(value);
      menu.getAction().setEnabled(value);
    }
  }

  @Override
  public PreferenceSetting getPreferenceSetting() {
    return new MapillaryPreferenceSetting();
  }

  public static CacheAccess<String, BufferedImageCacheEntry> getCache() {
    return cache;
  }

  public static File getCacheDirectory() {
    final File f = new File(Main.pref.getPluginsDirectory().getPath() + "/Mapillary/cache");
    if (!f.exists()) {
      f.mkdirs();
    }
    return f;
  }
}
