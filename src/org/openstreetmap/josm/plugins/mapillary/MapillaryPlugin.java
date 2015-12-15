// License: GPL. For details, see LICENSE file.
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
  public static final ImageIcon ICON24 = new ImageProvider("icon24.png").get();
  /** 16x16 icon. */
  public static final ImageIcon ICON16 = new ImageProvider("icon16.png").get();
  /** Icon representing an image in the map. */
  public static final ImageIcon MAP_ICON = new ImageProvider("mapicon.png").get();
  /** Icon representing a selected image in the map. */
  public static final ImageIcon MAP_ICON_SELECTED = new ImageProvider("mapiconselected.png").get();
  /** Icon representing an imported image in the map. */
  public static final ImageIcon MAP_ICON_IMPORTED = new ImageProvider("mapiconimported.png").get();
  /** Icon used to identify which images have signs on them */
  public static final ImageIcon MAP_SIGN = new ImageProvider("sign.png").get();

  /** Cache that stores the pictures the downloaded pictures. */
  public static CacheAccess<String, BufferedImageCacheEntry> cache;

  private static final MapillaryDownloadAction downloadAction = new MapillaryDownloadAction();
  private static final MapillaryExportAction exportAction = new MapillaryExportAction();
  /** Import action */
  private static final MapillaryImportAction importAction = new MapillaryImportAction();
  /** Zoom action */
  private static final MapillaryZoomAction zoomAction = new MapillaryZoomAction();
  private static final MapillaryDownloadViewAction downloadViewAction = new MapillaryDownloadViewAction();
  private static final MapillaryImportIntoSequenceAction importIntoSequenceAction = new MapillaryImportIntoSequenceAction();
  private static final MapillaryJoinAction joinAction = new MapillaryJoinAction();
  /** Walk action */
  private static final MapillaryWalkAction walkAction = new MapillaryWalkAction();
  /** Upload action */
  private static final MapillaryUploadAction uploadAction = new MapillaryUploadAction();

  /** Menu button for the {@link MapillaryDownloadAction} action. */
  private JMenuItem downloadMenu = Main.main == null ? null : MainMenu.add(Main.main.menu.imageryMenu, downloadAction, false);
  /** Menu button for the {@link MapillaryExportAction} action. */
  private static JMenuItem exportMenu = Main.main == null ? null : MainMenu.add(Main.main.menu.fileMenu, exportAction, false, 14);
  /** Menu button for the {@link MapillaryImportAction} action. */
  private JMenuItem importMenu = Main.main == null ? null : MainMenu.add(Main.main.menu.fileMenu, importAction, false, 14);
  /** Menu button for the {@link MapillaryZoomAction} action. */
  private static JMenuItem zoomMenu = Main.main == null ? null : MainMenu.add(Main.main.menu.viewMenu, zoomAction, false, 15);
  /** Menu button for the {@link MapillaryDownloadViewAction} action. */
  private static JMenuItem downloadViewMenu = Main.main == null ? null : MainMenu.add(Main.main.menu.fileMenu, downloadViewAction, false, 14);
  /** Menu button for the {@link MapillaryImportIntoSequenceAction} action. */
  private JMenuItem importIntoSequenceMenu = Main.main == null ? null : MainMenu.add(Main.main.menu.fileMenu, importIntoSequenceAction, false, 14);
  /** Menu button for the {@link MapillaryJoinAction} action. */
  private static JMenuItem joinMenu = Main.main == null ? null : MainMenu.add(Main.main.menu.dataMenu, joinAction, false);
  /** Menu button for the {@link MapillaryWalkAction} action. */
  private static JMenuItem walkMenu = Main.main == null ? null : MainMenu.add(Main.main.menu.moreToolsMenu, walkAction, false);
  /** Menu button for the {@link MapillaryUploadAction} action. */
  private static JMenuItem uploadMenu = Main.main == null ? null : MainMenu.add(Main.main.menu.fileMenu, uploadAction, false, 14);

  /**
   * Main constructor.
   *
   * @param info
   *          Required information of the plugin. Obtained from the jar file.
   */
  public MapillaryPlugin(PluginInformation info) {
    super(info);

    if (Main.main != null) { // important for headless mode
      exportMenu.setEnabled(false);
      downloadMenu.setEnabled(false);
      importMenu.setEnabled(false);
      importIntoSequenceMenu.setEnabled(false);
      zoomMenu.setEnabled(false);
      downloadViewMenu.setEnabled(false);
      joinMenu.setEnabled(false);
      walkMenu.setEnabled(false);
    }

    try {
      cache = JCSCacheManager.getCache("mapillary", 10, 10000, this.getPluginDir() + "/cache/");
    } catch (IOException e) {
      Main.error(e);
    }

    if (Main.pref.get("mapillary.access-token") == null)
      MapillaryUser.setTokenValid(false);
  }

  /**
   * @return the menu-item associated with the {@link MapillaryDownloadViewAction}
   */
  public static JMenuItem getDownloadViewMenu() {
    return downloadViewMenu;
  }

  /**
   * @return the menu-item associated with the {@link MapillaryExportAction}
   */
  public static JMenuItem getExportMenu() {
    return exportMenu;
  }

  /**
   * @return the menu-item associated with the {@link MapillaryJoinAction}
   */
  public static JMenuItem getJoinMenu() {
    return joinMenu;
  }

  /**
   * @return the {@link MapillaryUploadAction} for the plugin
   */
  public static MapillaryDataListener getUploadAction() {
    return uploadAction;
  }

  /**
   * @return the menu-item associated with the {@link MapillaryUploadAction}
   */
  public static JMenuItem getUploadMenu() {
    return uploadMenu;
  }

  /**
   * @return the {@link MapillaryWalkAction} for the plugin
   */
  public static MapillaryWalkAction getWalkAction() {
    return walkAction;
  }

  /**
   * @return the menu-item associated with the {@link MapillaryWalkAction}
   */
  public static JMenuItem getWalkMenu() {
    return walkMenu;
  }

  /**
   * @return the {@link MapillaryZoomAction} for the plugin
   */
  public static MapillaryDataListener getZoomAction() {
    return zoomAction;
  }

  /**
   * @return the menu-item associated with the {@link MapillaryZoomAction}
   */
  public static JMenuItem getZoomMenu() {
    return zoomMenu;
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
      setMenuEnabled(downloadMenu, true);
      if (MapillaryDownloader.getMode() == MapillaryDownloader.MODES.Manual)
        setMenuEnabled(downloadViewMenu, true);
      setMenuEnabled(importMenu, true);
      setMenuEnabled(importIntoSequenceMenu, true);
    }
    if (oldFrame != null && newFrame == null) { // map frame destroyed
      MapillaryMainDialog.destroyInstance();
      MapillaryHistoryDialog.destroyInstance();
      MapillaryFilterDialog.destroyInstance();
      setMenuEnabled(downloadMenu, false);
      setMenuEnabled(downloadViewMenu, false);
      setMenuEnabled(importMenu, false);
      setMenuEnabled(importIntoSequenceMenu, false);
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
