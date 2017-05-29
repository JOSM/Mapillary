// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary;

import java.io.IOException;

import javax.swing.JMenuItem;
import javax.swing.SwingUtilities;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.gui.MainMenu;
import org.openstreetmap.josm.gui.MapFrame;
import org.openstreetmap.josm.gui.MapView;
import org.openstreetmap.josm.gui.preferences.PreferenceSetting;
import org.openstreetmap.josm.plugins.Plugin;
import org.openstreetmap.josm.plugins.PluginInformation;
import org.openstreetmap.josm.plugins.mapillary.actions.MapObjectLayerAction;
import org.openstreetmap.josm.plugins.mapillary.actions.MapillaryDownloadAction;
import org.openstreetmap.josm.plugins.mapillary.actions.MapillaryDownloadViewAction;
import org.openstreetmap.josm.plugins.mapillary.actions.MapillaryExportAction;
import org.openstreetmap.josm.plugins.mapillary.actions.MapillaryImportAction;
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
import org.openstreetmap.josm.tools.Shortcut;

/**
 * This is the main class of the Mapillary plugin.
 *
 * @author nokutu
 *
 */
public class MapillaryPlugin extends Plugin {

  public static final ImageProvider LOGO = new ImageProvider("mapillary-logo");

  private static final MapillaryDownloadAction DOWNLOAD_ACTION = new MapillaryDownloadAction();
  private static final MapillaryExportAction EXPORT_ACTION = new MapillaryExportAction();
  /** Zoom action */
  private static final MapillaryZoomAction ZOOM_ACTION = new MapillaryZoomAction();
  private static final MapillaryDownloadViewAction DOWNLOAD_VIEW_ACTION = new MapillaryDownloadViewAction();
  private static final MapillaryJoinAction JOIN_ACTION = new MapillaryJoinAction();
  /** Walk action */
  private static final MapillaryWalkAction WALK_ACTION = new MapillaryWalkAction();
  /** Upload action */
  private static final MapillaryUploadAction UPLOAD_ACTION = new MapillaryUploadAction();

  /** Menu button for the {@link MapillaryExportAction} action. */
  private static final JMenuItem EXPORT_MENU;
  /** Menu button for the {@link MapillaryZoomAction} action. */
  private static final JMenuItem ZOOM_MENU;
  /** Menu button for the {@link MapillaryDownloadViewAction} action. */
  private static final JMenuItem DOWNLOAD_VIEW_MENU;
  /** Menu button for the {@link MapillaryJoinAction} action. */
  private static final JMenuItem JOIN_MENU;
  /** Menu button for the {@link MapillaryWalkAction} action. */
  private static final JMenuItem WALK_MENU;
  /** Menu button for the {@link MapillaryUploadAction} action. */
  private static final JMenuItem UPLOAD_MENU;

  static {
    if (Main.main == null) {
      EXPORT_MENU = null;
      ZOOM_MENU = null;
      DOWNLOAD_VIEW_MENU = null;
      JOIN_MENU = null;
      WALK_MENU = null;
      UPLOAD_MENU = null;
    } else {
      EXPORT_MENU = MainMenu.add(Main.main.menu.fileMenu, EXPORT_ACTION, false, 14);
      EXPORT_MENU.setEnabled(false);
      MainMenu.add(Main.main.menu.imagerySubMenu, DOWNLOAD_ACTION, false);
      MainMenu.add(Main.main.menu.fileMenu, new MapillaryImportAction(), false, 14);
      ZOOM_MENU = MainMenu.add(Main.main.menu.viewMenu, ZOOM_ACTION, false, 15);
      ZOOM_MENU.setEnabled(false);
      DOWNLOAD_VIEW_MENU = MainMenu.add(Main.main.menu.fileMenu, DOWNLOAD_VIEW_ACTION, false, 14);
      DOWNLOAD_VIEW_MENU.setEnabled(false);
      JOIN_MENU = MainMenu.add(Main.main.menu.dataMenu, JOIN_ACTION, false);
      JOIN_MENU.setEnabled(false);
      WALK_MENU = MainMenu.add(Main.main.menu.moreToolsMenu, WALK_ACTION, false);
      WALK_MENU.setEnabled(false);
      UPLOAD_MENU = MainMenu.add(Main.main.menu.fileMenu, UPLOAD_ACTION, false, 14);
      UPLOAD_MENU.setEnabled(false);

      JMenuItem mapObjImageryLayerItem = MainMenu.add(Main.main.menu.imagerySubMenu, new MapObjectLayerAction(), false);
      mapObjImageryLayerItem.setEnabled(true);
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
   * @return the shortcut for {@link MapillaryDownloadAction}
   */
  public static Shortcut getDownloadActionShortcut() {
    return DOWNLOAD_ACTION.getShortcut();
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
      if (MapillaryDownloader.getMode() == DOWNLOAD_MODE.MANUAL_ONLY) {
        setMenuEnabled(DOWNLOAD_VIEW_MENU, true);
      }
    }
    if (oldFrame != null && newFrame == null) { // map frame destroyed
      MapillaryMainDialog.destroyInstance();
      MapillaryHistoryDialog.destroyInstance();
      MapillaryChangesetDialog.destroyInstance();
      MapillaryFilterDialog.destroyInstance();
      ImageInfoPanel.destroyInstance();
      setMenuEnabled(DOWNLOAD_VIEW_MENU, false);
    }
  }

  /**
   * Enables/disables a {@link JMenuItem}.
   *
   * @param menu The JMenuItem object that is going to be enabled or disabled.
   * @param value <code>true</code> to enable the JMenuItem; <code>false</code> to disable it.
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

  /**
   * @return the current {@link MapView} without throwing a {@link NullPointerException}
   */
  public static MapView getMapView() {
    final MapFrame mf = Main.map;
    if (mf != null) {
      return mf.mapView;
    }
    return null;
  }
}
