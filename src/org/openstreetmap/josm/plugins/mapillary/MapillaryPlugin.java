// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary;

import org.openstreetmap.josm.gui.MainApplication;
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
import org.openstreetmap.josm.plugins.mapillary.oauth.MapillaryUser;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.tools.ImageProvider;

/**
 * This is the main class of the Mapillary plugin.
 */
public class MapillaryPlugin extends Plugin {

  public static final ImageProvider LOGO = new ImageProvider("mapillary-logo");

  /** Zoom action */
  private static final MapillaryZoomAction ZOOM_ACTION = new MapillaryZoomAction();
  /** Walk action */
  private static final MapillaryWalkAction WALK_ACTION = new MapillaryWalkAction();
  /** Upload action */
  private static final MapillaryUploadAction UPLOAD_ACTION = new MapillaryUploadAction();

  static {
    if (MainApplication.getMainFrame() != null) {
      MainMenu.add(MainApplication.getMenu().fileMenu, new MapillaryExportAction(), false, 14);
      MainMenu.add(MainApplication.getMenu().imagerySubMenu, new MapillaryDownloadAction(), false);
      MainMenu.add(MainApplication.getMenu().fileMenu, new MapillaryImportAction(), false, 14);
      MainMenu.add(MainApplication.getMenu().viewMenu, ZOOM_ACTION, false, 15);
      MainMenu.add(MainApplication.getMenu().fileMenu, new MapillaryDownloadViewAction(), false, 14);
      MainMenu.add(MainApplication.getMenu().dataMenu, new MapillaryJoinAction(), false);
      MainMenu.add(MainApplication.getMenu().moreToolsMenu, WALK_ACTION, false);
      MainMenu.add(MainApplication.getMenu().fileMenu, UPLOAD_ACTION, false, 14);
      MainMenu.add(MainApplication.getMenu().imagerySubMenu, new MapObjectLayerAction(), false);
    }
  }

  /**
   * Main constructor.
   *
   * @param info
   *        Required information of the plugin. Obtained from the jar file.
   */
  public MapillaryPlugin(PluginInformation info) {
    super(info);

    if (MapillaryProperties.ACCESS_TOKEN.get() == null) {
      MapillaryUser.setTokenValid(false);
    }
  }

  static MapillaryDataListener[] getMapillaryDataListeners() {
    return new MapillaryDataListener[]{UPLOAD_ACTION, WALK_ACTION, ZOOM_ACTION};
  }

  /**
   * @return the {@link MapillaryWalkAction} for the plugin
   */
  public static MapillaryWalkAction getWalkAction() {
    return WALK_ACTION;
  }

  /**
   * Called when the JOSM map frame is created or destroyed.
   */
  @Override
  public void mapFrameInitialized(MapFrame oldFrame, MapFrame newFrame) {
    if (oldFrame == null && newFrame != null) { // map frame added
      MainApplication.getMap().addToggleDialog(MapillaryMainDialog.getInstance(), false);
      MapillaryMainDialog.getInstance().setImageInfoHelp(new ImageInfoHelpPopup(
        MainApplication.getMap().addToggleDialog(ImageInfoPanel.getInstance(), false)
      ));
      MainApplication.getMap().addToggleDialog(MapillaryHistoryDialog.getInstance(), false);
      MainApplication.getMap().addToggleDialog(MapillaryChangesetDialog.getInstance(), false);
      MainApplication.getMap().addToggleDialog(MapillaryFilterDialog.getInstance(), false);
    }
    if (oldFrame != null && newFrame == null) { // map frame destroyed
      MapillaryMainDialog.destroyInstance();
      MapillaryHistoryDialog.destroyInstance();
      MapillaryChangesetDialog.destroyInstance();
      MapillaryFilterDialog.destroyInstance();
      ImageInfoPanel.destroyInstance();
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
    final MapFrame mf = MainApplication.getMap();
    if (mf != null) {
      return mf.mapView;
    }
    return null;
  }
}
