// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary;

import java.awt.Component;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import javax.swing.Action;
import javax.swing.JMenu;
import javax.swing.JMenuItem;

import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.MainMenu;
import org.openstreetmap.josm.gui.MapFrame;
import org.openstreetmap.josm.gui.MapView;
import org.openstreetmap.josm.gui.dialogs.ToggleDialog;
import org.openstreetmap.josm.gui.layer.Layer;
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
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapObjectLayer;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.PointObjectLayer;
import org.openstreetmap.josm.plugins.mapillary.oauth.MapillaryUser;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.tools.Destroyable;
import org.openstreetmap.josm.tools.ImageProvider;

/**
 * This is the main class of the Mapillary plugin.
 */
public class MapillaryPlugin extends Plugin implements Destroyable {

  public static final ImageProvider LOGO = new ImageProvider("mapillary-logo");

  /** Zoom action */
  private static final MapillaryZoomAction ZOOM_ACTION = new MapillaryZoomAction();
  /** Walk action */
  private static final MapillaryWalkAction WALK_ACTION = new MapillaryWalkAction();
  /** Upload action */
  private static final MapillaryUploadAction UPLOAD_ACTION = new MapillaryUploadAction();

  private final List<ToggleDialog> toggleDialog = new ArrayList<>();

  private final List<Destroyable> destroyables = new ArrayList<>();

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

    MainMenu menu = MainApplication.getMenu();

    MapillaryExportAction mapillaryExportAction = new MapillaryExportAction();
    MainMenu.add(menu.fileMenu, mapillaryExportAction, false, 14);
    destroyables.add(mapillaryExportAction);

    MapillaryDownloadAction mapillaryDownloadAction = new MapillaryDownloadAction();
    MainMenu.add(menu.imagerySubMenu, mapillaryDownloadAction, false);
    destroyables.add(mapillaryDownloadAction);

    MapillaryImportAction mapillaryImportAction = new MapillaryImportAction();
    MainMenu.add(menu.fileMenu, mapillaryImportAction, false, 14);
    destroyables.add(mapillaryImportAction);

    MainMenu.add(menu.viewMenu, ZOOM_ACTION, false, 15);
    destroyables.add(ZOOM_ACTION);

    MapillaryDownloadViewAction mapillaryDownloadViewAction = new MapillaryDownloadViewAction();
    MainMenu.add(menu.fileMenu, mapillaryDownloadViewAction, false, 14);
    destroyables.add(mapillaryDownloadViewAction);

    MapillaryJoinAction mapillaryJoinAction = new MapillaryJoinAction();
    MainMenu.add(menu.dataMenu, mapillaryJoinAction, false);
    destroyables.add(mapillaryJoinAction);

    MainMenu.add(menu.moreToolsMenu, WALK_ACTION, false);
    destroyables.add(WALK_ACTION);

    MainMenu.add(menu.fileMenu, UPLOAD_ACTION, false, 14);
    destroyables.add(UPLOAD_ACTION);

    MapObjectLayerAction mapObjectLayerAction = new MapObjectLayerAction();
    MainMenu.add(menu.imagerySubMenu, mapObjectLayerAction, false);
    destroyables.add(mapObjectLayerAction);

    mapFrameInitialized(null, MainApplication.getMap());
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
      toggleDialog.forEach(ToggleDialog::destroy);
      toggleDialog.clear();
      toggleDialog.add(MapillaryMainDialog.getInstance());
      newFrame.addToggleDialog(MapillaryMainDialog.getInstance(), false);
      MapillaryMainDialog.getInstance().setImageInfoHelp(new ImageInfoHelpPopup(
          newFrame.addToggleDialog(ImageInfoPanel.getInstance(), false)
      ));
      toggleDialog.add(MapillaryHistoryDialog.getInstance());
      newFrame.addToggleDialog(MapillaryHistoryDialog.getInstance(), false);
      toggleDialog.add(MapillaryChangesetDialog.getInstance());
      newFrame.addToggleDialog(MapillaryChangesetDialog.getInstance(), false);
      toggleDialog.add(MapillaryFilterDialog.getInstance());
      newFrame.addToggleDialog(MapillaryFilterDialog.getInstance(), false);
    } else if (oldFrame != null && newFrame == null) { // map frame removed
      toggleDialog.forEach(ToggleDialog::destroy);
      toggleDialog.clear();
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

  @Override
  public void destroy() {
    MainMenu menu = MainApplication.getMenu();
    for (JMenu jmenu : Arrays.asList(menu.dataMenu, menu.viewMenu, menu.imagerySubMenu, menu.moreToolsMenu,
        menu.editMenu, menu.fileMenu, menu.windowMenu)) {
      clearMenues(jmenu);
    }
    for (Class<? extends Layer> layerClazz : Arrays.asList(MapillaryLayer.class, MapObjectLayer.class,
        PointObjectLayer.class)) {
      MainApplication.getLayerManager().getLayersOfType(layerClazz)
          .forEach(layer -> MainApplication.getLayerManager().removeLayer(layer));
    }
    MapFrame frame = MainApplication.getMap();
    if (frame != null) {
      frame.removeToggleDialog(MapillaryMainDialog.getInstance());
      frame.removeToggleDialog(MapillaryHistoryDialog.getInstance());
      frame.removeToggleDialog(MapillaryChangesetDialog.getInstance());
      frame.removeToggleDialog(MapillaryFilterDialog.getInstance());
    }
    MapillaryMainDialog.getInstance().destroy();
    MapillaryHistoryDialog.getInstance().destroy();
    MapillaryChangesetDialog.getInstance().destroy();
    MapillaryFilterDialog.getInstance().destroy();
    ImageInfoPanel.getInstance().destroy();
    destroyables.forEach(Destroyable::destroy);
    toggleDialog.forEach(ToggleDialog::destroy);
  }

  private void clearMenues(JMenu menu) {
    final Map<Action, Component> actions = Arrays.asList(menu.getMenuComponents()).stream()
        .filter(JMenuItem.class::isInstance).map(JMenuItem.class::cast)
        .collect(Collectors.toMap(JMenuItem::getAction, component -> component));
    final List<JosmAction> menuEntries = destroyables.parallelStream().filter(JosmAction.class::isInstance)
        .map(JosmAction.class::cast).collect(Collectors.toList());
    for (final Map.Entry<Action, Component> action : actions.entrySet()) {
      if (menuEntries.contains(action.getKey())) {
        menu.remove(action.getValue());
      }
    }
  }
}
