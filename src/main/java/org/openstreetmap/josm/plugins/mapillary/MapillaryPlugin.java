// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary;

import java.awt.Component;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import javax.swing.Action;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.MainMenu;
import org.openstreetmap.josm.gui.MapFrame;
import org.openstreetmap.josm.gui.MapView;
import org.openstreetmap.josm.gui.dialogs.ToggleDialog;
import org.openstreetmap.josm.gui.dialogs.properties.PropertiesDialog;
import org.openstreetmap.josm.gui.layer.Layer;
import org.openstreetmap.josm.gui.preferences.PreferenceSetting;
import org.openstreetmap.josm.io.remotecontrol.RequestProcessor;
import org.openstreetmap.josm.plugins.Plugin;
import org.openstreetmap.josm.plugins.PluginInformation;
import org.openstreetmap.josm.plugins.mapillary.actions.MapObjectLayerAction;
import org.openstreetmap.josm.plugins.mapillary.actions.MapPointObjectLayerAction;
import org.openstreetmap.josm.plugins.mapillary.actions.MapillaryDownloadAction;
import org.openstreetmap.josm.plugins.mapillary.actions.MapillaryExportAction;
import org.openstreetmap.josm.plugins.mapillary.actions.MapillaryImportAction;
import org.openstreetmap.josm.plugins.mapillary.actions.MapillaryWalkAction;
import org.openstreetmap.josm.plugins.mapillary.actions.MapillaryZoomAction;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.VectorDataSelectionListener;
import org.openstreetmap.josm.plugins.mapillary.gui.DataMouseListener;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryMainDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryPreferenceSetting;
import org.openstreetmap.josm.plugins.mapillary.gui.dialog.MapillaryExpertFilterDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.dialog.MapillaryFilterDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.imageinfo.ImageInfoPanel;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.PointObjectLayer;
import org.openstreetmap.josm.plugins.mapillary.io.remotecontrol.MapillaryRemoteControl;
import org.openstreetmap.josm.plugins.mapillary.oauth.MapillaryUser;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.plugins.mapillary.utils.SecurityManagerUtils;
import org.openstreetmap.josm.tools.Destroyable;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.Logging;

/**
 * This is the main class of the Mapillary plugin.
 */
public class MapillaryPlugin extends Plugin implements Destroyable {

    public static final ImageProvider LOGO = new ImageProvider("mapillary-logo");

    /** Zoom action */
    private static final MapillaryZoomAction ZOOM_ACTION = new MapillaryZoomAction();
    /** Walk action */
    private static final MapillaryWalkAction WALK_ACTION = new MapillaryWalkAction();

    private final List<ToggleDialog> toggleDialog = new ArrayList<>();

    private final List<Destroyable> destroyables = new ArrayList<>();

    private DataMouseListener dataMouseListener;

    private MapillaryKeyListener popupHandler;

    /**
     * Main constructor.
     *
     * @param info Required information of the plugin. Obtained from the jar file.
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
        mapillaryDownloadAction.updateEnabledState();
        MainMenu.add(menu.imagerySubMenu, mapillaryDownloadAction, false);
        destroyables.add(mapillaryDownloadAction);

        MapillaryImportAction mapillaryImportAction = new MapillaryImportAction();
        MainMenu.add(menu.fileMenu, mapillaryImportAction, false, 14);
        destroyables.add(mapillaryImportAction);

        MainMenu.add(menu.viewMenu, ZOOM_ACTION, false, 15);
        destroyables.add(ZOOM_ACTION);

        MainMenu.add(menu.moreToolsMenu, WALK_ACTION, false);
        destroyables.add(WALK_ACTION);

        MapObjectLayerAction mapObjectLayerAction = new MapObjectLayerAction();
        MainMenu.add(menu.imagerySubMenu, mapObjectLayerAction, false);
        mapObjectLayerAction.updateEnabledState();
        destroyables.add(mapObjectLayerAction);

        MapPointObjectLayerAction mapPointObjectLayerAction = new MapPointObjectLayerAction();
        MainMenu.add(menu.imagerySubMenu, mapPointObjectLayerAction, false);
        mapPointObjectLayerAction.updateEnabledState();
        destroyables.add(mapPointObjectLayerAction);

        // TODO remove in destroy (not currently possible)
        RequestProcessor.addRequestHandlerClass("photo", MapillaryRemoteControl.class);
        // instantiate to get action into Remote Control Preferences
        new MapillaryRemoteControl();
        destroyables.add(new MapillaryLayerListener(MainApplication.getLayerManager()));

        SecurityManagerUtils.doInitializations();
        mapFrameInitialized(null, MainApplication.getMap());
    }

    public static VectorDataSelectionListener[] getMapillaryDataListeners() {
        return new VectorDataSelectionListener[] { WALK_ACTION, ZOOM_ACTION };
    }

    /**
     * Get the walk action
     *
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
            toggleDialog.add(MapillaryFilterDialog.getInstance());
            newFrame.addToggleDialog(MapillaryFilterDialog.getInstance(), false);
            newFrame.addToggleDialog(MapillaryExpertFilterDialog.getInstance(), true);
            toggleDialog.add(MapillaryExpertFilterDialog.getInstance());
            newFrame.addToggleDialog(ImageInfoPanel.getInstance());
            toggleDialog.add(ImageInfoPanel.getInstance());

            // This fixes a UI issue -- for whatever reason, the tab pane is occasionally unusable when the expert
            // filter
            // dialog is added.
            newFrame.conflictDialog.getToggleAction().actionPerformed(null);
            newFrame.conflictDialog.getToggleAction().actionPerformed(null);

            PropertiesDialog properties = newFrame.propertiesDialog;

            try {
                Field tagMenuField = PropertiesDialog.class.getDeclaredField("tagMenu");
                tagMenuField.setAccessible(true);
                JPopupMenu tagMenu = (JPopupMenu) tagMenuField.get(properties);
                popupHandler = new MapillaryKeyListener(properties, tagMenu);
                this.destroyables.add(popupHandler);
            } catch (ReflectiveOperationException e) {
                Logging.debug(e);
            }

            this.dataMouseListener = new DataMouseListener();
            this.destroyables.add(this.dataMouseListener);
        } else if (oldFrame != null && newFrame == null) { // map frame removed
            this.destroyables.remove(this.dataMouseListener);
            this.dataMouseListener.destroy();
            this.dataMouseListener = null;
            if (this.popupHandler != null) {
                this.destroyables.remove(this.popupHandler);
                this.popupHandler.destroy();
                this.popupHandler = null;
            }
            toggleDialog.forEach(ToggleDialog::destroy);
            toggleDialog.clear();
        }
    }

    @Override
    public PreferenceSetting getPreferenceSetting() {
        return new MapillaryPreferenceSetting();
    }

    /**
     * Get the current mapview
     *
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
        this.mapFrameInitialized(MainApplication.getMap(), null);
        MainMenu menu = MainApplication.getMenu();
        for (JMenu jmenu : Arrays.asList(menu.dataMenu, menu.viewMenu, menu.imagerySubMenu, menu.moreToolsMenu,
            menu.editMenu, menu.fileMenu, menu.windowMenu)) {
            clearMenues(jmenu);
        }
        for (Class<? extends Layer> layerClazz : Arrays.asList(MapillaryLayer.class, PointObjectLayer.class)) {
            MainApplication.getLayerManager().getLayersOfType(layerClazz)
                .forEach(layer -> MainApplication.getLayerManager().removeLayer(layer));
        }
        MapFrame frame = MainApplication.getMap();
        if (frame != null) {
            toggleDialog.forEach(frame::removeToggleDialog);
        }
        destroyables.forEach(Destroyable::destroy);
        toggleDialog.forEach(ToggleDialog::destroy);
    }

    private void clearMenues(JMenu menu) {
        final Map<Action, Component> actions = Arrays.stream(menu.getMenuComponents())
            .filter(JMenuItem.class::isInstance).map(JMenuItem.class::cast).filter(j -> j.getAction() != null)
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
