// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.actions;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.util.List;

import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.dialogs.LayerListDialog;
import org.openstreetmap.josm.gui.dialogs.LayerListDialog.LayerListModel;
import org.openstreetmap.josm.gui.layer.Layer;
import org.openstreetmap.josm.gui.layer.OsmDataLayer;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.ImageProvider.ImageSizes;
import org.openstreetmap.josm.tools.Logging;
import org.openstreetmap.josm.tools.Shortcut;

/**
 * Action that triggers the plugin. If in automatic mode, it will automatically
 * download the images in the areas where there is OSM data.
 *
 * @author nokutu
 */
public class MapillaryDownloadAction extends JosmAction {

    private static final long serialVersionUID = 325060354730454948L;
    public static final Shortcut SHORTCUT = Shortcut.registerShortcut("Mapillary", tr("Open Mapillary layer"),
        KeyEvent.VK_COMMA, Shortcut.SHIFT);

    /**
     * Main constructor.
     */
    public MapillaryDownloadAction() {
        super(tr("Mapillary"), new ImageProvider(MapillaryPlugin.LOGO).setSize(ImageSizes.DEFAULT),
            tr("Open Mapillary layer"), SHORTCUT, false, "mapillaryDownload", true);
    }

    @Override
    public void actionPerformed(ActionEvent ae) {
        if (!MapillaryLayer.hasInstance()) {
            addLayer();
            return;
        }

        try {
            // Successive calls to this action toggle the active layer between the OSM data layer and the mapillary
            // layer
            final OsmDataLayer editLayer = MainApplication.getLayerManager().getEditLayer();
            final Layer mapillaryLayer = MapillaryLayer.getInstance();
            if (MainApplication.getLayerManager().getActiveLayer() != mapillaryLayer) {
                MainApplication.getLayerManager().setActiveLayer(mapillaryLayer);
            } else if (editLayer != null) {
                MainApplication.getLayerManager().setActiveLayer(editLayer);
            }
        } catch (IllegalArgumentException e) {
            // If the MapillaryLayer is not managed by LayerManager but you try to set it as active layer
            Logging.warn(e);
        }
    }

    @Override
    public void updateEnabledState() {
        super.setEnabled(MainApplication.isDisplayingMapView());
    }

    /**
     * Add the MapillaryLayer to the JOSM layer manager
     */
    public static void addLayer() {
        LayerListModel model = LayerListDialog.getInstance().getModel();
        final Layer mapillaryLayer = MapillaryLayer.getInstance();
        if (!model.getLayerManager().containsLayer(mapillaryLayer)) {
            model.getLayerManager().addLayer(mapillaryLayer);
        }
        List<Layer> selected = model.getSelectedLayers();
        int index = model.getLayers().indexOf(model.getLayerManager().getActiveDataLayer());
        model.setSelectedLayer(mapillaryLayer);
        int mapillaryLayerIndex = model.getLayers().indexOf(mapillaryLayer);
        while (mapillaryLayerIndex < index && mapillaryLayerIndex < model.getLayers().size()) {
            model.moveDown(mapillaryLayerIndex);
            mapillaryLayerIndex++;
        }
        selected.forEach(model::setSelectedLayer);
    }
}
