// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.actions;

import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.layer.Layer;
import org.openstreetmap.josm.gui.util.GuiHelper;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.PointObjectLayer;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryKeys;
import org.openstreetmap.josm.tools.ImageProvider.ImageSizes;
import org.openstreetmap.josm.tools.Shortcut;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

import static org.openstreetmap.josm.tools.I18n.marktr;
import static org.openstreetmap.josm.tools.I18n.tr;

/**
 * An action to create a layer for Mapillary point objects (not traffic signs)
 */
public class MapPointObjectLayerAction extends JosmAction {
    private static final long serialVersionUID = 5780337309290262545L;
    private static final String ACTION_NAME = marktr("Mapillary point features layer");
    private static final String TOOLTIP = marktr(
        "Displays the layer displaying the map point objects detected by Mapillary");

    public MapPointObjectLayerAction() {
        super(tr(ACTION_NAME), MapillaryPlugin.LOGO.setSize(ImageSizes.DEFAULT), tr(TOOLTIP),
            Shortcut.registerShortcut("mapillary:pointFeaturesLayer", tr(ACTION_NAME), KeyEvent.CHAR_UNDEFINED, Shortcut.NONE),
            false, "mapillary:pointFeaturesLayer", true);
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        GuiHelper.runInEDTAndWait(() -> {
            // Synchronization lock must be held by EDT thread
            // See {@link LayerManager#addLayer(org.openstreetmap.josm.gui.layer.Layer, boolean)}.
            synchronized (MainApplication.getLayerManager()) {
                Layer layer = MainApplication.getLayerManager().getActiveLayer();
                if (MainApplication.getLayerManager().getActiveDataSet() != null
                    && MainApplication.getLayerManager().getLayersOfType(PointObjectLayer.class).stream()
                        .noneMatch(l -> MapillaryKeys.MAPILLARY_POINT_OBJECTS.equals(l.getInfo()))) {
                    MainApplication.getLayerManager()
                        .addLayer(new PointObjectLayer(MapillaryKeys.MAPILLARY_POINT_OBJECTS), false);
                    if (layer != null) {
                        MainApplication.getLayerManager().setActiveLayer(layer);
                    }
                }
            }
        });
    }

    @Override
    public void updateEnabledState() {
        super.setEnabled(MainApplication.isDisplayingMapView());
    }

}
