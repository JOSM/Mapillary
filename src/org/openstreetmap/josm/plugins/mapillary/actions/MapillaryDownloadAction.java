/**
 *
 */
package org.openstreetmap.josm.plugins.mapillary.actions;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.Shortcut;

/**
 * Action that triggers the plugin.
 * 
 * @author nokutu
 *
 */
public class MapillaryDownloadAction extends JosmAction {

    public MapillaryDownloadAction() {
        super(tr("Mapillary"), new ImageProvider("icon24.png"),
                tr("Create Mapillary layer"), Shortcut.registerShortcut(
                        "Mapillary", tr("Start Mapillary layer"),
                        KeyEvent.VK_COMMA, Shortcut.SHIFT), false,
                "mapillaryDownload", false);
        this.setEnabled(false);
    }

    @Override
    public void actionPerformed(ActionEvent arg0) {
        if (MapillaryLayer.INSTANCE == null)
            MapillaryLayer.getInstance().download();
        else {
            if (Main.map.mapView.getActiveLayer() != MapillaryLayer
                    .getInstance())
                Main.map.mapView.setActiveLayer(MapillaryLayer.getInstance());
            else
                Main.map.mapView
                        .setActiveLayer(Main.map.mapView.getEditLayer());
        }
    }
}
