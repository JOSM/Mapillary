/**
 *
 */
package org.openstreetmap.josm.plugins.mapillary.actions;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

import org.openstreetmap.josm.gui.layer.Layer;
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
	MapillaryLayer layer;

	public MapillaryDownloadAction() {
		super(tr("Mapillary"), new ImageProvider("icon24.png"),
				tr("Create Mapillary layer."), Shortcut.registerShortcut(
						"menu:Mapillary", tr("Menu: {0}", tr("Mapillary")),
						KeyEvent.VK_M, Shortcut.ALT_CTRL_SHIFT), false,
				"mapillaryDownload", false);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent arg0) {
		this.layer = null;
		if (Main.map == null || Main.map.mapView == null
				|| Main.map.mapView.getEditLayer() == null) {
			return;
		}
		for (Layer layer : Main.map.mapView.getAllLayers()) {
			if (layer instanceof MapillaryLayer) {
				this.layer = (MapillaryLayer) layer;
			}
		}

		if (this.layer == null)
			layer = new MapillaryLayer();
		else {
			if (Main.map.mapView.getActiveLayer() != layer)
				Main.map.mapView.setActiveLayer(layer);
			else
				Main.map.mapView
						.setActiveLayer(Main.map.mapView.getEditLayer());
		}
	}

}
