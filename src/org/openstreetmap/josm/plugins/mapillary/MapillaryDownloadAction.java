/**
 *
 */
package org.openstreetmap.josm.plugins.mapillary;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

import org.openstreetmap.josm.gui.layer.Layer;
import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.actions.JosmAction;
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
		super(tr("Mapillary"), "icon16.png",
				tr("Create Mapillary layer."), Shortcut.registerShortcut(
						"menu:Mapillary", tr("Menu: {0}", tr("Mapillary")),
						KeyEvent.VK_M, Shortcut.ALT_SHIFT), false);
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
		if (Main.map == null) {
			return;
		}
		for (Layer layer : Main.map.mapView.getAllLayers()) {
			if (layer instanceof MapillaryLayer) {
				System.out.println("Tuturu");
				this.layer = (MapillaryLayer) layer;
			}
		}

		if (this.layer == null)
			layer = new MapillaryLayer();
		else
			this.layer.download();
		/*
		 * MapillaryDialog dialog = new MapillaryDialog(); JOptionPane pane =
		 * new JOptionPane(dialog, JOptionPane.PLAIN_MESSAGE,
		 * JOptionPane.OK_CANCEL_OPTION); JDialog dlg =
		 * pane.createDialog(Main.parent, tr("Export"));
		 * dialog.setOptionPane(pane); dlg.setVisible(true);
		 * if(((Integer)pane.getValue()) == JOptionPane.OK_OPTION){ //
		 * MapillaryTask task = new MapillaryTask(); //
		 * Main.worker.execute(task); } dlg.dispose();
		 */
	}

}
