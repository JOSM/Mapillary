package org.openstreetmap.josm.plugins.mapillary.actions;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryToggleDialog;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.Shortcut;

/**
 * Switches the window mode from normal to signal and viceversa.
 * 
 * @author nokutu
 * @see MapillaryToggleDialog
 *
 */
public class MapillarySignalAction extends JosmAction {

	public MapillarySignalAction() {
		super(tr("Switch signal mode"), new ImageProvider("icon24signal.png"),
				tr("Switch signal mode"), Shortcut.registerShortcut(
						"Mapillary signal", tr("Switch Mapillary plugin's signal mode on/off"),
						KeyEvent.VK_M, Shortcut.NONE), false,
				"mapillarySignal", false);
		this.setEnabled(false);
	}

	@Override
	public void actionPerformed(ActionEvent e) {
		MapillaryToggleDialog.getInstance().switchMode();
	}
}
