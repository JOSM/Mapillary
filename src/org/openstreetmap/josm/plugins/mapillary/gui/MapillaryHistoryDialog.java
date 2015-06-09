package org.openstreetmap.josm.plugins.mapillary.gui;

import static org.openstreetmap.josm.tools.I18n.tr;

import org.openstreetmap.josm.gui.dialogs.ToggleDialog;

public class MapillaryHistoryDialog extends ToggleDialog {

	public MapillaryHistoryDialog() {
		super(tr("Mapillary history"), "mapillary.png",
				tr("Open Mapillary history dialog"), null, 200);
	}
}
