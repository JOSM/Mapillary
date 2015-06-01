package org.openstreetmap.josm.plugins.mapillary;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.util.List;

import javax.swing.JDialog;
import javax.swing.JOptionPane;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.plugins.mapillary.downloads.MapillaryExportManager;

/**
 * Action that launches a MapillaryExportDialog.
 * 
 * @author nokutu
 *
 */
public class MapillaryExportAction extends JosmAction {

	MapillaryExportDialog dialog;

	public MapillaryExportAction() {
		super(tr("Export images"), "icon24.png", tr("Export images."), null,
				false);
	}

	@Override
	public void actionPerformed(ActionEvent e) {
		dialog = new MapillaryExportDialog();
		JOptionPane pane = new JOptionPane(dialog, JOptionPane.PLAIN_MESSAGE,
				JOptionPane.OK_CANCEL_OPTION);
		JDialog dlg = pane.createDialog(Main.parent, tr("Export images"));
		dlg.setMinimumSize(new Dimension(400, 150));
		dlg.setVisible(true);

		if (pane.getValue() != null
				&& (int) pane.getValue() == JOptionPane.OK_OPTION
				&& dialog.chooser != null) {
			if (dialog.group.isSelected(dialog.all.getModel())) {
				export(MapillaryData.getInstance().getImages());
			} else if (dialog.group.isSelected(dialog.sequence.getModel())) {
				export(MapillaryData.getInstance().getSelectedImage()
						.getSequence().getImages());
			} else if (dialog.group.isSelected(dialog.selected.getModel())) {
				export(MapillaryData.getInstance().getMultiSelectedImages());
			}
		}

		dlg.dispose();
	}

	/**
	 * Exports the given images from the database
	 */
	public void export(List<MapillaryImage> images) {
		new Thread(new MapillaryExportManager(tr("Downloading..."), images,
				dialog.chooser.getSelectedFile().toString())).start();
	}

}
