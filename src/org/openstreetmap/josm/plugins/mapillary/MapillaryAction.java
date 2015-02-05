/**
 * 
 */
package org.openstreetmap.josm.plugins.sumoconvert;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

import javax.swing.JDialog;
import javax.swing.JOptionPane;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.tools.Shortcut;


/**
 * @author ignacio_palermo
 *
 */
public class SumoExportAction extends JosmAction {
	
	public SumoExportAction(){
        super(tr("OSM Export"), "images/dialogs/logo-sumo.png",
        tr("Export traffic data to SUMO network file."),
        Shortcut.registerShortcut("menu:sumoexport", tr("Menu: {0}", tr("SUMO Export")),
        KeyEvent.VK_G, Shortcut.ALT_CTRL), false);
    }

	/* (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent arg0) {
		SumoExportDialog dialog = new SumoExportDialog();
        JOptionPane pane = new JOptionPane(dialog, JOptionPane.PLAIN_MESSAGE, JOptionPane.OK_CANCEL_OPTION);
        JDialog dlg = pane.createDialog(Main.parent, tr("Export"));
        dialog.setOptionPane(pane);
        dlg.setVisible(true);
        if(((Integer)pane.getValue()) == JOptionPane.OK_OPTION){
            ExportTask task = new ExportTask();
            Main.worker.execute(task);
        }
        dlg.dispose();
	}

}
