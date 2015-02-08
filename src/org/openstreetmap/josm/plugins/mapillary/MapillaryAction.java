/**
 * 
 */
package org.openstreetmap.josm.plugins.mapillary;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

import javax.swing.JDialog;
import javax.swing.JOptionPane;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.tools.Shortcut;


/**
 * @author Polyglot
 *
 */
public class MapillaryAction extends JosmAction {
	
	public MapillaryAction(){
        super(tr("Mapillary"), "images/icon24.png",
        tr("Create Mapillary layer."),
        Shortcut.registerShortcut("menu:Mapillary", tr("Menu: {0}", tr("Mapillary")),
        KeyEvent.VK_M, Shortcut.ALT_CTRL), false);
    }

	/* (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent arg0) {
		MapillaryDialog dialog = new MapillaryDialog();
        JOptionPane pane = new JOptionPane(dialog, JOptionPane.PLAIN_MESSAGE, JOptionPane.OK_CANCEL_OPTION);
        JDialog dlg = pane.createDialog(Main.parent, tr("Export"));
        dialog.setOptionPane(pane);
        dlg.setVisible(true);
        if(((Integer)pane.getValue()) == JOptionPane.OK_OPTION){
            // MapillaryTask task = new MapillaryTask();
            // Main.worker.execute(task);
        }
        dlg.dispose();
	}

}
