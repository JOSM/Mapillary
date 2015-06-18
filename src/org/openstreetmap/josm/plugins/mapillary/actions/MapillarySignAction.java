package org.openstreetmap.josm.plugins.mapillary.actions;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryToggleDialog;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.Shortcut;

/**
 * Switches the window mode from normal to sign and viceversa.
 * 
 * @author nokutu
 * @see MapillaryToggleDialog
 *
 */
public class MapillarySignAction extends JosmAction {

    public MapillarySignAction() {
        super(tr("Switch sign/normal mode"),
                new ImageProvider("icon24sign.png"),
                tr("Switch sign/normal mode"), Shortcut.registerShortcut(
                        "Mapillary sign",
                        tr("Switch Mapillary plugin's sign mode on/off"),
                        KeyEvent.VK_M, Shortcut.NONE), false, "mapillarySign",
                false);
        this.setEnabled(false);
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        MapillaryToggleDialog.getInstance().switchMode();
    }
}
