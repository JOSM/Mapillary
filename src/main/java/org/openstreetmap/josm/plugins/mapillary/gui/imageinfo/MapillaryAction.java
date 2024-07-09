// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.imageinfo;

import javax.swing.Action;

import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.tools.Shortcut;

/**
 * A common class for Mapillary actions
 */
abstract class MapillaryAction extends JosmAction {
    MapillaryAction(String name, String icon, String tooltip, Shortcut shortcut, boolean registerInToolbar,
        String toolbarId, boolean installAdapters) {
        super(name, icon, tooltip, shortcut, registerInToolbar, toolbarId, installAdapters);
        // We don't need the large icon, and it messes with spacing in buttons
        this.putValue(Action.LARGE_ICON_KEY, null);
    }
}
