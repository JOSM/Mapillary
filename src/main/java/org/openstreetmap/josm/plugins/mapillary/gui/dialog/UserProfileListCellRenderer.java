// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.dialog;

import java.awt.Component;

import javax.swing.DefaultListCellRenderer;
import javax.swing.JLabel;
import javax.swing.JList;

import org.openstreetmap.josm.plugins.mapillary.model.UserProfile;

/**
 * A renderer for {@link UserProfile} objects
 */
class UserProfileListCellRenderer extends DefaultListCellRenderer {
    @Override
    public Component getListCellRendererComponent(JList<?> list, Object value, int index, boolean isSelected,
        boolean cellHasFocus) {
        JLabel comp = (JLabel) super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
        if (value instanceof UserProfile userProfile) {
            if (userProfile.username() != null && !userProfile.username().isBlank()) {
                comp.setText(userProfile.username());
            } else if (!UserProfile.NONE.equals(userProfile)) {
                comp.setText(Long.toString(userProfile.key()));
            } else {
                comp.setText("");
            }
        }
        return comp;
    }
}
