// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.dialog;

import java.awt.Component;
import java.awt.Image;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javax.swing.DefaultListCellRenderer;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JList;

import org.openstreetmap.josm.plugins.mapillary.data.mapillary.OrganizationRecord;
import org.openstreetmap.josm.tools.ImageProvider;

/**
 * A cell renderer for organization lists
 */
public class OrganizationListCellRenderer extends DefaultListCellRenderer {
    private static final long serialVersionUID = -1650696801628131389L;
    /** Scaled organization icons -- cached for performance */
    private static final Map<OrganizationRecord, ImageIcon> ORGANIZATION_SCALED_ICONS = new ConcurrentHashMap<>(0);

    @Override
    public Component getListCellRendererComponent(JList<?> list, Object value, int index, boolean isSelected,
        boolean cellHasFocus) {
        JLabel comp = (JLabel) super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
        if (value instanceof OrganizationRecord) {
            OrganizationRecord organization = (OrganizationRecord) value;
            if ((organization.niceName() != null && !organization.niceName().isEmpty())
                || OrganizationRecord.NULL_RECORD.equals(organization)) {
                comp.setText(organization.niceName());
            } else {
                comp.setText(Long.toString(organization.id()));
            }
            if (organization.avatar() != null) {
                comp.setIcon(ORGANIZATION_SCALED_ICONS.computeIfAbsent(organization,
                    OrganizationListCellRenderer::scaleOrganizationIcon));
            }
        }
        return comp;
    }

    /**
     * Scale organization icons
     *
     * @param organization The organization whose icon needs to be scaled
     * @return The scaled icon
     */
    private static ImageIcon scaleOrganizationIcon(final OrganizationRecord organization) {
        final ImageProvider.ImageSizes size = ImageProvider.ImageSizes.DEFAULT;
        final Image scaledImage = organization.avatar().getImage().getScaledInstance(size.getAdjustedWidth(),
            size.getAdjustedHeight(), Image.SCALE_SMOOTH);
        return new ImageIcon(scaledImage);
    }
}
