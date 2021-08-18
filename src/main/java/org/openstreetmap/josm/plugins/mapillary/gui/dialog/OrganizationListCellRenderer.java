package org.openstreetmap.josm.plugins.mapillary.gui.dialog;

import org.openstreetmap.josm.plugins.mapillary.data.mapillary.OrganizationRecord;
import org.openstreetmap.josm.tools.ImageProvider;

import javax.swing.DefaultListCellRenderer;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JList;
import java.awt.Component;
import java.awt.Image;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * A cell renderer for organization lists
 */
public class OrganizationListCellRenderer extends DefaultListCellRenderer {
    private static final long serialVersionUID = -1650696801628131389L;
    /** Scaled organization icons -- cached for performance */
    private static final Map<OrganizationRecord, ImageIcon> organizationScaledIcons = new ConcurrentHashMap<>();

    @Override
    public Component getListCellRendererComponent(JList<?> list, Object value, int index, boolean isSelected,
        boolean cellHasFocus) {
        JLabel comp = (JLabel) super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
        if (value instanceof OrganizationRecord) {
            OrganizationRecord organization = (OrganizationRecord) value;
            if (organization.getNiceName() != null && !organization.getNiceName().isEmpty()) {
                comp.setText(organization.getNiceName());
            } else {
                comp.setText(organization.getKey());
            }
            if (organization.getAvatar() != null) {
                comp.setIcon(organizationScaledIcons.computeIfAbsent(organization, tOrganization -> {
                    final ImageProvider.ImageSizes size = ImageProvider.ImageSizes.DEFAULT;
                    final Image scaledImage = tOrganization.getAvatar().getImage()
                        .getScaledInstance(size.getAdjustedWidth(), size.getAdjustedHeight(), Image.SCALE_SMOOTH);
                    return new ImageIcon(scaledImage);
                }));
            }
        }
        return comp;
    }
}
