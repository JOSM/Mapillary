package org.openstreetmap.josm.plugins.mapillary.gui.layer;

import java.awt.Color;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.UIManager;

class AdditionalActionPanel extends JPanel {
    private boolean hasEnabledActions;

    /**
     * Create an additional action panel
     *
     * @param actionButtons The action buttons
     */
    public AdditionalActionPanel(JButton... actionButtons) {
        this.setBackground(UIManager.getColor("ToolTip.background"));
        this.setForeground(UIManager.getColor("ToolTip.foreground"));
        this.setFont(UIManager.getFont("ToolTip.font"));
        this.setBorder(BorderFactory.createLineBorder(Color.black));
        for (JButton button : actionButtons) {
            this.add(button);
            this.hasEnabledActions = this.hasEnabledActions || button.getAction().isEnabled();
        }
    }

    /**
     * Check if there is content in the layer
     *
     * @return {@code true} if there is content to show
     */
    public boolean hasContent() {
        return this.hasEnabledActions;
    }
}
