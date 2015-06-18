package org.openstreetmap.josm.plugins.mapillary.gui;

import static org.openstreetmap.josm.tools.I18n.tr;
import java.awt.FlowLayout;

import javax.swing.JCheckBox;
import javax.swing.JPanel;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.gui.preferences.PreferenceTabbedPane;
import org.openstreetmap.josm.gui.preferences.SubPreferenceSetting;
import org.openstreetmap.josm.gui.preferences.TabPreferenceSetting;

public class MapillaryPreferenceSetting implements SubPreferenceSetting {

    private JCheckBox reverseButtons = new JCheckBox(
            tr("Reverse buttons position when displaying images."));
    private JCheckBox downloadMode = new JCheckBox(
            tr("Download images manually"));

    @Override
    public TabPreferenceSetting getTabPreferenceSetting(PreferenceTabbedPane gui) {
        return gui.getDisplayPreference();
    }

    @Override
    public void addGui(PreferenceTabbedPane gui) {
        JPanel panel = new JPanel();

        reverseButtons.setSelected(Main.pref
                .getBoolean("mapillary.reverse-buttons"));
        downloadMode.setSelected(Main.pref
                .getBoolean("mapillary.download-manually"));

        panel.setLayout(new FlowLayout(FlowLayout.LEFT));
        panel.add(reverseButtons);
        panel.add(downloadMode);
        gui.getDisplayPreference().addSubTab(this, "Mapillary", panel);
    }

    @Override
    public boolean ok() {
        boolean mod = false;
        Main.pref.put("mapillary.reverse-buttons", reverseButtons.isSelected());
        Main.pref.put("mapillary.download-manually", downloadMode.isSelected());
        return mod;
    }

    @Override
    public boolean isExpert() {
        return false;
    }

}
