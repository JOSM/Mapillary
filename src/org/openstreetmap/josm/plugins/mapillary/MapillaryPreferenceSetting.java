package org.openstreetmap.josm.plugins.mapillary;

import java.awt.FlowLayout;

import javax.swing.JCheckBox;
import javax.swing.JPanel;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.gui.preferences.PreferenceTabbedPane;
import org.openstreetmap.josm.gui.preferences.SubPreferenceSetting;
import org.openstreetmap.josm.gui.preferences.TabPreferenceSetting;

public class MapillaryPreferenceSetting implements SubPreferenceSetting {

	private JCheckBox reverseButtons = new JCheckBox("Reverse buttons position when displaying images.");;
	
	@Override
	public TabPreferenceSetting getTabPreferenceSetting(PreferenceTabbedPane gui) {
		return gui.getDisplayPreference();
	}

	@Override
	public void addGui(PreferenceTabbedPane gui) {
		// TODO Auto-generated method stub
		JPanel panel = new JPanel();
		
		reverseButtons.setSelected(Main.pref.getBoolean("mapillary.reverse-buttons"));
		
		panel.add(reverseButtons);
		panel.setLayout(new FlowLayout(FlowLayout.LEFT));
        gui.getDisplayPreference().addSubTab(this, "Mapillary", panel);
	}

	@Override
	public boolean ok() {
        boolean mod = false;
        Main.pref.put("mapillary.reverse-buttons", reverseButtons.isSelected());
        return mod;
	}

	@Override
	public boolean isExpert() {
		// TODO Auto-generated method stub
		return false;
	}

}
