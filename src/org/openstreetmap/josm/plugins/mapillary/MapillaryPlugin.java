package org.openstreetmap.josm.plugins.sumoconvert;

import java.util.ArrayList;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.data.osm.Relation;
import org.openstreetmap.josm.gui.MapFrame;
import org.openstreetmap.josm.gui.preferences.PreferenceSetting;
import org.openstreetmap.josm.plugins.Plugin;
import org.openstreetmap.josm.plugins.PluginInformation;
import org.openstreetmap.josm.plugins.turnrestrictions.list.TurnRestrictionsListDialog;
import org.openstreetmap.josm.plugins.turnrestrictions.preferences.PreferenceEditor;
import org.openstreetmap.josm.plugins.turnrestrictions.*;

/**
 * This is the main class for the sumoconvert plugin.
 * 
 */
public class SumoConvertPlugin extends Plugin{
    
	private final ArrayList<Relation> turnrestrictions = new ArrayList<Relation>();
	private SumoExportAction exportAction;
	
    public SumoConvertPlugin(PluginInformation info) {
        super(info);
        exportAction = new SumoExportAction();
        Main.main.menu.toolsMenu.add(exportAction);
        System.out.println(getPluginDir());
    }
    
    /**
     * Called when the JOSM map frame is created or destroyed. 
     */
    @Override
    public void mapFrameInitialized(MapFrame oldFrame, MapFrame newFrame) {             
        if (oldFrame == null && newFrame != null) { // map frame added
        	
            //TurnRestrictionsListDialog dialog  = new TurnRestrictionsListDialog();
            //add the dialog
            //newFrame.addToggleDialog(dialog);
            //CreateOrEditTurnRestrictionAction.getInstance();
        }
    }

    @Override
    public PreferenceSetting getPreferenceSetting() {
        return new PreferenceEditor();
    }
}
