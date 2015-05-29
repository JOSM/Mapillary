package org.openstreetmap.josm.plugins.mapillary;

import javax.swing.ImageIcon;
import javax.swing.JMenuItem;

import org.openstreetmap.josm.gui.MainMenu;
import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.gui.MapFrame;
import org.openstreetmap.josm.plugins.Plugin;
import org.openstreetmap.josm.plugins.PluginInformation;
import org.openstreetmap.josm.tools.ImageProvider;

/**
 * This is the main class for the mapillary plugin.
 * 
 * @author nokutu
 *
 */
public class MapillaryPlugin extends Plugin {

	public static final ImageIcon ICON = new ImageProvider("icon24.png").get();
	public static final ImageIcon ICON16 = new ImageProvider("icon16.png").get();
	public static final ImageIcon ICON16SELECTED = new ImageProvider("icon16selected.png").get();
	public static final int ICON_SIZE = 24;
	
	MapillaryDownloadAction downloadAction;
	MapillaryExportAction exportAction;
	
	public static JMenuItem DOWNLOAD_MENU;
	public static JMenuItem EXPORT_MENU;

	public MapillaryPlugin(PluginInformation info) {
		super(info);
		downloadAction = new MapillaryDownloadAction();
		exportAction = new MapillaryExportAction();

		DOWNLOAD_MENU = MainMenu.add(Main.main.menu.imageryMenu, downloadAction, false, 0);
		EXPORT_MENU = MainMenu.add(Main.main.menu.fileMenu, exportAction, false, 14);
		EXPORT_MENU.setEnabled(false);

	}

	/**
	 * Called when the JOSM map frame is created or destroyed.
	 */
	@Override
	public void mapFrameInitialized(MapFrame oldFrame, MapFrame newFrame) {
		if (oldFrame == null && newFrame != null) { // map frame added
			MapillaryToggleDialog.deleteInstance ();
		}
	}
	
	public static void setMenuEnabled(JMenuItem menu, boolean value) {
		menu.setEnabled(value);
	}
}

