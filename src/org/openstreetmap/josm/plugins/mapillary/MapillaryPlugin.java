package org.openstreetmap.josm.plugins.mapillary;

import java.io.IOException;

import javax.swing.ImageIcon;
import javax.swing.JMenuItem;

import org.apache.commons.jcs.access.CacheAccess;
import org.openstreetmap.josm.data.cache.BufferedImageCacheEntry;
import org.openstreetmap.josm.data.cache.JCSCacheManager;
import org.openstreetmap.josm.gui.MainMenu;
import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.gui.MapFrame;
import org.openstreetmap.josm.plugins.Plugin;
import org.openstreetmap.josm.plugins.PluginInformation;
import org.openstreetmap.josm.tools.ImageProvider;

/**
 * This is the main class of the Mapillary plugin.
 * 
 * @author nokutu
 *
 */
public class MapillaryPlugin extends Plugin {

	public static final ImageIcon ICON24 = new ImageProvider("icon24.png")
			.get();
	public static final ImageIcon ICON16 = new ImageProvider("icon16.png")
			.get();
	public static final ImageIcon MAP_ICON = new ImageProvider("mapicon.png")
			.get();
	public static final ImageIcon MAP_ICON_SELECTED = new ImageProvider(
			"mapiconselected.png").get();
	public static final int ICON_SIZE = 24;

	public static CacheAccess<String, BufferedImageCacheEntry> CACHE;

	private final MapillaryDownloadAction downloadAction;
	private final MapillaryExportAction exportAction;

	public static JMenuItem DOWNLOAD_MENU;
	public static JMenuItem EXPORT_MENU;

	public MapillaryPlugin(PluginInformation info) {
		super(info);
		synchronized (this) {
			Main.logLevel = 1;
		}
		downloadAction = new MapillaryDownloadAction();
		exportAction = new MapillaryExportAction();

		DOWNLOAD_MENU = MainMenu.add(Main.main.menu.imageryMenu,
				downloadAction, false);
		EXPORT_MENU = MainMenu.add(Main.main.menu.fileMenu, exportAction,
				false, 14);
		EXPORT_MENU.setEnabled(false);
		try {
			CACHE = JCSCacheManager.getCache("mapillary", 10, 10000,
					this.getPluginDir() + "/cache/");
		} catch (IOException e) {
			Main.error(e);
		}
	}

	/**
	 * Called when the JOSM map frame is created or destroyed.
	 */
	@Override
	public void mapFrameInitialized(MapFrame oldFrame, MapFrame newFrame) {
		if (oldFrame == null && newFrame != null) { // map frame added
		}
		if (oldFrame != null && newFrame == null) { // map frame destroyed
			MapillaryToggleDialog.destroyInstance();
		}
	}

	public static void setMenuEnabled(JMenuItem menu, boolean value) {
		menu.setEnabled(value);
	}
}
