package org.openstreetmap.josm.plugins.mapillary;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.event.ActionEvent;
import java.awt.image.BufferedImage;
import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.HashMap;

import org.apache.commons.jcs.access.CacheAccess;
import org.openstreetmap.josm.data.cache.BufferedImageCacheEntry;
import org.openstreetmap.josm.data.cache.CacheEntry;
import org.openstreetmap.josm.data.cache.CacheEntryAttributes;
import org.openstreetmap.josm.data.cache.ICachedLoaderListener;
import org.openstreetmap.josm.data.cache.JCSCacheManager;
import org.openstreetmap.josm.gui.dialogs.ToggleDialog;
import org.openstreetmap.josm.gui.SideButton;
import org.openstreetmap.josm.plugins.mapillary.cache.MapillaryCache;
import org.openstreetmap.josm.tools.ImageProvider;

import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.SwingUtilities;
import javax.swing.AbstractAction;
import javax.swing.JPanel;

/**
 * Toggle dialog that shows an image and some buttons.
 * 
 * @author nokutu
 *
 */
public class MapillaryToggleDialog extends ToggleDialog implements ICachedLoaderListener{

	public static MapillaryToggleDialog INSTANCE;

	public volatile JLabel active;
	public volatile MapillaryImage image;

	final SideButton nextButton = new SideButton(new nextPictureAction());
	final SideButton previousButton = new SideButton(
			new previousPictureAction());

	final JPanel buttons;

	public MapillaryToggleDialog() {
		super(tr("Mapillary image"), "mapillary", tr("Open Mapillary window"),
				null, 200);
		showDefault();
		buttons = new JPanel();
		buttons.setLayout(new FlowLayout(FlowLayout.CENTER));
		buttons.add(previousButton);
		buttons.add(nextButton);
		this.add(buttons, BorderLayout.SOUTH);
	}

	public static MapillaryToggleDialog getInstance() {
		if (INSTANCE == null) {
			INSTANCE = new MapillaryToggleDialog();
		}
		return INSTANCE;
	}
	
	public static void deleteInstance() {
		INSTANCE = null;
	}

	public void showDefault() {
		if (active != null)
			this.remove(active);
		JLabel label = new JLabel("", new ImageProvider(
				"mapillary_icon_960.png").setWidth(100).setHeight(100).get(),
				JLabel.CENTER);
		active = label;
		this.add(active);
		this.updateUI();
	}

	public synchronized void updateImage() {
		if (!SwingUtilities.isEventDispatchThread()) {
			SwingUtilities.invokeLater(new Runnable() {
				@Override
				public void run() {
					updateImage();
				}
			});
		} else {
			if (MapillaryLayer.INSTANCED == false) {
				showDefault();
				return;
			}
			if (this.image != null) {
				CacheAccess<String, BufferedImageCacheEntry> prev;
				try {
					prev = JCSCacheManager.getCache("mapillary");
					HashMap<String, String> headers = new HashMap<>();
					MapillaryCache cache = new MapillaryCache(image.getKey(), MapillaryCache.Type.FULL_IMAGE, prev, 200000, 200000, headers);
					cache.submit(MapillaryToggleDialog.getInstance(), false);
				} catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (Exception e) {
					System.out.println("Hello");
				}
				/*if (image.getImage() != null) {
					showImage();
				} else if (image.getThumbnail() != null) {
					showThumbnail();
				}*/
			} else
				showDefault();
		}
	}

	public synchronized void setImage(MapillaryImage image) {
		this.image = image;
	}

	public synchronized MapillaryImage getImage() {
		return this.image;
	}

	class nextPictureAction extends AbstractAction {
		public nextPictureAction() {
			putValue(NAME, tr("Next picture"));
			putValue(SHORT_DESCRIPTION,
					tr("Shows the next picture in the sequence"));
		}

		@Override
		public void actionPerformed(ActionEvent e) {
			if (MapillaryToggleDialog.getInstance().getImage() != null)
				MapillaryData.getInstance().selectNext();
		}
	}

	class previousPictureAction extends AbstractAction {
		public previousPictureAction() {
			putValue(NAME, tr("Previous picture"));
			putValue(SHORT_DESCRIPTION,
					tr("Shows the previous picture in the sequence"));
		}

		@Override
		public void actionPerformed(ActionEvent e) {
			if (MapillaryToggleDialog.getInstance().getImage() != null)
				MapillaryData.getInstance().selectPrevious();
		}
	}

	@Override
	public void loadingFinished(CacheEntry data,
			CacheEntryAttributes attributes, LoadResult result) {
		System.out.println("Listener");
		try {
			BufferedImage img = ImageIO.read(new ByteArrayInputStream(data.getContent()));
			this.remove(active);
			JLabel label = new JLabel("", new ImageIcon(img), JLabel.CENTER);
			active = label;
			this.add(active);
			this.updateUI();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
			
		
		
	}
}
