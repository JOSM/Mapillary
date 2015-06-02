package org.openstreetmap.josm.plugins.mapillary;

import org.apache.commons.jcs.access.CacheAccess;
import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.data.cache.BufferedImageCacheEntry;
import org.openstreetmap.josm.data.cache.CacheEntry;
import org.openstreetmap.josm.data.cache.CacheEntryAttributes;
import org.openstreetmap.josm.data.cache.ICachedLoaderListener;
import org.openstreetmap.josm.data.cache.JCSCacheManager;
import org.openstreetmap.josm.plugins.mapillary.cache.MapillaryCache;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * Database class for all the MapillaryImage objects.
 * 
 * @author nokutu
 * @see MapillaryImage
 * @see MapillarySequence
 *
 */
public class MapillaryData implements ICachedLoaderListener {
	public volatile static MapillaryData INSTANCE;

	private final List<MapillaryImage> images;
	private MapillaryImage selectedImage;
	private final List<MapillaryImage> multiSelectedImages;

	public MapillaryData() {
		images = new CopyOnWriteArrayList<>();
		multiSelectedImages = new ArrayList<>();
		selectedImage = null;
	}

	public static MapillaryData getInstance() {
		if (INSTANCE == null) {
			INSTANCE = new MapillaryData();
		}
		return INSTANCE;
	}

	/**
	 * Adds a set of MapillaryImages to the object, and then repaints mapView.
	 * 
	 * @param images
	 *            The set of images to be added.
	 */
	public synchronized void add(List<MapillaryImage> images) {
		for (MapillaryImage image : images) {
			add(image);
		}
	}

	/**
	 * Adds an MapillaryImage to the object, and then repaints mapView.
	 * 
	 * @param image
	 *            The image to be added.
	 */
	public synchronized void add(MapillaryImage image) {
		if (!images.contains(image)) {
			this.images.add(image);
		}
		dataUpdated();
	}

	/**
	 * Adds a set of MapillaryImages to the object, but doesn't repaint mapView.
	 * This is needed for concurrency.
	 * 
	 * @param images
	 *            The set of images to be added.
	 */
	public synchronized void addWithoutUpdate(List<MapillaryImage> images) {
		for (MapillaryImage image : images) {
			addWithoutUpdate(image);
		}
	}

	/**
	 * Adds a MapillaryImage to the object, but doesn't repaint mapView. This is
	 * needed for concurrency.
	 * 
	 * @param image
	 *            The image to be added.
	 */
	public synchronized void addWithoutUpdate(MapillaryImage image) {
		if (!images.contains(image)) {
			this.images.add(image);
		}
	}

	/**
	 * Repaints mapView object.
	 */
	public synchronized void dataUpdated() {
		Main.map.mapView.repaint();
	}

	/**
	 * Returns a List containing all images.
	 * 
	 * @return A List object containing all images.
	 */
	public List<MapillaryImage> getImages() {
		return images;
	}

	/**
	 * Returns the MapillaryImage object that is currently selected.
	 * 
	 * @return The selected MapillaryImage object.
	 */
	public MapillaryImage getSelectedImage() {
		return selectedImage;
	}

	/**
	 * If the selected MapillaryImage is part of a MapillarySequence then the
	 * following MapillaryImage is selected. In case there is none, does
	 * nothing.
	 */
	public void selectNext() {
		if (getSelectedImage() == null)
			return;
		if (getSelectedImage().getSequence() == null)
			return;
		setSelectedImage(getSelectedImage().next());
	}

	/**
	 * If the selected MapillaryImage is part of a MapillarySequence then the
	 * previous MapillaryImage is selected. In case there is none, does nothing.
	 */
	public void selectPrevious() {
		if (getSelectedImage() == null)
			return;
		if (getSelectedImage().getSequence() == null)
			throw new IllegalStateException();
		setSelectedImage(getSelectedImage().previous());
	}

	/**
	 * Selects a new image and then starts a new MapillaryImageDownloadThread
	 * thread in order to download its surrounding thumbnails. If the
	 * user does ctrl+click, this isn't triggered.
	 * 
	 * @param image
	 *            The MapillaryImage which is going to be selected
	 */
	public void setSelectedImage(MapillaryImage image) {
		selectedImage = image;
		multiSelectedImages.clear();
		multiSelectedImages.add(image);
		if (image != null) {
			MapillaryToggleDialog.getInstance().setImage(selectedImage);
			MapillaryToggleDialog.getInstance().updateImage();
			CacheAccess<String, BufferedImageCacheEntry> prev = null;
			try {
				prev = JCSCacheManager.getCache("mapillary");
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			if (image.next() != null) {
				new MapillaryCache(image.next().getKey(),
						MapillaryCache.Type.THUMBNAIL, prev, 200000, 200000,
						new HashMap<String, String>()).submit(this, false);
				if (image.next().next() != null)
					new MapillaryCache(image.next().next().getKey(),
							MapillaryCache.Type.THUMBNAIL, prev, 200000,
							200000, new HashMap<String, String>()).submit(this,
							false);
			}
			if (image.previous() != null) {
				new MapillaryCache(image.previous().getKey(),
						MapillaryCache.Type.THUMBNAIL, prev, 200000, 200000,
						new HashMap<String, String>()).submit(this, false);
				if (image.previous().previous() != null)
					new MapillaryCache(image.previous().previous().getKey(),
							MapillaryCache.Type.THUMBNAIL, prev, 200000,
							200000, new HashMap<String, String>()).submit(this,
							false);
			}
		}
		if (Main.map != null) {
			Main.map.mapView.repaint();
		}
	}

	/**
	 * Adds a MapillaryImage object to the list of selected images, (when ctrl +
	 * click)
	 * 
	 * @param image
	 *            The MapillaryImage object to be added.
	 */
	public void addMultiSelectedImage(MapillaryImage image) {
		this.multiSelectedImages.add(image);
		Main.map.mapView.repaint();
	}

	/**
	 * Returns a list containing all MapillaryImage objects selected with ctrl +
	 * click
	 * 
	 * @return
	 */
	public List<MapillaryImage> getMultiSelectedImages() {
		return multiSelectedImages;
	}

	/**
	 * This is empty because it is used just to make sure that certain images
	 * have already been downloaded.
	 */
	@Override
	public void loadingFinished(CacheEntry data,
			CacheEntryAttributes attributes, LoadResult result) {
		// DO NOTHING
	}
}
