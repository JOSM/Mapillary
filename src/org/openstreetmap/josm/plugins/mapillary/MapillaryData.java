package org.openstreetmap.josm.plugins.mapillary;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.data.cache.CacheEntry;
import org.openstreetmap.josm.data.cache.CacheEntryAttributes;
import org.openstreetmap.josm.data.cache.ICachedLoaderListener;
import org.openstreetmap.josm.plugins.mapillary.cache.MapillaryCache;

import java.util.ArrayList;
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

	private final List<MapillaryAbstractImage> images;
	private MapillaryAbstractImage selectedImage;
	private final List<MapillaryAbstractImage> multiSelectedImages;

	private List<MapillaryDataListener> listeners = new ArrayList<>();

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
	public synchronized void add(List<MapillaryAbstractImage> images) {
		for (MapillaryAbstractImage image : images) {
			add(image);
		}
	}

	/**
	 * Adds an MapillaryImage to the object, and then repaints mapView.
	 * 
	 * @param image
	 *            The image to be added.
	 */
	public synchronized void add(MapillaryAbstractImage image) {
		if (!images.contains(image)) {
			this.images.add(image);
		}
		dataUpdated();
	}

	public void addListener(MapillaryDataListener lis) {
		listeners.add(lis);
	}

	public void removeListener(MapillaryDataListener lis) {
		listeners.remove(lis);
	}

	/**
	 * Adds a set of MapillaryImages to the object, but doesn't repaint mapView.
	 * This is needed for concurrency.
	 * 
	 * @param images
	 *            The set of images to be added.
	 */
	public synchronized void addWithoutUpdate(
			List<MapillaryAbstractImage> images) {
		for (MapillaryAbstractImage image : images) {
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
	public synchronized void addWithoutUpdate(MapillaryAbstractImage image) {
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
	public List<MapillaryAbstractImage> getImages() {
		return images;
	}

	/**
	 * Returns the MapillaryImage object that is currently selected.
	 * 
	 * @return The selected MapillaryImage object.
	 */
	public MapillaryAbstractImage getSelectedImage() {
		return selectedImage;
	}

	/**
	 * If the selected MapillaryImage is part of a MapillarySequence then the
	 * following MapillaryImage is selected. In case there is none, does
	 * nothing.
	 */
	public void selectNext() {
		if (getSelectedImage() instanceof MapillaryImage) {
			if (getSelectedImage() == null)
				return;
			if (((MapillaryImage) getSelectedImage()).getSequence() == null)
				return;
			setSelectedImage(((MapillaryImage) getSelectedImage()).next(), true);
		}
	}

	/**
	 * If the selected MapillaryImage is part of a MapillarySequence then the
	 * previous MapillaryImage is selected. In case there is none, does nothing.
	 */
	public void selectPrevious() {
		if (getSelectedImage() instanceof MapillaryImage) {
			if (getSelectedImage() == null)
				return;
			if (((MapillaryImage) getSelectedImage()).getSequence() == null)
				throw new IllegalStateException();
			setSelectedImage(((MapillaryImage) getSelectedImage()).previous(),
					true);
		}
	}

	/**
	 * Selects a new image and then starts a new MapillaryImageDownloadThread
	 * thread in order to download its surrounding thumbnails. If the user does
	 * ctrl+click, this isn't triggered.
	 * 
	 * @param image
	 *            The MapillaryImage which is going to be selected
	 */
	public void setSelectedImage(MapillaryAbstractImage image) {
		setSelectedImage(image, false);
	}

	/**
	 * Selects a new image and then starts a new MapillaryImageDownloadThread
	 * thread in order to download its surrounding thumbnails. If the user does
	 * ctrl+click, this isn't triggered. You can choose wheter to center the
	 * view on the new image or not.
	 * 
	 * @param image
	 * @param zoom
	 */
	public void setSelectedImage(MapillaryAbstractImage image, boolean zoom) {
		MapillaryAbstractImage oldImage = selectedImage;
		selectedImage = image;
		multiSelectedImages.clear();
		multiSelectedImages.add(image);
		if (image != null) {
			if (image instanceof MapillaryImage) {
				MapillaryImage mapillaryImage = (MapillaryImage) image;
				if (mapillaryImage.next() != null) {
					new MapillaryCache(mapillaryImage.next().getKey(),
							MapillaryCache.Type.THUMBNAIL).submit(this, false);
					if (mapillaryImage.next().next() != null)
						new MapillaryCache(mapillaryImage.next().next()
								.getKey(), MapillaryCache.Type.THUMBNAIL)
								.submit(this, false);
				}
				if (mapillaryImage.previous() != null) {
					new MapillaryCache(mapillaryImage.previous().getKey(),
							MapillaryCache.Type.THUMBNAIL).submit(this, false);
					if (mapillaryImage.previous().previous() != null)
						new MapillaryCache(mapillaryImage.previous().previous()
								.getKey(), MapillaryCache.Type.THUMBNAIL)
								.submit(this, false);
				}
			}
		}
		if (zoom)
			Main.map.mapView.zoomTo(MapillaryData.getInstance()
					.getSelectedImage().getLatLon());
		if (Main.map != null) {
			Main.map.mapView.repaint();
		}
		fireSelectedImageChanged(oldImage, selectedImage);
	}

	private void fireSelectedImageChanged(MapillaryAbstractImage oldImage, MapillaryAbstractImage newImage) {
		if (listeners.isEmpty())
			return;
		for (MapillaryDataListener lis : listeners)
			lis.selectedImageChanged(oldImage, newImage);
	}

	/**
	 * Adds a MapillaryImage object to the list of selected images, (when ctrl +
	 * click)
	 * 
	 * @param image
	 *            The MapillaryImage object to be added.
	 */
	public void addMultiSelectedImage(MapillaryAbstractImage image) {
		if (!this.multiSelectedImages.contains(image)) {
			if (this.getSelectedImage() != null)
				this.multiSelectedImages.add(image);
			else
				this.setSelectedImage(image);
		}
		Main.map.mapView.repaint();
	}

	/**
	 * Adds a set of MapillaryImage objects to the list of selected images.
	 * 
	 * @param images
	 */
	public void addMultiSelectedImage(List<MapillaryAbstractImage> images) {
		for (MapillaryAbstractImage image : images)
			if (!this.multiSelectedImages.contains(image)) {
				if (this.getSelectedImage() != null)
					this.multiSelectedImages.add(image);
				else
					this.setSelectedImage(image);
			}
		Main.map.mapView.repaint();
	}

	/**
	 * Returns a list containing all MapillaryImage objects selected with ctrl +
	 * click
	 * 
	 * @return
	 */
	public List<MapillaryAbstractImage> getMultiSelectedImages() {
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
