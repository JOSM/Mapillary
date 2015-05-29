package org.openstreetmap.josm.plugins.mapillary;

import org.openstreetmap.josm.Main;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * Database class for all the MapillaryImage objects.
 * 
 * @author nokutu
 *
 */
public class MapillaryData {
	public volatile static MapillaryData INSTANCE;
	
	private final List<MapillaryImage> images;
	private MapillaryImage selectedImage = null;
	private List<MapillaryImage> multiSelectedImages;


	public MapillaryData() {
		images = new CopyOnWriteArrayList<>();
		multiSelectedImages = new ArrayList<>();
	}

	public static MapillaryData getInstance() {
		if (INSTANCE == null) {
			INSTANCE = new MapillaryData();
		}
		return INSTANCE;
	}

	public static void deleteInstance() {
		INSTANCE = null;
	}

	public MapillaryData(List<MapillaryImage> images) {
		this.images = images;
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
			throw new IllegalStateException();
		if (getSelectedImage().next() != null)
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
		if (getSelectedImage().previous() != null)
			setSelectedImage(getSelectedImage().previous());
	}

	/**
	 * Selects a new image and then starts a new MapillaryImageDownloadThread
	 * thread in order to download its surrounding thumbnails and images.
	 * 
	 * @param image
	 *            The MapillaryImage which is going to be selected
	 * @param clearThumbnail
	 *            Whether if the old selected MapillaryImage's thumbnail should
	 *            be deleted or not.
	 * @param clearImageWhether
	 *            if the old selected MapillaryImage's full resolution image
	 *            should be deleted or not.
	 */
	public void setSelectedImage(MapillaryImage image) {
		selectedImage = image;
		multiSelectedImages.clear();
		multiSelectedImages.add(image);
		if (image != null) {
			MapillaryToggleDialog.getInstance().setImage(selectedImage);
			MapillaryToggleDialog.getInstance().updateImage();
		}
		if (Main.map != null) {
			Main.map.mapView.repaint();
		}
	}
	
	public void addMultiSelectedImage(MapillaryImage image) {
		this.multiSelectedImages.add(image);
		Main.map.mapView.repaint();
	}
	
	public List<MapillaryImage> getMultiSelectedImages(){
		return multiSelectedImages;
	}
}
