package org.openstreetmap.josm.plugins.mapillary;

import java.util.ArrayList;
import java.util.List;

/**
 * Class that stores a sequence of MapillaryImage objects.
 * 
 * @author nokutu
 * @see MapillaryImage
 *
 */
public class MapillarySequence {
	private final List<MapillaryImage> images;
	private final String key;
	private final int created_at;

	public MapillarySequence(String key, int created_at) {
		this.images = new ArrayList<>();
		this.key = key;
		this.created_at = created_at;
	}


	/**
	 * Returns all MapillaryImages objects contained by this object.
	 * 
	 * @return
	 */
	public List<MapillaryImage> getImages() {
		return this.images;
	}
	

	public int getCreatedAt() {
		return created_at;
	}

	/**
	 * Adds a new MapillaryImage object to this object.
	 * 
	 * @param image
	 */
	public synchronized void add(MapillaryImage image) {
		this.images.add(image);
	}
	
	public String getKey() {
		return this.key;
	}

	/**
	 * Adds a set of MapillaryImage objects to this object.
	 * 
	 * @param images
	 */
	public synchronized void add(List<MapillaryImage> images) {
		for (MapillaryImage image : images) {
			add(image);
		}
	}

	/**
	 * Removes a MapillaryImage object from this object.
	 * 
	 * @param image
	 */
	public void remove(MapillaryImage image) {
		this.images.remove(image);
	}

	/**
	 * Returns the next MapillaryImage in the sequence.
	 * 
	 * @param image
	 * @return
	 */
	public MapillaryImage next(MapillaryImage image) {
		if (!images.contains(image))
			throw new IllegalArgumentException();
		int i = images.indexOf(image);
		if (i == images.size() - 1) {
			return null;
		} else
			return images.get(i + 1);
	}

	/**
	 * Returns the previous MapillaryImage in the sequence.
	 * 
	 * @param image
	 * @return
	 */
	public MapillaryImage previous(MapillaryImage image) {
		if (!images.contains(image))
			throw new IllegalArgumentException();
		int i = images.indexOf(image);
		if (i == 0) {
			return null;
		} else
			return images.get(i - 1);
	}

	/**
	 * Returns the difference of index between two MapillaryImage objects
	 * belonging to the same MapillarySequence.
	 * 
	 * @param image1
	 * @param image2
	 * @return
	 */
	public int getDistance(MapillaryImage image1, MapillaryImage image2) {
		if (!this.images.contains(image1) || !this.images.contains(image2))
			throw new IllegalArgumentException();
		return Math.abs(this.images.indexOf(image1)
				- this.images.indexOf(image2));
	}
	
	public boolean equals(MapillarySequence sequence) {
		return this.getKey() == sequence.getKey();
	}
}
