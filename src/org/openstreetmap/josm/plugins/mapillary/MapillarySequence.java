package org.openstreetmap.josm.plugins.mapillary;

import java.util.ArrayList;
import java.util.List;

/**
 * Class that stores a sequence of MapillaryImage objects.
 * 
 * @author nokutu
 *
 */
public class MapillarySequence {
	private final List<MapillaryImage> images;

	public MapillarySequence() {
		this.images = new ArrayList<>();
	}

	public MapillarySequence(List<MapillaryImage> images) {
		this.images = images;
	}

	public List<MapillaryImage> getImages() {
		return this.images;
	}

	public synchronized void add(MapillaryImage image) {
		this.images.add(image);
	}

	public synchronized void add(List<MapillaryImage> images) {
		for (MapillaryImage image : images) {
			add(image);
		}
	}

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

	public int getDistance(MapillaryImage image1, MapillaryImage image2) {
		if (!this.images.contains(image1) || !this.images.contains(image2))
			throw new IllegalArgumentException();
		return Math.abs(this.images.indexOf(image1)
				- this.images.indexOf(image2));
	}
}
