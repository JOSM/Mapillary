package org.openstreetmap.josm.plugins.mapillary;

import org.openstreetmap.josm.data.coor.LatLon;

/**
 * A MapillaryImage object represents each of the images stored in Mapillary.
 * 
 * @author nokutu
 * @see MapillarySequence
 * @see MapillaryData
 */
public class MapillaryImage {
	private String key;
	private LatLon latLon;
	private Double ca;
	private boolean isModified = false;
	private MapillarySequence sequence;

	/**
	 * Main contructor of the class MapillaryImage
	 * 
	 * @param key
	 *            The unique identifier of the image.
	 * @param lat
	 *            The latitude where it is positioned.
	 * @param lon
	 *            The longitude where it is positioned.
	 * @param ca
	 *            The direction of the images in degrees, meaning 0 north.
	 */
	public MapillaryImage(String key, double lat, double lon, double ca) {
		this.key = key;
		this.latLon = new LatLon(lat, lon);
		this.ca = ca;
	}

	/**
	 * Returns whether the object has been modified or not.
	 * 
	 * @return true if the object has been modified; false otherwise.
	 */
	public boolean isModified() {
		return this.isModified;
	}

	/**
	 * Returns a LatLon object containing the coordintes of the object.
	 * 
	 * @return The LatLon object with the position of the object.
	 */
	public LatLon getLatLon() {
		return latLon;
	}

	/**
	 * Returns the direction towards the image has been taken.
	 * 
	 * @return The direction of the image (0 means north and goes clockwise).
	 */
	public Double getCa() {
		return ca;
	}

	/**
	 * Returns the unique identifier of the object.
	 * 
	 * @return A String containing the unique identifier of the object.
	 */
	public String getKey() {
		return this.key;
	}

	/**
	 * Sets the MapillarySequence object which contains the MapillaryImage.
	 * 
	 * @param sequence
	 *            The MapillarySequence that contains the MapillaryImage.
	 */
	public void setSequence(MapillarySequence sequence) {
		this.sequence = sequence;
	}

	/**
	 * Returns the sequence which contains this image.
	 * 
	 * @return The MapillarySequence object that contains this MapillaryImage.
	 */
	public MapillarySequence getSequence() {
		return this.sequence;
	}

	public String toString() {
		return "Image[key=" + this.key + ";lat=" + this.latLon.lat() + ";lon="
				+ this.latLon.lon() + ";ca=" + this.ca + "]";
	}

	@Override
	public boolean equals(Object image) {
		if (image instanceof MapillaryImage)
			return this.key.equals(((MapillaryImage) image).getKey());
		return false;
	}

	/**
	 * If the MapillaryImage belongs to a MapillarySequence, returns the next
	 * MapillarySequence in it.
	 * 
	 * @return The following MapillaryImage, or null if there is none.
	 */
	public MapillaryImage next() {
		if (this.getSequence() == null)
			return null;
		return this.getSequence().next(this);
	}

	/**
	 * If the MapillaryImage belongs to a MapillarySequence, returns the
	 * previous MapillarySequence in it.
	 * 
	 * @return The previous MapillaryImage, or null if there is none.
	 */
	public MapillaryImage previous() {
		if (this.getSequence() == null)
			return null;
		return this.getSequence().previous(this);
	}
}
