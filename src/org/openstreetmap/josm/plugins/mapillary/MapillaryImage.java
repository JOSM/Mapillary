package org.openstreetmap.josm.plugins.mapillary;


import org.openstreetmap.josm.data.coor.LatLon;

/**
 * A MapillaryImage object represents each of the images stored in Mapillary.
 * 
 * @author nokutu
 *
 */
public class MapillaryImage {
	private String key;
	private LatLon latLon;

	/**
	 * 0 means north.
	 */
	private Double ca;
	private boolean isModified = false;

	/**
	 * Used to prevent old running threads from setting images in an object
	 * which are no longer needed.
	 */

	public MapillarySequence sequence;

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

	public boolean isModified() {
		return this.isModified;
	}

	public LatLon getLatLon() {
		return latLon;
	}

	public Double getCa() {
		return ca;
	}

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


	public MapillarySequence getSequence() {
		return this.sequence;
	}

	public String toString() {
		return "Image[key=" + this.key + ";lat=" + this.latLon.lat() + ";lon="
				+ this.latLon.lon() + ";ca=" + this.ca + "]";
	}

	public Boolean equals(MapillaryImage image) {
		return this.key.equals(image.getKey());
	}

	/**
	 * If the MapillaryImage belongs to a MapillarySequence, returns the next
	 * MapillarySequence in it.
	 * 
	 * @return The following MapillaryImage, or null if there is none.
	 */
	public MapillaryImage next() {
		return this.getSequence().next(this);
	}

	/**
	 * If the MapillaryImage belongs to a MapillarySequence, returns the
	 * previous MapillarySequence in it.
	 * 
	 * @return The previous MapillaryImage, or null if there is none.
	 */
	public MapillaryImage previous() {
		return this.getSequence().previous(this);
	}
}
