package org.openstreetmap.josm.plugins.mapillary;

import java.util.ArrayList;
import java.util.List;

/**
 * A MapillaryImage object represents each of the images stored in Mapillary.
 * 
 * @author nokutu
 * @see MapillarySequence
 * @see MapillaryData
 */
public class MapillaryImage extends MapillaryAbstractImage {
	/** Unique identifier of the object */
	private final String key;
	/** Sequence of pictures containing this object */
	private MapillarySequence sequence;

	/** Epoch time when the image was taken. */
	/** The user that made the image */
	private String user;
	/** Set of traffic signs in the image */
	private List<String> signs;
	private String location;

	public String getLocation() {
		return location;
	}

	public void setLocation(String location) {
		this.location = location;
	}

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
		super(lat, lon, ca);
		this.key = key;
		this.signs = new ArrayList<>();
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
	 * Adds a new sign to the set of signs.
	 * 
	 * @param sign
	 */
	public void addSign(String sign) {
		signs.add(sign);
	}

	public List<String> getSigns() {
		return signs;
	}

	public void setUser(String user) {
		this.user = user;
	}

	public String getUser() {
		return user;
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

	@Override
	public boolean equals(Object object) {
		if (object instanceof MapillaryImage)
			return this.key.equals(((MapillaryImage) object).getKey());
		return false;
	}

	@Override
	public int hashCode() {
		return this.key.hashCode();
	}
}
