package org.openstreetmap.josm.plugins.mapillary;

import org.openstreetmap.josm.data.coor.LatLon;

/**
 * Abstract supperclass for all image objects. At the moment there is just 2,
 * {@code MapillaryImportedImage} and {@code MapillaryImage}.
 * 
 * @author nokutu
 *
 */
public abstract class MapillaryAbstractImage {

	/** Postion of the picture */
	public final LatLon latLon;
	/** Direction of the picture */
	public final double ca;
	public boolean isModified = false;
	/** Temporal position of the picture until it is uplaoded */
	public LatLon tempLatLon;
	/**
	 * When the object is being dragged in the map, the temporal position is
	 * stored here
	 */
	public LatLon movingLatLon;
	/** Temporal direction of the picture until it is uplaoded */
	public double tempCa;
	/**
	 * When the object direction is being moved in the map, the temporal
	 * direction is stored here
	 */
	protected double movingCa;

	public MapillaryAbstractImage(double lat, double lon, double ca) {
		this.latLon = new LatLon(lat, lon);
		this.tempLatLon = this.latLon;
		this.movingLatLon = this.latLon;
		this.ca = ca;
		this.tempCa = ca;
		this.movingCa = ca;
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
		return movingLatLon;
	}

	public LatLon getTempLatLon() {
		return tempLatLon;
	}

	/**
	 * Moves the image temporally to another position
	 * 
	 * @param pos
	 */
	public void move(double x, double y) {
		this.movingLatLon = new LatLon(this.tempLatLon.getY() + y,
				this.tempLatLon.getX() + x);
		this.isModified = true;
	}

	/**
	 * Turns the image direction.
	 * 
	 * @param ca
	 */
	public void turn(double ca) {
		this.movingCa = this.tempCa + ca;
		this.isModified = true;
	}

	/**
	 * Called when the mouse button is released, meaning that the picture has
	 * stopped being dragged.
	 */
	public void stopMoving() {
		this.tempLatLon = this.movingLatLon;
		this.tempCa = this.movingCa;
	}

	/**
	 * Returns the direction towards the image has been taken.
	 * 
	 * @return The direction of the image (0 means north and goes clockwise).
	 */
	public double getCa() {
		return movingCa;
	}

	/**
	 * Returns the last fixed direction of the object.
	 * 
	 * @return
	 */
	public double getTempCa() {
		return tempCa;
	}
}