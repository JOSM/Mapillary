// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary;

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.data.coor.LatLon;

/**
 * Abstract superclass for all image objects. At the moment there are just 2,
 * {@link MapillaryImportedImage} and {@link MapillaryImage}.
 *
 * @author nokutu
 *
 */
public abstract class MapillaryAbstractImage {

  /** The time the image was captured, in Epoch format. */
  protected long capturedAt;
  /** Sequence of pictures containing this object. */
  private MapillarySequence sequence;
  /** Position of the picture. */
  public final LatLon latLon;
  /** Direction of the picture. */
  public final double ca;
  /** Temporal position of the picture until it is uploaded. */
  public LatLon tempLatLon;
  /**
   * When the object is being dragged in the map, the temporal position is
   * stored here.
   */
  public LatLon movingLatLon;
  /** Temporal direction of the picture until it is uploaded */
  public double tempCa;
  /**
   * When the object direction is being moved in the map, the temporal direction
   * is stored here
   */
  protected double movingCa;
  /** Whether the image must be drown in the map or not */
  private boolean visible;

  /**
   * Creates a new object in the given position and with the given direction.
   *
   * @param lat
   *          The latitude where the picture was taken.
   * @param lon
   *          The longitude where the picture was taken.
   * @param ca
   *          The direction of the picture (0 means north).
   */
  public MapillaryAbstractImage(double lat, double lon, double ca) {
    this.latLon = new LatLon(lat, lon);
    this.tempLatLon = this.latLon;
    this.movingLatLon = this.latLon;
    this.ca = ca;
    this.tempCa = ca;
    this.movingCa = ca;
    this.visible = true;
  }

  /**
   * Returns the direction towards the image has been taken.
   *
   * @return The direction of the image (0 means north and goes clockwise).
   */
  public double getCa() {
    return this.movingCa;
  }

  /**
   * Returns the Epoch time when the image was captured.
   *
   * @return The long containing the Epoch time when the image was captured.
   */
  public long getCapturedAt() {
    return this.capturedAt;
  }

  /**
   * Returns the date the picture was taken in DMY format.
   *
   * @return A String object containing the date when the picture was taken.
   */
  public String getDate() {
    StringBuilder format = new StringBuilder("");
    if (Main.pref.getBoolean("iso.dates"))
      format.append("yyyy-MM-dd");
    else
      format.append("dd/MM/yyyy");
    if (Main.pref.getBoolean("mapillary.display-hour", true)) {
      if (Main.pref.getBoolean("mapillary.format-24"))
        format.append(" - HH:mm:ss (z)");
      else
        format.append(" - h:mm:ss a (z)");
    }
    return getDate(format.toString());
  }

  /**
   * Returns the date the picture was taken in the given format.
   *
   * @param format
   *          Format of the date. See {@link SimpleDateFormat}.
   * @return A String containing the date the picture was taken using the given
   *         format.
   */
  public String getDate(String format) {
    Date date = new Date(getCapturedAt());

    SimpleDateFormat formatter = new SimpleDateFormat(format);
    formatter.setTimeZone(Calendar.getInstance().getTimeZone());
    return formatter.format(date);
  }

  /**
   * Returns a LatLon object containing the current coordinates of the object.
   * When you are dragging the image this changes.
   *
   * @return The LatLon object with the position of the object.
   */
  public LatLon getLatLon() {
    return this.movingLatLon;
  }

  /**
   * Returns the sequence which contains this image.
   *
   * @return The MapillarySequence object that contains this MapillaryImage.
   */
  public MapillarySequence getSequence() {
    if (this.sequence == null) {
      this.sequence = new MapillarySequence();
      this.sequence.add(this);
    }

    return this.sequence;
  }

  /**
   * Returns the last fixed direction of the object.
   *
   * @return The last fixed direction of the object. 0 means north.
   */
  public double getTempCa() {
    return this.tempCa;
  }

  /**
   * Returns the last fixed coordinates of the object.
   *
   * @return A LatLon object containing.
   */
  public LatLon getTempLatLon() {
    return this.tempLatLon;
  }

  /**
   * Returns whether the object has been modified or not.
   *
   * @return true if the object has been modified; false otherwise.
   */
  public boolean isModified() {
    return (this.getLatLon() != this.latLon || this.getCa() != this.ca);
  }

  /**
   * Returns whether the image is visible on the map or not.
   *
   * @return True if the image is visible; false otherwise.
   */
  public boolean isVisible() {
    return this.visible;
  }

  /**
   * Moves the image temporally to another position
   *
   * @param x
   *          The movement of the image in longitude units.
   * @param y
   *          The movement of the image in latitude units.
   */
  public void move(double x, double y) {
    this.movingLatLon = new LatLon(this.tempLatLon.getY() + y,
        this.tempLatLon.getX() + x);
  }

  /**
   * If the MapillaryImage belongs to a MapillarySequence, returns the next
   * MapillarySequence in it.
   *
   * @return The following MapillaryImage, or null if there is none.
   */
  public MapillaryAbstractImage next() {
    synchronized (this.getClass()) {
      if (this.getSequence() == null)
        return null;
      return this.getSequence().next(this);
    }
  }

  /**
   * If the MapillaryImage belongs to a MapillarySequence, returns the previous
   * MapillarySequence in it.
   *
   * @return The previous MapillaryImage, or null if there is none.
   */
  public MapillaryAbstractImage previous() {
    synchronized (this.getClass()) {
      if (this.getSequence() == null)
        return null;
      return this.getSequence().previous(this);
    }
  }

  /**
   * Sets the Epoch time when the picture was captured.
   *
   * @param capturedAt
   *          Epoch time when the image was captured.
   */
  public void setCapturedAt(long capturedAt) {
    this.capturedAt = capturedAt;
  }

  /**
   * Sets the MapillarySequence object which contains the MapillaryImage.
   *
   * @param sequence
   *          The MapillarySequence that contains the MapillaryImage.
   */
  public void setSequence(MapillarySequence sequence) {
    this.sequence = sequence;
  }

  /**
   * Set's whether the image should be visible on the map or not.
   *
   * @param visible
   *          true if the image is set to be visible; false otherwise.
   */
  public void setVisible(boolean visible) {
    this.visible = visible;
  }

  /**
   * Called when the mouse button is released, meaning that the picture has
   * stopped being dragged, so the temporal values are saved.
   */
  public void stopMoving() {
    this.tempLatLon = this.movingLatLon;
    this.tempCa = this.movingCa;
  }

  /**
   * Turns the image direction.
   *
   * @param ca
   *          The angle the image is moving.
   */
  public void turn(double ca) {
    this.movingCa = this.tempCa + ca;
  }
}
