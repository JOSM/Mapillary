// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary;

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.Locale;

import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.tools.date.DateUtils;

/**
 * Abstract superclass for all image objects. At the moment there are just 2,
 * {@link MapillaryImportedImage} and {@link MapillaryImage}.
 *
 * @author nokutu
 *
 */
public abstract class MapillaryAbstractImage implements Comparable<MapillaryAbstractImage> {
  /**
   * If two values for field ca differ by less than EPSILON both values are considered equal.
   */
  private static final float EPSILON = 1e-5f;

  /** The time the image was captured, in Epoch format. */
  protected long capturedAt;
  /** Sequence of pictures containing this object. */
  private MapillarySequence sequence;
  /** Position of the picture. */
  protected LatLon latLon;
  /** Direction of the picture. */
  protected double ca;
  /** Temporal position of the picture until it is uploaded. */
  private LatLon tempLatLon;

  private final boolean pano;

  /**
   * When the object is being dragged in the map, the temporal position is
   * stored here.
   */
  private LatLon movingLatLon;
  /** Temporal direction of the picture until it is uploaded */
  private double tempCa;
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
   * @param latLon  The latitude and longitude where the picture was taken.
   * @param ca  The direction of the picture (0 means north).
   */
  protected MapillaryAbstractImage(final LatLon latLon, final double ca, final boolean pano) {
    this.latLon = latLon;
    this.tempLatLon = this.latLon;
    this.movingLatLon = this.latLon;
    this.ca = ca;
    this.tempCa = ca;
    this.movingCa = ca;
    this.pano = pano;
    this.visible = true;
  }

  /**
   * Returns the original direction towards the image has been taken.
   *
   * @return The direction of the image (0 means north and goes clockwise).
   */
  public double getCa() {
    return this.ca;
  }

  /**
   * Returns the Epoch time when the image was captured.
   *
   * @return The long containing the Epoch time when the image was captured.
   */
  public long getCapturedAt() {
    return this.capturedAt;
  }

  public boolean isPanorama() {
    return this.pano;
  }

  /**
   * Returns the date the picture was taken in DMY format.
   *
   * @return A String object containing the date when the picture was taken.
   */
  public String getDate() {
    final StringBuilder format = new StringBuilder(26);
    if (DateUtils.PROP_ISO_DATES.get()) {
      format.append("yyyy-MM-dd");
    } else {
      format.append("dd/MM/yyyy");
    }
    if (MapillaryProperties.DISPLAY_HOUR.get()) {
      if (MapillaryProperties.TIME_FORMAT_24.get()) {
        format.append(" - HH:mm:ss (z)");
      } else {
        format.append(" - h:mm:ss a (z)");
      }
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
   * @throws NullPointerException if parameter format is <code>null</code>
   */
  public String getDate(String format) {
    final Date date = new Date(getCapturedAt());
    final SimpleDateFormat formatter = new SimpleDateFormat(format, Locale.US);
    formatter.setTimeZone(Calendar.getInstance().getTimeZone());
    return formatter.format(date);
  }

  /**
  * Returns a LatLon object containing the original coordinates of the object.
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
  public double getMovingCa() {
    return this.movingCa;
  }

  /**
   * Returns a LatLon object containing the current coordinates of the object.
   * When you are dragging the image this changes.
   *
   * @return The LatLon object with the position of the object.
   */
  public LatLon getMovingLatLon() {
    return movingLatLon;
  }

  /**
   * Returns the sequence which contains this image.
   * Never null.
   *
   * @return The MapillarySequence object that contains this MapillaryImage.
   */
  public MapillarySequence getSequence() {
    synchronized (this) {
      if (sequence == null) {
        sequence = new MapillarySequence();
        sequence.add(this);
      }
      return this.sequence;
    }
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
    return !this.getMovingLatLon().equals(this.latLon) || Math.abs(this.getMovingCa() - this.ca) > EPSILON;
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
   * @param lonDelta The movement of the image in longitude units.
   * @param latDelta The movement of the image in latitude units.
   */
  public void move(final double lonDelta, final double latDelta) {
    this.movingLatLon = new LatLon(this.tempLatLon.getY() + latDelta, this.tempLatLon.getX() + lonDelta);
  }

  /**
   * If the MapillaryImage belongs to a MapillarySequence, returns the next
   * image in the sequence.
   *
   * @return The following MapillaryImage, or null if there is none.
   */
  public MapillaryAbstractImage next() {
    synchronized (this) {
      return getSequence().next(this);
    }
  }

  /**
   * If the MapillaryImage belongs to a MapillarySequence, returns the previous
   * image in the sequence.
   *
   * @return The previous MapillaryImage, or null if there is none.
   */
  public MapillaryAbstractImage previous() {
    synchronized (this) {
      return getSequence().previous(this);
    }
  }

  public void setCa(final double ca) {
    this.ca = ca;
  }

  /**
   * Sets the Epoch time when the picture was captured.
   *
   * @param capturedAt Epoch time when the image was captured.
   */
  public void setCapturedAt(final long capturedAt) {
    this.capturedAt = capturedAt;
  }

  public void setLatLon(final LatLon latLon) {
    if (latLon != null) {
      this.latLon = latLon;
    }
  }

  /**
   * Sets the MapillarySequence object which contains the MapillaryImage.
   *
   * @param sequence The MapillarySequence that contains the MapillaryImage.
   * @throws IllegalArgumentException if the image is not already part of the {@link MapillarySequence}.
   *   Call {@link MapillarySequence#add(MapillaryAbstractImage)} first.
   */
  public void setSequence(final MapillarySequence sequence) {
    synchronized (this) {
      if (sequence != null && !sequence.getImages().contains(this)) {
        throw new IllegalArgumentException();
      }
      this.sequence = sequence;
    }
  }

  /**
   * Set's whether the image should be visible on the map or not.
   *
   * @param visible true if the image is set to be visible; false otherwise.
   */
  public void setVisible(final boolean visible) {
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
   * @param ca The angle the image is moving.
   */
  public void turn(final double ca) {
    this.movingCa = this.tempCa + ca;
  }
}
