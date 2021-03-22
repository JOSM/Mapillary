// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.data.image;

import static java.lang.Integer.compare;
import static java.lang.Long.compare;

import java.awt.Color;
import java.awt.Image;
import java.io.Serializable;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;

import javax.swing.ImageIcon;

import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.data.gpx.GpxImageEntry;
import org.openstreetmap.josm.data.osm.TagMap;
import org.openstreetmap.josm.plugins.mapillary.io.download.MapillaryDownloader;
import org.openstreetmap.josm.plugins.mapillary.utils.LocalDateConverter;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.tools.ImageProvider;

/**
 * Abstract superclass for all image objects. At the moment there are just 2,
 * {@link MapillaryImportedImage} and {@link MapillaryImage}.
 *
 * @author nokutu
 */
public abstract class MapillaryAbstractImage extends GpxImageEntry implements MapillaryTagged, Serializable {
  /** The common directory for the Mapillary image sprites (for Mapillary Images) */
  protected static final String IMAGE_SPRITE_DIR = "josm-ca";
  /** The default sprite for a Mapillary image */
  protected static final ImageIcon DEFAULT_SPRITE = new ImageProvider(IMAGE_SPRITE_DIR, "default-ca")
    .setMaxWidth(ImageProvider.ImageSizes.MAP.getAdjustedHeight()).get();
  /** The sprite to use for the active Mapillary sequence */
  public static final ImageIcon ACTIVE_SEQUENCE_SPRITE = new ImageProvider(IMAGE_SPRITE_DIR, "sequence-ca")
    .setMaxWidth(ImageProvider.ImageSizes.MAP.getAdjustedHeight()).get();
  /** The sprite to use for the currently selected image */
  public static final ImageIcon SELECTED_IMAGE = new ImageProvider(IMAGE_SPRITE_DIR, "current-ca")
    .setMaxWidth(ImageProvider.ImageSizes.MAP.getAdjustedHeight()).get();
  /** The sprite to use for images marked as deleted */
  public static final ImageIcon DELETED_IMAGE = new ImageProvider(IMAGE_SPRITE_DIR, "deleted-ca")
    .setMaxWidth(ImageProvider.ImageSizes.MAP.getAdjustedHeight()).get();

  /**
   * If two values for field ca differ by less than EPSILON both values are considered equal.
   */
  private static final float EPSILON = 1e-5f;

  /** The time the image was captured, in Epoch format. */
  protected long capturedAt;
  /** Sequence of pictures containing this object. */
  private MapillarySequence sequence;
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
   * Indicates whether or not this image has been reviewed for Uploading/Changeset submission.
   */
  private boolean reviewed;
  /** The quality score for the image. May not be present. */
  private int qualityScore = Integer.MIN_VALUE;
  private boolean deleted;
  private final TagMap tags = new TagMap();

  /**
   * Creates a new object in the given position and with the given direction.
   *
   * @param latLon The latitude and longitude where the picture was taken.
   * @param ca The direction of the picture (0 means north).
   * @param pano The property to indicate whether image is panorama or not.
   */
  protected MapillaryAbstractImage(final LatLon latLon, final double ca, final boolean pano) {
    Objects.requireNonNull(latLon, "Initial LatLon cannot be null");
    super.setPos(latLon);
    super.setExifCoor(latLon);
    this.tempLatLon = latLon;
    this.movingLatLon = latLon;
    super.setExifImgDir(ca);
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
    return getDate(getDateFormat());
  }

  /**
   * Returns the date format used for display
   *
   * @return The date format (see {@link SimpleDateFormat}).
   */
  public String getDateFormat() {
    final StringBuilder format = new StringBuilder(26);
    format.append(LocalDateConverter.getDateFormat());
    if (Boolean.TRUE.equals(MapillaryProperties.DISPLAY_HOUR.get())) {
      if (Boolean.TRUE.equals(MapillaryProperties.TIME_FORMAT_24.get())) {
        format.append(" - HH:mm:ss (z)");
      } else {
        format.append(" - h:mm:ss a (z)");
      }
    }
    return format.toString();
  }

  /**
   * Returns the date the picture was taken in the given format.
   *
   * @param format
   *        Format of the date. See {@link SimpleDateFormat}.
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
    return this.getPos();
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
  public synchronized MapillarySequence getSequence() {
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
    return this.getMovingLatLon() != null && !this.getMovingLatLon().equals(this.getExifCoor())
      || Math.abs(this.getMovingCa() - this.ca) > EPSILON;
  }

  /**
   * Returns whether the image is visible on the map or not.
   *
   * @return True if the image is visible; false otherwise.
   */
  public boolean isVisible() {
    return this.visible && !this.isDeleted();
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
      if (this.getSequence() != null) {
        return getSequence().next(this);
      }
      return null;
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
      if (this.getSequence() != null) {
        return getSequence().previous(this);
      }
      return null;
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
    this.setExifTime(new Date(capturedAt));
  }

  @Override
  public void setExifCoor(final LatLon latLon) {
    super.setExifCoor(latLon);
    if (this.movingLatLon == null) {
      this.movingLatLon = latLon;
    }
    if (this.tempLatLon == null) {
      this.tempLatLon = latLon;
    }
  }

  public void setLatLon(final LatLon latLon) {
    if (latLon != null) {
      setExifCoor(latLon);
    }
  }

  /**
   * Sets the MapillarySequence object which contains the MapillaryImage.
   *
   * @param sequence The MapillarySequence that contains the MapillaryImage.
   * @throws IllegalArgumentException if the image is not already part of the {@link MapillarySequence}.
   *         Call {@link MapillarySequence#add(MapillaryAbstractImage)} first.
   */
  public void setSequence(final MapillarySequence sequence) {
    synchronized (this) {
      if (sequence != null && !sequence.getImages().contains(this)
        && MapillaryDownloader.downloadSequences(true, sequence.getKey()).isEmpty()) {
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

  public boolean isReviewed() {
    return reviewed;
  }

  public void setReviewed(boolean reviewed) {
    this.reviewed = reviewed;
  }

  public abstract Color paintHighlightedAngleColour();

  public abstract Color paintSelectedAngleColour();

  public abstract Color paintUnselectedAngleColour();

  /**
   * @return The default image to represent this particular type of image
   */
  public abstract Image getDefaultImage();

  /**
   * @return The default image to indicate that this particular image is selected
   */
  public Image getSelectedImage() {
    return SELECTED_IMAGE.getImage();
  }

  /**
   * @return The image to indicate that the current sequence is active
   */
  public Image getActiveSequenceImage() {
    return ACTIVE_SEQUENCE_SPRITE.getImage();
  }

  /**
   * @return The default image to indicate that this particular image is marked deleted.
   */
  public Image getDeletedImage() {
    return DELETED_IMAGE.getImage();
  }

  public void setMovingCa(double ca) {
    double tCa = ca;
    if (ca > 360) {
      tCa = ca - 360;
    } else if (ca < 0) {
      tCa = ca + 360;
    }
    this.movingCa = tCa;
  }

  public void setMovingLatLon(LatLon latLon) {
    this.movingLatLon = latLon;
  }

  @Override
  public int compareTo(GpxImageEntry image) {
    if (image instanceof MapillaryAbstractImage) {
      MapillaryAbstractImage img = (MapillaryAbstractImage) image;
      if (this.getSequence() == img.getSequence() && this.getSequence() != null) {
        MapillarySequence seq = this.getSequence();
        return compare(seq.getImages().indexOf(this), seq.getImages().indexOf(image));
      }
      if (this.getSequence() != null && img.getSequence() != null) {
        int compareSeq = compare(this.getSequence().getCapturedAt(), img.getSequence().getCapturedAt());
        if (compareSeq != 0) {
          return compareSeq;
        }
      }
    }
    int compareTime = super.compareTo(image);
    if (compareTime == 0) {// both have same time
      return hashCode() - image.hashCode();
    }
    return compareTime;
  }

  /**
   * The quality score for the image
   *
   * @param qualityScore Set the quality score for this image
   */
  public void setQuality(int qualityScore) {
    if (qualityScore > 0 && qualityScore < 6) {
      this.qualityScore = qualityScore;
    } else {
      this.qualityScore = Integer.MIN_VALUE;
    }
  }

  /**
   * The quality score for the image
   *
   * @return An int in the range of 1-5. Any other value should be considered "unset". We use {@code Integer#MIN_VALUE}
   *         for this purpose explicitly.
   */
  public int getQuality() {
    return this.qualityScore;
  }

  /**
   * @param deleted Set the image as deleted
   */
  public void setDeleted(boolean deleted) {
    this.deleted = deleted;
  }

  /**
   * @return {@code true} if this image is "deleted".
   */
  public boolean isDeleted() {
    return this.deleted;
  }

  @Override
  public Map<String, String> getTagMap() {
    return this.tags;
  }

  @Override
  public boolean equals(Object other) {
    if (super.equals(other) && other instanceof MapillaryAbstractImage) {
      MapillaryAbstractImage o = (MapillaryAbstractImage) other;
      // Ignore temporary/changing variables
      return Objects.equals(this.pano, o.pano) && Objects.equals(this.qualityScore, o.qualityScore);
    }
    return false;
  }

  @Override
  public int hashCode() {
    return Objects.hash(super.hashCode(), this.pano, this.qualityScore);
  }
}
