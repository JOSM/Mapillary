// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary;

import java.util.ArrayList;
import java.util.List;

import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.ValidationUtil;

/**
 * A MapillaryImage object represents each of the images stored in Mapillary.
 *
 * @author nokutu
 * @see MapillarySequence
 * @see MapillaryData
 */
public class MapillaryImage extends MapillaryAbstractImage {
  /** Unique identifier of the object. */
  private final String key;
  /** The user that made the image. */
  private String user;
  /** Set of traffic signs in the image. */
  private final List<String> signs = new ArrayList<>();
  /** Where the picture was taken. */
  private String location;

  /**
   * Main constructor of the class MapillaryImage
   *
   * @param key  The unique identifier of the image.
   * @param latLon  The latitude and longitude where it is positioned.
   * @param ca  The direction of the images in degrees, meaning 0 north.
   */
  public MapillaryImage(final String key, final LatLon latLon, final double ca) {
    super(latLon, ca);
    ValidationUtil.throwExceptionForInvalidImgKey(key, true);
    this.key = key;
  }

  /**
   * Returns the location where the image was taken.
   *
   * @return A {@code String} containing the location where the picture was taken.
   */
  public String getLocation() {
    return this.location;
  }

  /**
   * Sets the location of the image.
   *
   * @param location
   *          A {@code String} object containing the place where the image was taken.
   */
  public void setLocation(String location) {
    this.location = location;
  }

  /**
   * Returns the unique identifier of the object.
   *
   * @return A {@code String} containing the unique identifier of the object.
   */
  public String getKey() {
    return this.key;
  }

  /**
   * Adds a new sign to the set of signs.
   *
   * @param sign
   *          A {@code String} that identifies the type of sign.
   */
  public void addSign(String sign) {
    this.signs.add(sign);
  }

  /**
   * Returns a {@link List} containing the signs assigned to this image.
   *
   * @return A {@link List} object containing the signs assigned to this image.
   */
  public List<String> getSigns() {
    return this.signs;
  }

  /**
   * Sets the username of the person who took the image.
   *
   * @param user
   *          A {@code String} containing the username of the person who took the image.
   */
  public void setUser(String user) {
    this.user = user;
  }

  /**
   * Returns the username of the person who took the picture.
   *
   * @return A {@code String} containing the username of the person who took the
   *         picture.
   */
  public String getUser() {
    return this.user;
  }

  @Override
  public String toString() {
    return String.format(
      "Image[key=%s,lat=%f,lon=%f,ca=%f,location=%s,user=%s,capturedAt=%d]",
      key, latLon.lat(), latLon.lon(), ca, location, user, capturedAt
    );
  }

  @Override
  public boolean equals(Object object) {
    if (object instanceof MapillaryImage)
      return this.key.equals(((MapillaryImage) object).getKey());
    return false;
  }

  @Override
  public int compareTo(MapillaryAbstractImage image) {
    if (image instanceof MapillaryImage)
      return this.key.compareTo(((MapillaryImage) image).getKey());
    return super.compareTo(image);
  }

  @Override
  public int hashCode() {
    return this.key.hashCode();
  }
}
