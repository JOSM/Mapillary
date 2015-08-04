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

  /** The user that made the image */
  private String user;
  /** Set of traffic signs in the image */
  private List<String> signs;
  private String location;

  /**
   * Returns the location where the image was taken.
   *
   * @return A String containing the location where the picture was taken.
   */
  public String getLocation() {
    return this.location;
  }

  /**
   * Sets the location of the image.
   *
   * @param location
   *          A String object containing the place where the image was taken.
   */
  public void setLocation(String location) {
    this.location = location;
  }

  /**
   * Main constructor of the class MapillaryImage
   *
   * @param key
   *          The unique identifier of the image.
   * @param lat
   *          The latitude where it is positioned.
   * @param lon
   *          The longitude where it is positioned.
   * @param ca
   *          The direction of the images in degrees, meaning 0 north.
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
   *          A String that identifies the type of sign.
   */
  public void addSign(String sign) {
    this.signs.add(sign);
  }

  /**
   * Returns a List containing the signs assigned to this image.
   *
   * @return A List object containing the signs assigned to this image.
   */
  public List<String> getSigns() {
    return this.signs;
  }

  /**
   * Sets the username of the person who took the image.
   *
   * @param user
   *          A String containing the username of the person who took the image.
   */
  public void setUser(String user) {
    this.user = user;
  }

  /**
   * Returns the username of the person who took the picture.
   *
   * @return A String containing the username of the person who took the
   *         picture.
   */
  public String getUser() {
    return this.user;
  }

  @Override
  public String toString() {
    return "Image[key=" + this.key + ";lat=" + this.latLon.lat() + ";lon="
        + this.latLon.lon() + ";ca=" + this.ca + "]";
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
