// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.plugins.mapillary.model.ImageDetection;
import org.openstreetmap.josm.plugins.mapillary.model.UserProfile;
import org.openstreetmap.josm.tools.Logging;

/**
 * A MapillaryImage object represents each of the images stored in Mapillary.
 *
 * @author nokutu
 * @see MapillarySequence
 * @see MapillaryData
 */
public class MapillaryImage extends MapillaryAbstractImage {
  /**
   * Unique identifier of the object.
   */
  private final String key;
  /**
   * Set of traffic signs in the image.
   */
  private final List<ImageDetection> detections = Collections.synchronizedList(new ArrayList<>());

  /**
   * Main constructor of the class MapillaryImage
   *
   * @param key    The unique identifier of the image.
   * @param latLon The latitude and longitude where it is positioned.
   * @param ca     The direction of the images in degrees, meaning 0 north.
   */
  public MapillaryImage(final String key, final LatLon latLon, final double ca, final boolean pano) {
    super(latLon, ca, pano);
    this.key = key;
  }

  /**
   * Returns the unique identifier of the object.
   *
   * @return A {@code String} containing the unique identifier of the object.
   */
  public String getKey() {
    return this.key;
  }

  public List<ImageDetection> getDetections() {
    return detections;
  }

  public UserProfile getUser() {
    return getSequence().getUser();
  }

  public void setAllDetections(Collection<ImageDetection> newDetections) {
    Logging.debug("Add {0} detections to image {1}", newDetections.size(), this.getKey());
    synchronized (detections) {
      detections.clear();
      detections.addAll(newDetections);
    }
  }

  @Override
  public String toString() {
    return String.format(
      "Image[key=%s,lat=%f,lon=%f,ca=%f,user=%s,capturedAt=%d]",
      key, latLon.lat(), latLon.lon(), ca, getUser() == null ? "null" : getUser().getUsername(), capturedAt
    );
  }

  @Override
  public boolean equals(Object object) {
    return object instanceof MapillaryImage && this.key.equals(((MapillaryImage) object).getKey());
  }

  @Override
  public int compareTo(MapillaryAbstractImage image) {
    if (image instanceof MapillaryImage) {
      return this.key.compareTo(((MapillaryImage) image).getKey());
    }
    return hashCode() - image.hashCode();
  }

  @Override
  public int hashCode() {
    return this.key.hashCode();
  }

  @Override
  public void stopMoving() {
    super.stopMoving();
    checkModified();
  }

  private void checkModified() {
    if (MapillaryLayer.hasInstance()) {
      if (isModified()) {
        MapillaryLayer.getInstance().getLocationChangeset().add(this);
      } else {
        MapillaryLayer.getInstance().getLocationChangeset().remove(this);
      }
    }
  }

  @Override
  public void turn(double ca) {
    super.turn(ca);
    checkModified();
  }
}
