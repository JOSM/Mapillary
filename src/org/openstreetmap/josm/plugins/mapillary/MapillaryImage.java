// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.plugins.mapillary.model.ImageDetection;
import org.openstreetmap.josm.plugins.mapillary.model.UserProfile;

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
  private final List<ImageDetection> detections = new ArrayList<>();

  /**
   * Main constructor of the class MapillaryImage
   *
   * @param key    The unique identifier of the image.
   * @param latLon The latitude and longitude where it is positioned.
   * @param ca     The direction of the images in degrees, meaning 0 north.
   */
  public MapillaryImage(final String key, final LatLon latLon, final double ca) {
    super(latLon, ca);
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
    final MapillarySequence seq = getSequence();
    return seq == null ? null : seq.getUser();
  }

  public void setAllDetections(Collection<ImageDetection> detection) {
    Main.debug("Add " + detection.size() + " detections to image " + this.getKey());
    this.detections.clear();
    this.detections.addAll(detection);
  }

  @Override
  public String toString() {
    return String.format(
            "Image[key=%s,lat=%f,lon=%f,ca=%f,user=%s,capturedAt=%d]",
            key, latLon.lat(), latLon.lon(), ca, getUser(), capturedAt
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
    MapillaryLayer mapillaryLayer = MapillaryLayer.getInstance();
    MapillaryLocationChangeset locationChangeset = mapillaryLayer.getLocationChangeset();
    if (this.isModified()) {
      locationChangeset.add(this);
    } else {
      locationChangeset.remove(this);
    }
  }

  @Override
  public void turn(double ca) {
    super.turn(ca);
    checkModified();
  }
}
