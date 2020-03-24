// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.data.gpx.GpxImageEntry;
import org.openstreetmap.josm.plugins.mapillary.model.ImageDetection;
import org.openstreetmap.josm.plugins.mapillary.model.UserProfile;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryColorScheme;
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
   * If {@code true}, the image is private. If {@code false}, then it is public.
   */
  private final boolean privateImage;

  /**
   * Main constructor of the class MapillaryImage
   *
   * @param key          The unique identifier of the image.
   * @param latLon       The latitude and longitude where it is positioned.
   * @param ca           The direction of the images in degrees, meaning 0 north.
   * @param pano         The property to indicate whether image is panorama or not.
   * @param privateImage The property to indicate if the image is private or not.
   */
  public MapillaryImage(final String key, final LatLon latLon, final double ca, final boolean pano,
      final boolean privateImage) {
    super(latLon, ca, pano);
    this.key = key;
    this.privateImage = privateImage;
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
  public int compareTo(GpxImageEntry image) {
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

  @Override
  public Color paintHighlightedColour() {
    return privateImage ? MapillaryColorScheme.SEQ_PRIVATE_HIGHLIGHTED : MapillaryColorScheme.SEQ_HIGHLIGHTED;
  }

  @Override
  public Color paintHighlightedAngleColour() {
    return privateImage ? MapillaryColorScheme.SEQ_PRIVATE_HIGHLIGHTED_CA : MapillaryColorScheme.SEQ_HIGHLIGHTED_CA;
  }

  @Override
  public Color paintSelectedColour() {
    return privateImage ? MapillaryColorScheme.SEQ_PRIVATE_SELECTED : MapillaryColorScheme.SEQ_SELECTED;
  }

  @Override
  public Color paintSelectedAngleColour() {
    return privateImage ? MapillaryColorScheme.SEQ_PRIVATE_SELECTED_CA : MapillaryColorScheme.SEQ_SELECTED_CA;
  }

  @Override
  public Color paintUnselectedColour() {
    return privateImage ? MapillaryColorScheme.SEQ_PRIVATE_UNSELECTED : MapillaryColorScheme.SEQ_UNSELECTED;
  }

  @Override
  public Color paintUnselectedAngleColour() {
    return privateImage ? MapillaryColorScheme.SEQ_PRIVATE_UNSELECTED_CA : MapillaryColorScheme.SEQ_UNSELECTED_CA;
  }
}
