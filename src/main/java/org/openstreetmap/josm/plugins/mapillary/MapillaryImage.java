// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary;

import java.awt.Color;
import java.awt.Image;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import javax.swing.ImageIcon;

import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.io.download.DetectionsDownloadRunnable;
import org.openstreetmap.josm.plugins.mapillary.model.ImageDetection;
import org.openstreetmap.josm.plugins.mapillary.model.UserProfile;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryColorScheme;
import org.openstreetmap.josm.tools.ImageProvider;
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
  private boolean detectionsForced;
  /**
   * If {@code true}, the image is private. If {@code false}, then it is public.
   */
  private final boolean privateImage;
  /**
   * Indicates whether or not this images has been marked for deletion.
   */
  private boolean isDeleted = false;

  /** The default sprite for a private Mapillary image */
  public static final ImageIcon PRIVATE_SPRITE = new ImageProvider(IMAGE_SPRITE_DIR, "private-ca")
    .setMaxWidth(ImageProvider.ImageSizes.MAP.getAdjustedHeight()).get();

  /**
   * Main constructor of the class MapillaryImage
   *
   * @param key The unique identifier of the image.
   * @param latLon The latitude and longitude where it is positioned.
   * @param ca The direction of the images in degrees, meaning 0 north.
   * @param pano The property to indicate whether image is panorama or not.
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

  /**
   * Get the detections for this image
   *
   * @return Currently downloaded detections for the image
   */
  public List<ImageDetection> getDetections() {
    return this.getDetections(false);
  }

  /**
   * Get the detections for this image
   *
   * @param force {@code true} to force a download, if there are no detections yet for the image.
   * @return Detections for the image
   */
  public List<ImageDetection> getDetections(boolean force) {
    if (force && !this.detectionsForced && this.detections.isEmpty()) {
      DetectionsDownloadRunnable.get(this);
      this.detectionsForced = true;
    }
    return detections;

  }

  public UserProfile getUser() {
    if (getSequence() != null) {
      return getSequence().getUser();
    }
    return UserProfile.NONE;
  }

  public void setAllDetections(Collection<ImageDetection> newDetections) {
    if (newDetections != null) {
      Logging.debug("Add {0} detections to image {1}", newDetections.size(), this.getKey());
      synchronized (detections) {
        detections.clear();
        detections.addAll(newDetections);
      }
    }
  }

  @Override
  public String toString() {
    return String.format("Image[key=%s,lat=%f,lon=%f,ca=%f,user=%s,capturedAt=%d]", key, getExifCoor().lat(),
      getExifCoor().lon(), ca, getUser() == null ? "null" : getUser().getUsername(), capturedAt);
  }

  @Override
  public boolean equals(Object object) {
    return object instanceof MapillaryImage && this.key.equals(((MapillaryImage) object).getKey());
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
    if (!isDeleted) {
      super.turn(ca);
      checkModified();
    }
  }

  public void markDeleted() {
    if (MapillaryLayer.hasInstance()) {
      MapillaryLayer.getInstance().getDeletionChangeset().add(this);
      isDeleted = Boolean.TRUE;
      this.setMovingLatLon(getExifCoor());
    }
  }

  public void unmarkDeleted() {
    if (MapillaryLayer.hasInstance()) {
      MapillaryLayer.getInstance().getDeletionChangeset().add(this);
      isDeleted = Boolean.FALSE;
      this.setMovingLatLon(getTempLatLon());
    }
  }

  public boolean isDeleted() {
    return isDeleted;
  }

  @Override
  public Color paintHighlightedAngleColour() {
    return privateImage ? MapillaryColorScheme.SEQ_PRIVATE_HIGHLIGHTED_CA : MapillaryColorScheme.SEQ_HIGHLIGHTED_CA;
  }

  @Override
  public Color paintSelectedAngleColour() {
    return privateImage ? MapillaryColorScheme.SEQ_PRIVATE_SELECTED_CA : MapillaryColorScheme.SEQ_SELECTED_CA;
  }

  @Override
  public Color paintUnselectedAngleColour() {
    return privateImage ? MapillaryColorScheme.SEQ_PRIVATE_UNSELECTED_CA : MapillaryColorScheme.SEQ_UNSELECTED_CA;
  }

  @Override
  public Image getDefaultImage() {
    return privateImage ? PRIVATE_SPRITE.getImage() : DEFAULT_SPRITE.getImage();
  }
}
