// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.model;

import java.awt.Color;
import java.awt.Shape;
import java.io.Serializable;
import java.util.Objects;

import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.ObjectDetections;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.PointObjectLayer;
import org.openstreetmap.josm.plugins.mapillary.utils.DetectionVerification;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryColorScheme;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.tools.Pair;

/**
 * A store for ImageDetection information
 */
public class ImageDetection<T extends Shape & Serializable> extends SpecialImageArea<T> {
  private static final String PACKAGE_TRAFFIC_SIGNS = "trafficsign";

  private final String packag;
  private final double score;
  private final ObjectDetections value;
  private final String originalValue;
  private boolean rejected;

  private DetectionVerification.TYPE approvalType;

  /**
   * Create a new ImageDetection
   *
   * @param shape The shape of the image detection to be drawn
   * @param imageKey The image key for the detection
   * @param key The detection key
   * @param score The score of the detection
   * @param packag The layer (formerly package) of the detection
   * @param value The actual detection value (e.g., `object--fire-hydrant`)
   */
  public ImageDetection(final T shape, final String imageKey, final String key, final double score, final String packag,
    final String value) {
    super(shape, imageKey, key);
    this.packag = packag;
    this.score = score;
    Pair<Boolean, ObjectDetections> foundDetection = ObjectDetections.findFallbackDetection(value);
    this.value = foundDetection.b;
    this.rejected = foundDetection.a;
    if (this.value == ObjectDetections.UNKNOWN) {
      this.originalValue = value;
    } else {
      this.originalValue = null;
    }
  }

  /**
   * Get the layer (formerly called "package") of the detection
   *
   * @return The detection layer
   */
  public String getPackage() {
    return packag;
  }

  public double getScore() {
    return score;
  }

  /**
   * Check if the detection has been rejected
   *
   * @return {@code true} if rejected
   */
  public boolean isRejected() {
    return this.rejected;
  }

  /**
   * Get the ObjectDetection type
   *
   * @return The {@link ObjectDetections} value
   */
  public ObjectDetections getValue() {
    return value;
  }

  /**
   * @return The unknown value for the detection, or the mapillary key from the {@link ObjectDetections}.
   */
  public String getUnknownValue() {
    return this.originalValue == null ? getValue().getKey() : this.originalValue;
  }

  /**
   * Check if the detection was a traffic sign
   *
   * @return {@code true} if the detection is a traffic sign
   */
  public boolean isTrafficSign() {
    return (packag != null && packag.contains(PACKAGE_TRAFFIC_SIGNS))
      || (value != null && value.getKey().contains("traffic-sign"));
  }

  /**
   * Get the color to paint this detection with
   *
   * @return The color to paint the detection outline
   */
  public Color getColor() {
    if (MainApplication.getLayerManager().getLayersOfType(PointObjectLayer.class).parallelStream()
      .map(PointObjectLayer::getDataSet).flatMap(ds -> ds.getSelected().parallelStream())
      .filter(prim -> prim.hasKey("detections")).anyMatch(prim -> prim.get("detections").contains(getKey()))) {
      return isRejected() || Boolean.TRUE.equals(MapillaryProperties.SMART_EDIT.get()) ? Color.RED : Color.CYAN;
    }
    if (isTrafficSign())
      return MapillaryColorScheme.IMAGEDETECTION_TRAFFICSIGN;
    if (ObjectDetections.IGNORE_DETECTIONS.contains(value))
      return Color.LIGHT_GRAY;
    return MapillaryColorScheme.IMAGEDETECTION_UNKNOWN;
  }

  /**
   * Set the approval type
   *
   * @param type The approval type
   */
  public void setApprovalType(DetectionVerification.TYPE type) {
    this.approvalType = type;
  }

  /**
   * Get the approval type
   *
   * @return The approval type
   */
  public DetectionVerification.TYPE getApprovalType() {
    return this.approvalType;
  }

  @Override
  public boolean equals(Object other) {
    if (super.equals(other) && other instanceof ImageDetection) {
      ImageDetection<?> o = (ImageDetection<?>) other;
      return Objects.equals(this.approvalType, o.approvalType) && Objects.equals(this.originalValue, o.originalValue)
        && Objects.equals(this.packag, o.packag) && Objects.equals(this.rejected, o.rejected)
        && Objects.equals(this.score, o.score) && Objects.equals(this.value, o.value);
    }
    return false;
  }

  @Override
  public int hashCode() {
    return Objects.hash(super.hashCode(), this.approvalType, this.originalValue, this.packag, this.rejected, this.score,
      this.value);
  }
}
