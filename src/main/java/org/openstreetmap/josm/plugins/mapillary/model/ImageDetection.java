// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.model;

import java.awt.Color;
import java.awt.geom.Path2D;
import java.util.Arrays;

import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.PointObjectLayer;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryColorScheme;

public class ImageDetection extends SpecialImageArea {
  private static final String PACKAGE_TRAFFIC_SIGNS = "trafficsign";

  private final String packag;
  private final double score;
  private final String value;

  public ImageDetection(
    final Path2D shape, final String imageKey, final String key, final double score, final String packag,
    final String value
  ) {
    super(shape, imageKey, key);
    this.packag = packag;
    this.score = score;
    this.value = value;
  }

  public String getPackage() {
    return packag;
  }

  public double getScore() {
    return score;
  }

  public String getValue() {
    return value;
  }

  public boolean isTrafficSign() {
    return (packag != null && packag.contains(PACKAGE_TRAFFIC_SIGNS))
      || (value != null && value.contains("traffic-sign"));
  }

  /**
   * Get the color to paint this detection with
   *
   * @return The color to paint the detection outline
   */
  public Color getColor() {
    if (MainApplication.getLayerManager().getLayersOfType(PointObjectLayer.class).parallelStream().map(
      PointObjectLayer::getDataSet
    ).flatMap(ds -> ds.getSelected().parallelStream()).filter(prim -> prim.hasKey("detections"))
      .anyMatch(prim -> prim.get("detections").contains(getKey()))) {
      return Color.CYAN;
    }
    if (isTrafficSign())
      return MapillaryColorScheme.IMAGEDETECTION_TRAFFICSIGN;
    if (Arrays.asList("object--vehicle--car", "human--person--individual").contains(value))
      return Color.LIGHT_GRAY;
    return MapillaryColorScheme.IMAGEDETECTION_UNKNOWN;
  }
}
