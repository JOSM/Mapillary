// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.model;

import java.awt.geom.Path2D;

public class ImageDetection extends SpecialImageArea {
  private static final String PACKAGE_TRAFFIC_SIGNS = "trafficsign";

  private final String packag;
  private final double score;
  private final String value;

  public ImageDetection(final Path2D shape, final String imageKey, final String key, final double score, final String packag, final String value) {
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
    return PACKAGE_TRAFFIC_SIGNS.equals(packag);
  }
}
