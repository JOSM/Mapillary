// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.detection;

import java.awt.Shape;

public class ImageDetection extends SpecialImageArea {

  private final double score;
  private final String value;

  public ImageDetection(final Shape shape, final String image, final String key, final double score, final String value) {
    super(shape, image, key);
    this.score = score;
    this.value = value;
  }

  public double getScore() {
    return score;
  }

  public String getValue() {
    return value;
  }

}
