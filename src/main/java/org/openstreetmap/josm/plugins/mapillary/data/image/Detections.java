// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.data.image;

import java.util.Collection;
import java.util.List;

import org.openstreetmap.josm.plugins.mapillary.model.ImageDetection;

/**
 *
 */
public interface Detections {
  /**
   * Get the detections for this image
   *
   * @return Currently downloaded detections for the image
   */
  default List<ImageDetection<?>> getDetections() {
    return getDetections(false);
  }

  /**
   * Get the detections for this image
   *
   * @param force {@code true} to force a download, if there are no detections yet for the image.
   * @return Detections for the image
   */
  List<ImageDetection<?>> getDetections(boolean force);

  /**
   * Set all detections for this image
   *
   * @param newDetections The detections for this image
   */
  void setAllDetections(Collection<ImageDetection<?>> newDetections);

}
