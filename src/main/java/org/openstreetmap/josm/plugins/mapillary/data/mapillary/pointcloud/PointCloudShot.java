// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.data.mapillary.pointcloud;

import java.time.Instant;

public record PointCloudShot(String cameraId, PointCloudXYZ rotation, PointCloudXYZ translation,
    PointCloudXYZ gpsPosition, double gpsDop, int orientation, Instant captureTime) {
}
