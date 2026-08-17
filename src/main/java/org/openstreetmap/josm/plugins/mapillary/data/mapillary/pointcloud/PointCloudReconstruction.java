// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.data.mapillary.pointcloud;

import java.util.Map;

public record PointCloudReconstruction(PointCloudLatLonAlt referenceLatLonAlt, Map<String, PointCloudCamera> cameras,
    Map<String, PointCloudShot> shots, Map<String, PointCloudPoint> points) {
}
