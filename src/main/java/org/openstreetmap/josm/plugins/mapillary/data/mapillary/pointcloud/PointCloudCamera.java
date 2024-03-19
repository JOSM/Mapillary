// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.data.mapillary.pointcloud;

public record PointCloudCamera(ProjectionType projectionType, int width, int height, double focal, double k1,
    double k2) {
}
