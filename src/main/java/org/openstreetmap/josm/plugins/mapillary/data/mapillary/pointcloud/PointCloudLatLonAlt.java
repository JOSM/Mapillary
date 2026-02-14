// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.data.mapillary.pointcloud;

import org.openstreetmap.josm.data.coor.ILatLon;

public record PointCloudLatLonAlt(double lat, double lon, double altitude) implements ILatLon {
}
