// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.panorama;

final class Vector3D {

    private double x;
    private double y;
    private double z;

    Vector3D(double x, double y, double z) {
        this.x = x;
        this.y = y;
        this.z = z;
    }

    double getX() {
        return x;
    }
    double getY() {
        return y;
    }
    double getZ() {
        return z;
    }

    synchronized Vector3D normalize() {
      final double length = Math.sqrt(x*x + y*y + z*z);
      x /= length;
      y /= length;
      z /= length;
      return this;
    }
}
