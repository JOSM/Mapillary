// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.panorama;

public final class Vector3D {
    public static final Vector3D DEFAULT_VECTOR_3D = new Vector3D(0, 0, 1);

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
