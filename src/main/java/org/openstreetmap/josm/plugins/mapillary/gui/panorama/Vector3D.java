// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.panorama;

import javax.annotation.concurrent.Immutable;

@Immutable
public final class Vector3D {
    public static final Vector3D DEFAULT_VECTOR_3D = new Vector3D(0, 0, 1);

    private final double x;
    private final double y;
    private final double z;
    /** The radius r */
    private final double radialDistance;
    /** The polar angle theta (inclination) */
    private final double polarAngle;
    /** The azimuthal angle phi */
    private final double azimuthalAngle;

    /**
     * Create a new Vector3D object
     *
     * @param x The x coordinate
     * @param y The y coordinate
     * @param z The z coordinate
     */
    Vector3D(double x, double y, double z) {
        this.x = x;
        this.y = y;
        this.z = z;
        this.radialDistance = Math.sqrt(Math.pow(this.x, 2) + Math.pow(this.y, 2) + Math.pow(this.z, 2));
        // This _should_ be Math.acos(z / radialDistance)
        // This was Math.acos(y / radialDistance)
        this.azimuthalAngle = Math.acos(this.y / this.radialDistance);
        // This _should_ be Math.atan(y, x)
        // This was Math.atan(x, z)
        this.polarAngle = Math.atan2(this.x, this.z);
    }

    /**
     * Get the x coordinate
     *
     * @return The x coordinate
     */
    double getX() {
        return x;
    }

    /**
     * Get the y coordinate
     *
     * @return The y coordinate
     */
    double getY() {
        return y;
    }

    /**
     * Get the z coordinate
     *
     * @return The z coordinate
     */
    double getZ() {
        return z;
    }

    /**
     * Get the radius
     *
     * @return The radius
     */
    double getRadialDistance() {
        return this.radialDistance;
    }

    /**
     * Get the polar angle (inclination)
     *
     * @return The polar angle
     */
    double getPolarAngle() {
        return this.polarAngle;
    }

    /**
     * Get the azimuthal angle
     *
     * @return The azimuthal angle
     */
    double getAzimuthalAngle() {
        return this.azimuthalAngle;
    }

    /**
     * Normalize the vector
     *
     * @return A normalized vector
     */
    Vector3D normalize() {
        final double length = Math.sqrt(x * x + y * y + z * z);
        final double newX = x / length;
        final double newY = y / length;
        final double newZ = z / length;
        return new Vector3D(newX, newY, newZ);
    }

    @Override
    public String toString() {
        return "[x=" + this.x + ", y=" + this.y + ", z=" + this.z + ", r=" + this.radialDistance + ", inclination="
            + this.polarAngle + ", azimuthal=" + this.azimuthalAngle + "]";
    }
}
