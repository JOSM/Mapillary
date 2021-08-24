package org.openstreetmap.josm.plugins.mapillary.gui.panorama;

import javax.annotation.concurrent.Immutable;

/**
 * Basic rotation container.
 */
@Immutable
public final class Rotation {
    /** Radial distance */
    private final double radialDistance;
    /** Polar angle (angle from Z axis, AKA straight up) */
    private final double polarAngle;
    /** Cosine of polar angle (angle from Z axis, AKA straight up) */
    private final double polarAngleCos;
    /** Sine of polar angle (angle from Z axis, AKA straight up) */
    private final double polarAngleSin;
    /** Azimuthal angle (angle from X axis) */
    private final double azimuthalAngle;
    /** Cosine of azimuthal angle (angle from X axis) */
    private final double azimuthalAngleCos;
    /** Sine of azimuthal angle (angle from X axis) */
    private final double azimuthalAngleSin;

    /**
     * Create a new rotation object
     *
     * @param radialDistance The radial distance (rho)
     * @param polarAngle The polar angle (physics: theta, math: phi)
     * @param azimuthalAngle The azimuthal angle (physics: phi, math: theta)
     */
    public Rotation(final double radialDistance, final double polarAngle, final double azimuthalAngle) {
        this.radialDistance = radialDistance;
        this.polarAngle = polarAngle;
        this.polarAngleCos = Math.cos(this.polarAngle);
        this.polarAngleSin = Math.sin(this.polarAngle);
        this.azimuthalAngle = azimuthalAngle;
        this.azimuthalAngleSin = Math.sin(this.azimuthalAngle);
        this.azimuthalAngleCos = Math.cos(this.azimuthalAngle);
    }

    /**
     * Get the radial distance (rho)
     *
     * @return The radial distance
     */
    public double getRadialDistance() {
        return this.radialDistance;
    }

    /**
     * Get the azimuthal angle (physics: phi, math: theta)
     *
     * @return The azimuthal angle (angle from X axis)
     */
    public double getAzimuthalAngle() {
        return this.azimuthalAngle;
    }

    /**
     * Get the sine of the azimuthal angle
     *
     * @return The sine (angle from X axis)
     */
    public double getAzimuthalAngleSin() {
        return this.azimuthalAngleSin;
    }

    /**
     * Get the cosine of the azimuthal angle
     *
     * @return The cosine (angle from X axis)
     */
    public double getAzimuthalAngleCos() {
        return this.azimuthalAngleCos;
    }

    /**
     * Get the polar angle (physics: theta, math: phi)
     *
     * @return The polar angle (angle from Z axis)
     */
    public double getPolarAngle() {
        return this.polarAngle;
    }

    /**
     * Get the sine of the polar angle
     *
     * @return The sine (angle from Z axis)
     */
    public double getPolarAngleSin() {
        return this.polarAngleSin;
    }

    /**
     * Get the cosine of the polar angle
     *
     * @return The cosine (angle from Z axis)
     */
    public double getPolarAngleCos() {
        return this.polarAngleCos;
    }
}
