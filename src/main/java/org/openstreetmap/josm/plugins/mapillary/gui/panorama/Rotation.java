package org.openstreetmap.josm.plugins.mapillary.gui.panorama;

import javax.annotation.concurrent.Immutable;

/**
 * Basic rotation container.
 */
@Immutable
public class Rotation {
  /** Radial distance */
  private final double radialDistance;
  /** Polar angle (angle from Z axis, AKA straight up) */
  private final double polarAngle;
  /** Azimuthal angle (angle from X axis) */
  private final double azimuthalAngle;

  /**
   * Create a new rotation object
   * @param radialDistance The radial distance (rho)
   * @param polarAngle The polar angle (physics: theta, math: phi)
   * @param azimuthalAngle The azimuthal angle (physics: phi, math: theta)
   */
  public Rotation(final double radialDistance, final double polarAngle, final double azimuthalAngle) {
    this.radialDistance = radialDistance;
    this.polarAngle = polarAngle;
    this.azimuthalAngle = azimuthalAngle;
  }

  /**
   * Get the radial distance (rho)
   * @return The radial distance
   */
  public double getRadialDistance() {
    return this.radialDistance;
  }

  /**
   * Get the azimuthal angle (physics: phi, math: theta)
   * @return The azimuthal angle (angle from X axis)
   */
  public double getAzimuthalAngle() {
    return this.azimuthalAngle;
  }

  /**
   * Get the polar angle (physics: theta, math: phi)
   * @return The polar angle (angle from Z axis)
   */
  public double getPolarAngle() {
    return this.polarAngle;
  }
}
