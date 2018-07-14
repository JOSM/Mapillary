// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.panorama;

import java.awt.Point;
import java.awt.geom.Point2D;
import java.awt.image.BufferedImage;
import java.util.stream.IntStream;

public class CameraPlane {
  private final int width;
  private final int height;
  private final double distance;

  private Vector3D[][] vectors;
  private double theta;
  private double sinTheta;
  private double cosTheta;
  private double phi;
  private double sinPhi;
  private double cosPhi;

  public CameraPlane(int width, int height, double distance) {
    this.width = width;
    this.height = height;
    this.distance = distance;
    setRotation(0.0, 0.0);
    vectors = new Vector3D[width][height];
    IntStream.range(0, height).parallel().forEach(y -> {
      IntStream.range(0, width).parallel().forEach(x -> {
        vectors[x][y] = new Vector3D(x - width / 2d, y - height / 2d, distance).normalize();
      });
    });
  }

  /**
   * @param vector the vector for which the corresponding point on the camera plane will be returned
   * @return the point on the camera plane to which the given vector is mapped, nullable
   */
  public Point getPoint(final Vector3D vector) {
    final Vector3D rotatedVector = rotate(vector, -1);
    if (rotatedVector.getZ() < 0) {
      return null; // Ignores any points "behind the back", so they don't get painted a second time on the other side of the sphere
    }
    return new Point(
      (int) Math.max(Integer.MIN_VALUE, Math.min(Integer.MAX_VALUE, Math.round(
        rotatedVector.getX() / rotatedVector.getZ() * distance + width / 2d
      ))),
      (int) Math.max(Integer.MIN_VALUE, Math.min(Integer.MAX_VALUE, Math.round(
        rotatedVector.getY() / rotatedVector.getZ() * distance + height / 2d
      )))
    );
  }

  Vector3D getVector3D(final Point p) {
    Vector3D res;
    try {
      res = rotate(vectors[p.x][p.y]);
    } catch (Exception e) {
      res = new Vector3D(0, 0, 1);
    }
    return res;
  }

  /**
   * Set camera plane rotation by current plane position.
   * @param p Point within current plane.
   */
  public void setRotation(final Point p) {
    setRotation(getVector3D(p));
  }

  public void setRotationFromDelta(final Point from, final Point to) {
    Vector3D f1 = vectors[from.x][from.y];
    Vector3D t1 = vectors[to.x][to.y];
    double deltaTheta = Math.atan2(f1.getX(), f1.getZ()) - Math.atan2(t1.getX(), t1.getZ());
    double deltaPhi = Math.atan2(f1.getY(), Math.sqrt(f1.getX() * f1.getX() + f1.getZ() * f1.getZ()))
        - Math.atan2(t1.getY(), Math.sqrt(t1.getX() * t1.getX() + t1.getZ() * t1.getZ()));
    double newTheta = theta + deltaTheta;
    double newPhi = phi + deltaPhi;
    setRotation(newTheta, newPhi);
  }

  /**
   * Set camera plane rotation by spherical vector.
   * @param vec vector pointing new view position.
   */
  public void setRotation(Vector3D vec) {
    double theta, phi;
    try {
      theta = Math.atan2(vec.getX(), vec.getZ());
      phi = Math.atan2(vec.getY(), Math.sqrt(vec.getX() * vec.getX() + vec.getZ() * vec.getZ()));
    } catch (Exception e) {
      theta = 0;
      phi = 0;
    }
    setRotation(theta, phi);
  }

  Vector3D getRotation() {
    return new Vector3D(sinTheta, sinPhi, cosPhi * cosTheta);
  }

  synchronized void setRotation(double theta, double phi) {
    this.theta = theta;
    this.sinTheta = Math.sin(theta);
    this.cosTheta = Math.cos(theta);
    this.phi = phi;
    this.sinPhi = Math.sin(phi);
    this.cosPhi = Math.cos(phi);
  }

  private Vector3D rotate(final Vector3D vec) {
    return rotate(vec, 1);
  }

  private Vector3D rotate(final Vector3D vec, final int rotationFactor) {
    double vecX, vecY, vecZ;
    vecZ = vec.getZ() * cosPhi - vec.getY() * sinPhi;
    vecY = vec.getZ() * sinPhi + vec.getY() * cosPhi;
    vecX = vecZ * sinTheta * rotationFactor + vec.getX() * cosTheta;
    vecZ = vecZ * cosTheta - vec.getX() * sinTheta * rotationFactor;
    return new Vector3D(vecX, vecY, vecZ);
  }

  public void mapping(BufferedImage sourceImage, BufferedImage targetImage) {
    IntStream.range(0, targetImage.getHeight()).parallel().forEach(y -> {
      IntStream.range(0, targetImage.getWidth()).forEach(x -> {
        final Vector3D vec = getVector3D(new Point(x, y));
        final Point2D.Double p = UVMapping.getTextureCoordinate(vec);
        targetImage.setRGB(x, y,
            sourceImage.getRGB((int) (p.x * (sourceImage.getWidth() - 1)), (int) (p.y * (sourceImage.getHeight() - 1)))
        );
      });
    });
  }
}
