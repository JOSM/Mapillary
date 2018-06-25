// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.panorama;

import java.awt.Point;
import java.awt.image.BufferedImage;
import java.util.stream.IntStream;

public class CameraPlane {
  private Vector3D[][] vectors;
  private double theta;
  private double sinTheta;
  private double cosTheta;
  private double phi;
  private double sinPhi;
  private double cosPhi;

  public CameraPlane(int width, int height, double d) {
    setRotation(0.0, 0.0);
    vectors = new Vector3D[width][height];
    IntStream.range(0, height).forEach(y -> {
      IntStream.range(0, width).forEach(x -> {
        double vecX = x - width / 2;
        double vecY = y - height / 2;
        double vecZ = d;
        double invVecLength = 1 / Math.sqrt(vecX * vecX + vecY * vecY + vecZ * vecZ);
        vectors[x][y] = new Vector3D(vecX * invVecLength, vecY * invVecLength, vecZ * invVecLength);
      });
    });
  }

  public Vector3D getVector3D(final Point p) {
    return getVector3D(p.x, p.y);
  }

  public Vector3D getVector3D(final int x, final int y) {
    Vector3D res;
    try {
      res = rotate(vectors[x][y]);
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

  public void setRotation(final int x, final int y) {
    setRotation(getVector3D(x, y));
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

  void setRotation(double theta, double phi) {
    this.theta = theta;
    this.sinTheta = Math.sin(theta);
    this.cosTheta = Math.cos(theta);
    this.phi = phi;
    this.sinPhi = Math.sin(phi);
    this.cosPhi = Math.cos(phi);
  }

  Vector3D rotate(Vector3D vec) {
    double vecX, vecY, vecZ;
    vecZ = vec.getZ() * cosPhi - vec.getY() * sinPhi;
    vecY = vec.getZ() * sinPhi + vec.getY() * cosPhi;
    vecX = vecZ * sinTheta + vec.getX() * cosTheta;
    vecZ = vecZ * cosTheta - vec.getX() * sinTheta;
    return new Vector3D(vecX, vecY, vecZ);
  }

  public void mapping(BufferedImage sourceImage, BufferedImage targetImage) {
    int height = targetImage.getHeight();
    int width = targetImage.getWidth();
    IntStream.range(0, height).parallel().forEach(y -> {
      IntStream.range(0, width).forEach(x -> {
        Vector3D vec = getVector3D(new Point(x, y));
        Point p = mapping(vec, sourceImage.getWidth(), sourceImage.getHeight());
        int color = sourceImage.getRGB(p.x, p.y);
        targetImage.setRGB(x, y, color);
      });
    });
  }

  Point mapping(Vector3D vec, int width, int height) {
    // https://en.wikipedia.org/wiki/UV_mapping
    double u = 0.5 + (Math.atan2(vec.getX(), vec.getZ()) / (2 * Math.PI));
    double v = 0.5 + (Math.asin(vec.getY()) / Math.PI);
    int tx = (int) ((width - 1) * u);
    int ty = (int) ((height - 1) * v);
    return new Point(tx, ty);
  }
}
