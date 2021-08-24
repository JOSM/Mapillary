// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.panorama;

import java.awt.Point;
import java.awt.geom.Point2D;
import java.awt.image.BufferedImage;
import java.awt.image.DataBuffer;
import java.awt.image.DataBufferInt;
import java.util.stream.IntStream;

import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.tools.Logging;

public class CameraPlane {
    private final int width;
    private final int height;
    private final double distance;

    private final Vector3D[][] vectors;
    private double theta;
    private double sinTheta;
    private double cosTheta;
    private double phi;
    private double sinPhi;
    private double cosPhi;

    public static final double HALF_PI = Math.PI / 2;
    public static final double TWO_PI = 2 * Math.PI;

    public CameraPlane(int width, int height, double distance) {
        this.width = width;
        this.height = height;
        this.distance = distance;
        setRotation(theta, 0.0);
        vectors = new Vector3D[width][height];
        IntStream.range(0, height).parallel().forEach(y -> IntStream.range(0, width).parallel()
            .forEach(x -> vectors[x][y] = new Vector3D(x - width / 2d, y - height / 2d, distance).normalize()));
    }

    /**
     * Get the point for a vector
     *
     * @param vector the vector for which the corresponding point on the camera plane will be returned
     * @return the point on the camera plane to which the given vector is mapped, nullable
     */
    public Point getPoint(final Vector3D vector) {
        final Vector3D rotatedVector = rotate(vector, -1);
        if (rotatedVector.getZ() < 0) {
            return null; // Ignores any points "behind the back", so they don't get painted a second time on the other
                         // side of
                         // the sphere
        }
        // This is a slightly faster than just doing the (brute force) method of Math.max(Math.min)). Reduces if
        // statements
        // by 1 per call.
        final long x = Math.round(rotatedVector.getX() / rotatedVector.getZ() * distance + width / 2d);
        long y = Math.round(rotatedVector.getY() / rotatedVector.getZ() * distance + height / 2d);

        try {
            return new Point(Math.toIntExact(x), Math.toIntExact(y));
        } catch (ArithmeticException e) {
            return new Point((int) Math.max(Integer.MIN_VALUE, Math.min(Integer.MAX_VALUE, x)),
                (int) Math.max(Integer.MIN_VALUE, Math.min(Integer.MAX_VALUE, y)));
        }
    }

    Vector3D getVector3D(final Point p) {
        Vector3D res;
        try {
            res = rotate(vectors[p.x][p.y]);
        } catch (Exception e) {
            res = Vector3D.DEFAULT_VECTOR_3D;
        }
        return res;
    }

    /**
     * Set camera plane rotation by current plane position.
     *
     * @param p Point within current plane.
     */
    public void setRotation(final Point p) {
        setRotation(getVector3D(p));
    }

    public void setRotationFromDelta(final Point from, final Point to) {
        try {
            Vector3D f1 = vectors[from.x][from.y];
            Vector3D t1 = vectors[to.x][to.y];
            // TODO switch to all apache math
            // rotation = new Rotation(f1, t1).applyTo(rotation);
            double deltaTheta = Math.atan2(f1.getX(), f1.getZ()) - Math.atan2(t1.getX(), t1.getZ());
            double deltaPhi = Math.atan2(f1.getY(), Math.sqrt(f1.getX() * f1.getX() + f1.getZ() * f1.getZ()))
                - Math.atan2(t1.getY(), Math.sqrt(t1.getX() * t1.getX() + t1.getZ() * t1.getZ()));
            double newTheta = theta + deltaTheta;
            // Prevent flipping the 360 viewer accidentally
            double newPhi = Math.max(Math.min(phi + deltaPhi, HALF_PI), -HALF_PI);
            setRotation(newTheta, newPhi);
            MapillaryLayer.invalidateInstance();
        } catch (ArrayIndexOutOfBoundsException e) {
            Logging.error(e);
        }
    }

    /**
     * Set camera plane rotation by spherical vector.
     *
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

    public Rotation getRotation() {
        // TODO use this.rotation -- not currently used due to other issues (largely rotation such that the image
        // appears skewed)
        // TODO don't forget to re-enable the test!
        return new Rotation(this.distance, this.phi, this.theta);
    }

    synchronized void setRotation(double theta, double phi) {
        if (theta < 0) {
            this.theta = theta + TWO_PI;
        } else if (theta > TWO_PI) {
            this.theta = theta - TWO_PI;
        } else {
            this.theta = theta;
        }
        this.sinTheta = Math.sin(this.theta);
        this.cosTheta = Math.cos(this.theta);
        this.phi = phi;
        this.sinPhi = Math.sin(this.phi);
        this.cosPhi = Math.cos(this.phi);
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
        DataBuffer sourceBuffer = sourceImage.getRaster().getDataBuffer();
        DataBuffer targetBuffer = targetImage.getRaster().getDataBuffer();
        // Faster mapping
        if (sourceBuffer.getDataType() == DataBuffer.TYPE_INT && targetBuffer.getDataType() == DataBuffer.TYPE_INT) {
            int[] sourceImageBuffer = ((DataBufferInt) sourceImage.getRaster().getDataBuffer()).getData();
            int[] targetImageBuffer = ((DataBufferInt) targetImage.getRaster().getDataBuffer()).getData();
            IntStream.range(0, targetImage.getHeight()).parallel()
                .forEach(y -> IntStream.range(0, targetImage.getWidth()).forEach(x -> {
                    final Vector3D vec = getVector3D(new Point(x, y));
                    final Point2D.Double p = UVMapping.getTextureCoordinate(vec);
                    int tx = (int) (p.x * (sourceImage.getWidth() - 1));
                    int ty = (int) (p.y * (sourceImage.getHeight() - 1));
                    int color = sourceImageBuffer[ty * sourceImage.getWidth() + tx];
                    targetImageBuffer[y * targetImage.getWidth() + x] = color;
                }));
        } else {
            IntStream.range(0, targetImage.getHeight()).parallel()
                .forEach(y -> IntStream.range(0, targetImage.getWidth()).parallel().forEach(x -> {
                    final Vector3D vec = getVector3D(new Point(x, y));
                    final Point2D.Double p = UVMapping.getTextureCoordinate(vec);
                    targetImage.setRGB(x, y, sourceImage.getRGB((int) (p.x * (sourceImage.getWidth() - 1)),
                        (int) (p.y * (sourceImage.getHeight() - 1))));
                }));
        }
    }
}
