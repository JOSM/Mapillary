// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.panorama;

import java.awt.Point;
import java.awt.geom.Point2D;
import java.awt.image.BufferedImage;
import java.awt.image.DataBuffer;
import java.awt.image.DataBufferInt;
import java.util.stream.IntStream;

import javax.annotation.Nullable;

import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.tools.Logging;

/**
 * The plane that the camera appears on and rotates around.
 */
public class CameraPlane {
    /** The field of view for the panorama at 0 zoom */
    static final double PANORAMA_FOV = Math.toRadians(110);
    /** The width of the image */
    private final int width;
    /** The height of the image */
    private final int height;
    /** The current zoom level */
    private double zoom;

    private final Vector3D[][] vectors;
    private Rotation rotation;

    public static final double HALF_PI = Math.PI / 2;
    public static final double TWO_PI = 2 * Math.PI;

    /**
     * Create a new CameraPlane with the default FOV (110 degrees).
     *
     * @param width The width of the image
     * @param height The height of the image
     */
    public CameraPlane(int width, int height) {
        this(width, height, (width / 2d) / Math.tan(PANORAMA_FOV / 2));
    }

    /**
     * Create a new CameraPlane
     *
     * @param width The width of the image
     * @param height The height of the image
     * @param distance The radial distance of the photosphere
     */
    private CameraPlane(int width, int height, double distance) {
        this.width = width;
        this.height = height;
        this.rotation = new Rotation(distance, 0, 0);
        this.vectors = new Vector3D[width][height];
        this.zoom = 1;
        IntStream.range(0, this.height).parallel().forEach(y -> IntStream.range(0, this.width).parallel()
            .forEach(x -> this.vectors[x][y] = this.getVector3D((double) x, y)));
    }

    /**
     * Get the point for a vector
     *
     * @param vector the vector for which the corresponding point on the camera plane will be returned
     * @return the point on the camera plane to which the given vector is mapped, nullable
     */
    @Nullable
    public Point getPoint(final Vector3D vector) {
        final Vector3D rotatedVector = rotate(vector, -1);
        // Currently set to false due to change in painting
        if (rotatedVector.getZ() < 0) {
            return null; // Ignores any points "behind the back", so they don't get painted a second time on the other
                         // side of the sphere
        }
        // This is a slightly faster than just doing the (brute force) method of Math.max(Math.min)). Reduces if
        // statements by 1 per call.
        final long x = Math
            .round((rotatedVector.getX() / rotatedVector.getZ()) * this.rotation.getRadialDistance() + width / 2d);
        final long y = Math
            .round((rotatedVector.getY() / rotatedVector.getZ()) * this.rotation.getRadialDistance() + height / 2d);

        try {
            return new Point(Math.toIntExact(x), Math.toIntExact(y));
        } catch (ArithmeticException e) {
            return new Point((int) Math.max(Integer.MIN_VALUE, Math.min(Integer.MAX_VALUE, x)),
                (int) Math.max(Integer.MIN_VALUE, Math.min(Integer.MAX_VALUE, y)));
        }
    }

    /**
     * Convert a point to a 3D vector
     *
     * @param p The point to convert
     * @return The vector
     */
    public Vector3D getVector3D(final Point p) {
        return this.getVector3D(p.x, p.y);
    }

    /**
     * Convert a point to a 3D vector
     *
     * @param x The x coordinate
     * @param y The y coordinate
     * @return The vector
     */
    public Vector3D getVector3D(final int x, final int y) {
        Vector3D res;
        try {
            res = rotate(vectors[x][y]);
        } catch (Exception e) {
            res = Vector3D.DEFAULT_VECTOR_3D;
        }
        return res;
    }

    /**
     * Convert a point to a 3D vector. Warning: This method does not cache.
     *
     * @param x The x coordinate
     * @param y The y coordinate
     * @return The vector (the middle of the image is 0, 0)
     */
    public Vector3D getVector3D(final double x, final double y) {
        return new Vector3D(x - width / 2d, y - height / 2d, this.rotation.getRadialDistance()).normalize();
    }

    /**
     * Set camera plane rotation by current plane position.
     *
     * @param p Point within current plane.
     */
    public void setRotation(final Point p) {
        setRotation(getVector3D(p));
    }

    /**
     * Set the rotation
     *
     * @param rotation Set the new rotation
     */
    public void setRotation(Rotation rotation) {
        this.rotation = rotation;
    }

    /**
     * Set the rotation from the difference of two points
     *
     * @param from The originating point
     * @param to The new point
     */
    public void setRotationFromDelta(final Point from, final Point to) {
        try {
            Vector3D f1 = vectors[from.x][from.y];
            Vector3D t1 = vectors[to.x][to.y];
            double deltaPolarAngle = f1.getPolarAngle() - t1.getPolarAngle();
            double deltaAzimuthalAngle = t1.getAzimuthalAngle() - f1.getAzimuthalAngle();
            double polarAngle = this.rotation.getPolarAngle() + deltaPolarAngle;
            double azimuthalAngle = this.rotation.getAzimuthalAngle() + deltaAzimuthalAngle;
            this.setRotation(azimuthalAngle, polarAngle);
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
        // TODO don't forget to re-enable the test!
        return this.rotation;
    }

    /**
     * Set the zoom
     *
     * @param zoom The zoom to set
     */
    public void setZoom(final double zoom) {
        this.zoom = zoom;
    }

    synchronized void setRotation(double azimuthalAngle, double polarAngle) {
        // Prevent us from going much outside 2pi
        if (polarAngle < 0) {
            polarAngle = polarAngle + TWO_PI;
        } else if (polarAngle > TWO_PI) {
            polarAngle = polarAngle - TWO_PI;
        }
        // Avoid flipping the camera
        if (azimuthalAngle > HALF_PI) {
            azimuthalAngle = HALF_PI;
        } else if (azimuthalAngle < -HALF_PI) {
            azimuthalAngle = -HALF_PI;
        }
        this.rotation = new Rotation(this.rotation.getRadialDistance(), polarAngle, azimuthalAngle);
    }

    private Vector3D rotate(final Vector3D vec) {
        return rotate(vec, 1);
    }

    private Vector3D rotate(final Vector3D vec, final int rotationFactor) {
        double vecX, vecY, vecZ;
        // Rotate around z axis first
        vecZ = vec.getZ() * this.rotation.getAzimuthalAngleCos() - vec.getY() * this.rotation.getAzimuthalAngleSin();
        vecY = vec.getZ() * this.rotation.getAzimuthalAngleSin() + vec.getY() * this.rotation.getAzimuthalAngleCos();
        vecX = vecZ * this.rotation.getPolarAngleSin() * rotationFactor + vec.getX() * this.rotation.getPolarAngleCos();
        vecZ = vecZ * this.rotation.getPolarAngleCos() - vec.getX() * this.rotation.getPolarAngleSin() * rotationFactor;
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
                    final Vector3D vec = getVector3D(x, y);
                    final Point2D.Double p = UVMapping.getTextureCoordinate(vec);
                    int tx = (int) (p.x * (sourceImage.getWidth() - 1));
                    int ty = (int) (p.y * (sourceImage.getHeight() - 1));
                    int color = sourceImageBuffer[ty * sourceImage.getWidth() + tx];
                    targetImageBuffer[y * targetImage.getWidth() + x] = color;
                }));
        } else {
            IntStream.range(0, targetImage.getHeight()).parallel()
                .forEach(y -> IntStream.range(0, targetImage.getWidth()).parallel().forEach(x -> {
                    final Vector3D vec = getVector3D(x, y);
                    final Point2D.Double p = UVMapping.getTextureCoordinate(vec);
                    targetImage.setRGB(x, y, sourceImage.getRGB((int) (p.x * (sourceImage.getWidth() - 1)),
                        (int) (p.y * (sourceImage.getHeight() - 1))));
                }));
        }
    }

}
