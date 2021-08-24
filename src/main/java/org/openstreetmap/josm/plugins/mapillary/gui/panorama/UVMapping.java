package org.openstreetmap.josm.plugins.mapillary.gui.panorama;

import java.awt.geom.Point2D;

public final class UVMapping {
    private UVMapping() {
        // Private constructor to avoid instantiation
    }

    /**
     * Returns the point of the texture image that is mapped to the given point in 3D space (given as {@link Vector3D})
     * See <a href="https://en.wikipedia.org/wiki/UV_mapping">the Wikipedia article on UV mapping</a>.
     *
     * @param vector the vector to which the texture point is mapped
     * @return a point on the texture image somewhere in the rectangle between (0, 0) and (1, 1)
     */
    public static Point2D.Double getTextureCoordinate(final Vector3D vector) {
        final double u = 0.5 + (Math.atan2(vector.getX(), vector.getZ()) / (CameraPlane.TWO_PI));
        final double v = 0.5 + (Math.asin(vector.getY()) / Math.PI);
        return new Point2D.Double(u, v);
    }

    /**
     * For a given point of the texture (i.e. the image), return the point in 3D space where the point
     * of the texture is mapped to (as {@link Vector3D}).
     *
     * @param u x-coordinate of the point on the texture (in the range between 0 and 1, from left to right)
     * @param v y-coordinate of the point on the texture (in the range between 0 and 1, from top to bottom)
     * @return the vector from the origin to where the point of the texture is mapped on the sphere
     */
    public static Vector3D getVector(final double u, final double v) {
        final double vectorY = Math.cos(v * Math.PI);
        final double vectorYSquared = Math.pow(vectorY, 2);
        return new Vector3D(-Math.sin(CameraPlane.TWO_PI * u) * Math.sqrt(1 - vectorYSquared), -vectorY,
            -Math.cos(CameraPlane.TWO_PI * u) * Math.sqrt(1 - vectorYSquared));
    }
}
