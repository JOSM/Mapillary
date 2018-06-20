// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui;

import java.awt.Point;
import java.util.stream.IntStream;

class VectorUtil {
    private Vector3D[][] vectors;
    private Rotation rotation;

    VectorUtil() {
        rotation = new Rotation(0, 0);
    }

    void setCameraScreen(int width, int height, double d) {
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

    Vector3D getVector3D(int x, int y) {
        Vector3D res;
        try {
            res = rotation.rotate(vectors[x][y]);
        } catch (Exception e) {
            res =  new Vector3D(0,0,1);
        }
        return res;
    }

    void setRotation(Vector3D vec) {
        double theta, phi;
        try {
            theta = Math.atan2(vec.getX(), vec.getZ());
            phi = Math.atan2(vec.getY(), Math.sqrt(vec.getX()*vec.getX() + vec.getZ()*vec.getZ()));
        } catch (Exception e) {
            theta = 0; phi = 0;
        }
        rotation.setTheta(theta);
        rotation.setPhi(phi);
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
