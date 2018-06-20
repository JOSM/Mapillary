// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui;

class Rotation {
    private double theta;
    private double sinTheta;
    private double cosTheta;
    private double phi;
    private double sinPhi;
    private double cosPhi;

    Rotation(double theta, double phi) {
        setTheta(theta);
        setPhi(phi);
    }

    double getTheta() {
        return theta;
    }

    void setTheta(double theta) {
        this.theta = theta;
        this.sinTheta = Math.sin(theta);
        this.cosTheta = Math.cos(theta);
    }

    double getPhi() {
        return phi;
    }

    void setPhi(double phi) {
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
}
