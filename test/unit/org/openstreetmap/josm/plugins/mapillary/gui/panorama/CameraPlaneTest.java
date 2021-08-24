// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.panorama;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.awt.Point;
import java.awt.geom.Point2D;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.RegisterExtension;
import org.openstreetmap.josm.testutils.JOSMTestRules;
import org.openstreetmap.josm.testutils.annotations.BasicPreferences;

@BasicPreferences
class CameraPlaneTest {

    @RegisterExtension
    JOSMTestRules rule = new JOSMTestRules().main();

    private CameraPlane cameraPlane;
    private static final double FOV = Math.toRadians(110);
    private static final double CAMERA_PLANE_DISTANCE = (800 / 2) / Math.tan(FOV / 2);

    @Test
    @Disabled("Apache math + custom code is not currently playing well together")
    void testSetRotation() {
        cameraPlane = new CameraPlane(800, 400, CAMERA_PLANE_DISTANCE);
        Vector3D vec = new Vector3D(0, 0, 1);
        cameraPlane.setRotation(vec);
        Rotation out = cameraPlane.getRotation();
        assertEquals(0, out.getRadialDistance(), 0.001);
        assertEquals(0, out.getPolarAngle(), 0.001);
        assertEquals(1, out.getAzimuthalAngle(), 0.001);
    }

    @Test
    void testGetVector3D() {
        cameraPlane = new CameraPlane(800, 600, CAMERA_PLANE_DISTANCE);
        Vector3D vec = new Vector3D(0, 0, 1);
        cameraPlane.setRotation(vec);
        Vector3D out = cameraPlane.getVector3D(new Point(400, 300));
        assertEquals(0.0, out.getX(), 1.0E-04);
        assertEquals(0.0, out.getY(), 1.0E-04);
        assertEquals(1.0, out.getZ(), 1.0E-04);
    }

    @Test
    void testMapping() {
        cameraPlane = new CameraPlane(800, 600, CAMERA_PLANE_DISTANCE);
        Vector3D vec = new Vector3D(0, 0, 1);
        cameraPlane.setRotation(vec);
        Vector3D out = cameraPlane.getVector3D(new Point(300, 200));
        Point2D map = UVMapping.getTextureCoordinate(out);
        assertEquals(0.44542099, map.getX(), 1e-8);
        assertEquals(0.39674936, map.getY(), 1e-8);
    }
}
