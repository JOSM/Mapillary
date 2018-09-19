// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.panorama;

import static org.junit.Assert.assertEquals;

import java.awt.Point;
import java.awt.geom.Point2D;

import org.junit.Rule;
import org.junit.Test;

import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil.MapillaryTestRules;
import org.openstreetmap.josm.testutils.JOSMTestRules;

public class CameraPlaneTest {

  private CameraPlane cameraPlane;
  private static final double FOV = Math.toRadians(110);
  private static final double CAMERA_PLANE_DISTANCE = (800 / 2) / Math.tan(FOV / 2);

  @Rule
  public JOSMTestRules rules = new MapillaryTestRules();

  @Test
  public void testSetRotation() {
    cameraPlane = new CameraPlane(800, 400, CAMERA_PLANE_DISTANCE);
    Vector3D vec = new Vector3D(0, 0, 1);
    cameraPlane.setRotation(vec);
    Vector3D out = cameraPlane.getRotation();
    assertEquals(0, out.getX(), 0.001);
    assertEquals(0, out.getY(), 0.001);
    assertEquals(1, out.getZ(), 0.001);
  }

  @Test
  public void testGetVector3D() {
    cameraPlane = new CameraPlane(800, 600, CAMERA_PLANE_DISTANCE);
    Vector3D vec = new Vector3D(0, 0, 1);
    cameraPlane.setRotation(vec);
    Vector3D out = cameraPlane.getVector3D(new Point(400, 300));
    assertEquals(0.0, out.getX(), 1.0E-04);
    assertEquals(0.0, out.getY(), 1.0E-04);
    assertEquals(1.0, out.getZ(), 1.0E-04);
  }

  @Test
  public void testMapping() {
    cameraPlane = new CameraPlane(800, 600, CAMERA_PLANE_DISTANCE);
    Vector3D vec = new Vector3D(0, 0, 1);
    cameraPlane.setRotation(vec);
    Vector3D out = cameraPlane.getVector3D(new Point(300, 200));
    Point2D map = UVMapping.getTextureCoordinate(out);
    assertEquals(0.44542099, map.getX(), 1e-8);
    assertEquals(0.39674936, map.getY(), 1e-8);
  }
}

