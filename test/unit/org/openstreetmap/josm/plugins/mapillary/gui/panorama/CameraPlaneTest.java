// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.panorama;

import static org.junit.Assert.assertEquals;

import java.awt.Point;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;

import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil.MapillaryTestRules;
import org.openstreetmap.josm.testutils.JOSMTestRules;

public class CameraPlaneTest {

  private CameraPlane cameraPlane;
  private static final double FOV = Math.toRadians(110);
  private static final double CAMERA_PLANE_DISTANCE = (800 / 2) / Math.tan(FOV / 2);

  @Rule
  public JOSMTestRules rules = new MapillaryTestRules().platform();

  @Before
  public void setUp() {
    cameraPlane = new CameraPlane();
  }

  @Test
  public void testSetRotation() {
    Vector3D vec = new Vector3D(0, 0, 1);
    cameraPlane.setRotation(vec);
    Vector3D out = cameraPlane.getRotation();
    assertEquals(0, out.getX(), 0.001);
    assertEquals(0, out.getY(), 0.001);
    assertEquals(1, out.getZ(), 0.001);
  }

  @Test
  public void testGetVector3D() {
    cameraPlane.setCameraPlane(800, 600, CAMERA_PLANE_DISTANCE);
    Vector3D vec = new Vector3D(0, 0, 1);
    cameraPlane.setRotation(vec);
    Vector3D out = cameraPlane.getVector3D(400, 300);
    assertEquals(0.0, out.getX(), 1.0E-04);
    assertEquals(0.0, out.getY(), 1.0E-04);
    assertEquals(1.0, out.getZ(), 1.0E-04);
  }

  @Test
  public void testMapping() {
    cameraPlane.setCameraPlane(800, 600, CAMERA_PLANE_DISTANCE);
    Vector3D vec = new Vector3D(0, 0, 1);
    cameraPlane.setRotation(vec);
    Vector3D out = cameraPlane.getVector3D(300, 200);
    Point map = cameraPlane.mapping(out, 2048, 1024);
    assertEquals(911, map.getX(), 1);
    assertEquals(405, map.getY(), 1);
  }
}

