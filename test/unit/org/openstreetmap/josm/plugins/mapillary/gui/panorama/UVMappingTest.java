package org.openstreetmap.josm.plugins.mapillary.gui.panorama;

import static org.junit.Assert.assertEquals;

import java.awt.geom.Point2D;

import org.junit.Test;

public class UVMappingTest {
  private static final double DEFAULT_DELTA = 1e-5;

  @Test
  public void testMapping() {
    assertPointEquals(new Point2D.Double(.5, 1), UVMapping.getTextureCoordinate(new Vector3D(0, 1, 0)), DEFAULT_DELTA);
    assertPointEquals(new Point2D.Double(.5, 0), UVMapping.getTextureCoordinate(new Vector3D(0, -1, 0)), DEFAULT_DELTA);
    assertVectorEquals(new Vector3D(0, 1, 0), UVMapping.getVector(.5, 1), DEFAULT_DELTA);
    assertVectorEquals(new Vector3D(0, -1, 0), UVMapping.getVector(.5, 0), DEFAULT_DELTA);

    assertPointEquals(new Point2D.Double(.25, .5), UVMapping.getTextureCoordinate(new Vector3D(-1, 0, 0)), DEFAULT_DELTA);
    assertPointEquals(new Point2D.Double(.5, .5), UVMapping.getTextureCoordinate(new Vector3D(0, 0, 1)), DEFAULT_DELTA);
    assertPointEquals(new Point2D.Double(.75, .5), UVMapping.getTextureCoordinate(new Vector3D(1, 0, 0)), DEFAULT_DELTA);
    assertPointEquals(new Point2D.Double(1, .5), UVMapping.getTextureCoordinate(new Vector3D(0, 0, -1)), DEFAULT_DELTA);
    assertVectorEquals(new Vector3D(-1, 0, 0), UVMapping.getVector(.25, .5), DEFAULT_DELTA);
    assertVectorEquals(new Vector3D(0, 0, 1), UVMapping.getVector(.5, .5), DEFAULT_DELTA);
    assertVectorEquals(new Vector3D(1, 0, 0), UVMapping.getVector(.75, .5), DEFAULT_DELTA);
    assertVectorEquals(new Vector3D(0, 0, -1), UVMapping.getVector(1, .5), DEFAULT_DELTA);

    assertPointEquals(new Point2D.Double(.125, .25), UVMapping.getTextureCoordinate(new Vector3D(-.5, -1 / Math.sqrt(2), -.5)), DEFAULT_DELTA);
    assertPointEquals(new Point2D.Double(.625, .75), UVMapping.getTextureCoordinate(new Vector3D(.5, 1 / Math.sqrt(2), .5)), DEFAULT_DELTA);
    assertVectorEquals(new Vector3D(-.5, -1 / Math.sqrt(2), -.5), UVMapping.getVector(.125, .25), DEFAULT_DELTA);
    assertVectorEquals(new Vector3D(.5, 1 / Math.sqrt(2), .5), UVMapping.getVector(.625, .75), DEFAULT_DELTA);
  }

  private static void assertVectorEquals(final Vector3D expected, final Vector3D actual, final double delta) {
    final String message = String.format(
      "Expected (%f %f %f), but was (%f %f %f)",
      expected.getX(), expected.getY(), expected.getZ(),
      actual.getX(), actual.getY(), actual.getZ()
    );
    assertEquals(message, expected.getX(), actual.getX(), delta);
    assertEquals(message, expected.getY(), actual.getY(), delta);
    assertEquals(message, expected.getZ(), actual.getZ(), delta);
  }

  private static void assertPointEquals(final Point2D expected, final Point2D actual, final double delta) {
    final String message = String.format(
      "Expected (%f, %f), but was (%f, %f)",
      expected.getX(), expected.getY(),
      actual.getX(), actual.getY()
    );
    assertEquals(message, expected.getX(), actual.getX(), delta);
    assertEquals(message, expected.getY(), actual.getY(), delta);
  }
}
