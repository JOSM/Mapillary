package org.openstreetmap.josm.plugins.mapillary.traffico;

import static org.junit.Assert.assertEquals;

import java.lang.reflect.Constructor;

import org.junit.Test;

public class TrafficoGlyphTest {

  @Test
  public void testGetGlyph() {
    // The following line has to be updated everytime a new version of traffico is included
    assertEquals(new Character('\uf105'), TrafficoGlyph.getGlyph("airplane"));
    assertEquals(null, TrafficoGlyph.getGlyph("some-nonexistent-glyph"));
    assertEquals(null, TrafficoGlyph.getGlyph(null));
  }

  /**
   * The following test has the only purpose to provide code coverage for the private constructor.
   */
  @Test
  public void testPrivateConstructor() throws Exception {
    Constructor<?> c = TrafficoGlyph.class.getDeclaredConstructors()[0];
    c.setAccessible(true);
    c.newInstance();
  }

}
