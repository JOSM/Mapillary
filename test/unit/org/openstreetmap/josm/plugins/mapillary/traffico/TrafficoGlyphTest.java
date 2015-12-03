package org.openstreetmap.josm.plugins.mapillary.traffico;

import static org.junit.Assert.assertEquals;

import org.junit.Test;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil;

public class TrafficoGlyphTest {

  @Test
  public void testGetGlyph() {
    // The following line has to be updated everytime a new version of traffico is included
    assertEquals(new Character('\uf105'), TrafficoGlyph.getGlyph("airplane"));
    assertEquals(null, TrafficoGlyph.getGlyph("some-nonexistent-glyph"));
    assertEquals(null, TrafficoGlyph.getGlyph(null));
  }

  @Test
  public void testUtilityClass() {
    TestUtil.testUtilityClass(TrafficoGlyph.class);
  }

}
