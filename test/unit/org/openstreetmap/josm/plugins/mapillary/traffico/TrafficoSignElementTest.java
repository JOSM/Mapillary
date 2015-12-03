package org.openstreetmap.josm.plugins.mapillary.traffico;

import static org.junit.Assert.*;

import java.awt.Color;

import org.junit.Test;

public class TrafficoSignElementTest {

  @Test
  public void testPersistence() {
    TrafficoSignElement tse1 = new TrafficoSignElement('\udead', Color.BLACK);
    TrafficoSignElement tse2 = new TrafficoSignElement('\ubeef', Color.RED);
    assertEquals('\udead', tse1.getGlyph());
    assertEquals('\ubeef', tse2.getGlyph());
    assertEquals(Color.BLACK, tse1.getColor());
    assertEquals(Color.RED, tse2.getColor());
  }

  @Test(expected=IllegalArgumentException.class)
  public void testNullColor() {
    new TrafficoSignElement('\ufeed', null);
  }

}
