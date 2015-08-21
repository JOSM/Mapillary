package org.openstreetmap.josm.plugins.mapillary.traffico;

import java.awt.Color;

public class TrafficoSignElement {

  private Color color;
  private char glyph;

  public TrafficoSignElement(char glyph, Color c) {
    if (c == null) {
      throw new IllegalArgumentException();
    }
    this.color = c;
    this.glyph = glyph;
  }

  /**
   * @return the color
   */
  public Color getColor() {
    return this.color;
  }

  /**
   * @return the glyph
   */
  public char getGlyph() {
    return this.glyph;
  }
}
