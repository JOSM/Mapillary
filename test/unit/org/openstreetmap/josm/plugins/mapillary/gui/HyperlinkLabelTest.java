package org.openstreetmap.josm.plugins.mapillary.gui;

import static org.junit.Assert.assertEquals;
import static org.openstreetmap.josm.tools.I18n.tr;

import org.junit.Test;

public class HyperlinkLabelTest {

  @Test
  public void testTextPersistence() {
    String testString1 = "Some string";
    String testString2 = "this is something else";
    HyperlinkLabel label = new HyperlinkLabel();
    assertEquals(tr("View in website"), label.getNormalText());
    label.setText(testString1);
    assertEquals(testString1, label.getNormalText());
    label.setText(testString2);
    assertEquals(testString2, label.getNormalText());
  }

}
