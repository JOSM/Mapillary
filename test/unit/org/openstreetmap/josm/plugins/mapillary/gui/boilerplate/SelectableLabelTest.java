// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.boilerplate;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class SelectableLabelTest {
  @Test
  public void test() {
    SelectableLabel l1 = new SelectableLabel();
    assertFalse(l1.isEditable());
    SelectableLabel l2 = new SelectableLabel("some text");
    assertTrue(l2.getText().contains("some text"));
    assertFalse(l2.isEditable());

  }
}
