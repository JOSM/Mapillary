// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.boilerplate;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

class SelectableLabelTest {
  @Test
  void test() {
    SelectableLabel l1 = new SelectableLabel();
    assertFalse(l1.isEditable());
    SelectableLabel l2 = new SelectableLabel("some text");
    assertTrue(l2.getText().contains("some text"));
    assertFalse(l2.isEditable());

  }
}
