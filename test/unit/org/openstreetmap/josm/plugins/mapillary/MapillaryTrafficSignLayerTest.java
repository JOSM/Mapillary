package org.openstreetmap.josm.plugins.mapillary;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;

import java.io.IOException;

import org.junit.Test;
import org.openstreetmap.josm.data.imagery.ImageryInfo;
import org.openstreetmap.josm.gui.layer.ImageryLayer;
import org.openstreetmap.josm.gui.layer.Layer;

public class MapillaryTrafficSignLayerTest extends AbstractTest {
  private Layer dummyLayer = ImageryLayer.create(new ImageryInfo());

  @Test
  public void test() throws IOException {
    assertFalse(MapillaryTrafficSignLayer.getInstance().isModified());
    assertFalse(MapillaryTrafficSignLayer.getInstance().isMergable(dummyLayer));
    assertFalse(MapillaryTrafficSignLayer.getInstance().isMergable(MapillaryTrafficSignLayer.getInstance()));
    assertEquals(MapillaryPlugin.ICON16, MapillaryTrafficSignLayer.getInstance().getIcon());
    assertEquals(null, MapillaryTrafficSignLayer.getInstance().getToolTipText());
    assertNotEquals(null, MapillaryTrafficSignLayer.getInstance().getInfoComponent());
  }
}
