package org.openstreetmap.josm.plugins.mapillary;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

import org.junit.Test;
import org.openstreetmap.josm.data.imagery.ImageryInfo;
import org.openstreetmap.josm.gui.layer.ImageryLayer;
import org.openstreetmap.josm.gui.layer.Layer;

public class MapillaryLayerTest extends AbstractTest {
  private Layer dummyLayer = ImageryLayer.create(new ImageryInfo());

  @Test
  public void testGetIcon() {
    assertEquals(MapillaryPlugin.ICON16, MapillaryLayer.getInstance().getIcon());
  }

  @Test
  public void testIsMergable() {
    assertFalse(MapillaryLayer.getInstance().isMergable(dummyLayer));
  }

  @Test(expected=UnsupportedOperationException.class)
  public void testMergeFrom() {
    MapillaryLayer.getInstance().mergeFrom(dummyLayer);
  }
}
