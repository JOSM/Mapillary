package org.openstreetmap.josm.plugins.mapillary;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.File;

import org.junit.Test;
import org.openstreetmap.josm.data.coor.LatLon;
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

  @Test(expected = UnsupportedOperationException.class)
  public void testMergeFrom() {
    MapillaryLayer.getInstance().mergeFrom(dummyLayer);
  }

  @Test
  public void testSetVisible() {
    MapillaryLayer.getInstance().getData().add(new MapillaryImportedImage(new LatLon(0.0, 0.0), 0.0, new File("")));
    MapillaryLayer.getInstance().getData().add(new MapillaryImportedImage(new LatLon(0.0, 0.0), 0.0, new File("")));
    MapillaryImportedImage invisibleImage = new MapillaryImportedImage(new LatLon(0.0, 0.0), 0.0, new File(""));
    invisibleImage.setVisible(false);
    MapillaryLayer.getInstance().getData().add(invisibleImage);

    MapillaryLayer.getInstance().setVisible(false);
    for (MapillaryAbstractImage img : MapillaryLayer.getInstance().getData().getImages()) {
      assertEquals(false, img.isVisible());
    }


    MapillaryLayer.getInstance().setVisible(true);
    for (MapillaryAbstractImage img : MapillaryLayer.getInstance().getData().getImages()) {
      assertEquals(true, img.isVisible());
    }
  }

  @Test
  public void testGetInfoComponent() {
    Object comp = MapillaryLayer.getInstance().getInfoComponent();
    assertTrue(comp instanceof String);
    assertTrue(((String) comp).length() >= 9);
  }

  @Test
  public void testClearInstance() {
    MapillaryLayer.getInstance();
    assertTrue(MapillaryLayer.hasInstance());
    MapillaryLayer.clearInstance();
    assertFalse(MapillaryLayer.hasInstance());
    MapillaryLayer.getInstance();
    assertTrue(MapillaryLayer.hasInstance());
  }
}
