// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;

import org.junit.Rule;
import org.junit.Test;

import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.data.imagery.ImageryInfo;
import org.openstreetmap.josm.gui.layer.ImageryLayer;
import org.openstreetmap.josm.gui.layer.Layer;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil.MapillaryTestRules;
import org.openstreetmap.josm.testutils.JOSMTestRules;

public class MapillaryLayerTest {

  @Rule
  public JOSMTestRules rules = new MapillaryTestRules().main().preferences().projection();

  private static Layer getDummyLayer() {
    return ImageryLayer.create(new ImageryInfo("dummy", "https://example.org"));
  }

  @Test
  public void testGetIcon() {
    assertNotNull(MapillaryLayer.getInstance().getIcon());
  }

  @Test
  public void testIsMergable() {
    assertFalse(MapillaryLayer.getInstance().isMergable(getDummyLayer()));
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testMergeFrom() {
    MapillaryLayer.getInstance().mergeFrom(getDummyLayer());
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
      assertFalse(img.isVisible());
    }


    MapillaryLayer.getInstance().setVisible(true);
    for (MapillaryAbstractImage img : MapillaryLayer.getInstance().getData().getImages()) {
      assertTrue(img.isVisible());
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
    MapillaryLayer.getInstance().destroy();
    assertFalse(MapillaryLayer.hasInstance());
    MapillaryLayer.getInstance();
    assertTrue(MapillaryLayer.hasInstance());
  }
}
