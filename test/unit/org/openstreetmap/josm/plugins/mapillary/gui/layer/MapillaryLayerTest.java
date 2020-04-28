// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.layer;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.File;

import org.junit.Rule;
import org.junit.Test;

import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.data.imagery.ImageryInfo;
import org.openstreetmap.josm.data.osm.DataSet;
import org.openstreetmap.josm.data.osm.Node;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.layer.ImageryLayer;
import org.openstreetmap.josm.gui.layer.Layer;
import org.openstreetmap.josm.gui.layer.OsmDataLayer;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImportedImage;
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
    MapillaryLayer.getInstance().getData().add(new MapillaryImportedImage(new LatLon(0.0, 0.0), 0.0, new File(""), false));
    MapillaryLayer.getInstance().getData().add(new MapillaryImportedImage(new LatLon(0.0, 0.0), 0.0, new File(""), false));
    MapillaryImportedImage invisibleImage = new MapillaryImportedImage(new LatLon(0.0, 0.0), 0.0, new File(""), false);
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
  public void testSetImageViewed() {
    MapillaryImage image = new MapillaryImage("0", LatLon.ZERO, 0, false, false);
    assertFalse("An image should not be set as viewed if there is no image or dataset",
        MapillaryLayer.getInstance().setImageViewed(null));
    assertFalse("An image should not be set as viewed if there is no dataset to edit",
        MapillaryLayer.getInstance().setImageViewed(image));
    MainApplication.getLayerManager().addLayer(new OsmDataLayer(new DataSet(), "Test Layer", null));
    assertFalse("An image should not be set as viewed if there is no image (i.e., image is null)",
        MapillaryLayer.getInstance().setImageViewed(null));
    assertTrue("An image should be set as viewed if there is an image and a dataset",
        MapillaryLayer.getInstance().setImageViewed(image));
  }

  @Test
  public void testGetChangesetSourceTag() {
    String actualChangesetSourceTag = MapillaryLayer.getInstance().getChangesetSourceTag();
    assertNull("OpenStreetmap changeset source for Mapillary layer should be 'null' when there is no dataset",
        actualChangesetSourceTag);
    DataSet ds = new DataSet();
    Node node = new Node(LatLon.ZERO);
    ds.addPrimitive(node);
    node.setModified(true);
    MapillaryImage image = new MapillaryImage("0", LatLon.ZERO, 0, false, false);
    MainApplication.getLayerManager().addLayer(new OsmDataLayer(ds, "Test Layer", null));
    MapillaryLayer.getInstance().setImageViewed(image);
    actualChangesetSourceTag = MapillaryLayer.getInstance().getChangesetSourceTag();
    assertEquals("OpenStreetmap changeset source for Mapillary layer should be 'Mapillary'", "Mapillary", actualChangesetSourceTag);
    node.setCoor(LatLon.NORTH_POLE);
    actualChangesetSourceTag = MapillaryLayer.getInstance().getChangesetSourceTag();
    assertNull(
        "OpenStreetmap changeset source for Mapillary layer should be 'null' when the viewed images are very far from the modified objects",
        actualChangesetSourceTag);
    node.setCoor(new LatLon(0.0049, 0.0049));
    actualChangesetSourceTag = MapillaryLayer.getInstance().getChangesetSourceTag();
    assertNull(
        "OpenStreetmap changeset source for Mapillary layer should be 'null' when the viewed images are more than 30.0m away (default)",
        actualChangesetSourceTag);
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
