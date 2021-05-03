// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.layer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.RegisterExtension;

import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.data.imagery.ImageryInfo;
import org.openstreetmap.josm.data.osm.DataSet;
import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.data.osm.Node;
import org.openstreetmap.josm.data.vector.VectorNode;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.layer.ImageryLayer;
import org.openstreetmap.josm.gui.layer.Layer;
import org.openstreetmap.josm.gui.layer.OsmDataLayer;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryTestRules;
import org.openstreetmap.josm.plugins.mapillary.utils.api.JsonImageDetailsDecoderTest;
import org.openstreetmap.josm.testutils.JOSMTestRules;

class MapillaryLayerTest {

  @RegisterExtension
  static JOSMTestRules rules = new MapillaryTestRules().main().preferences().projection();

  private static Layer getDummyLayer() {
    return ImageryLayer.create(new ImageryInfo("dummy", "https://example.org"));
  }

  @Test
  void testGetIcon() {
    assertNotNull(MapillaryLayer.getInstance().getIcon());
  }

  @Test
  void testIsMergable() {
    assertFalse(MapillaryLayer.getInstance().isMergable(getDummyLayer()));
  }

  @Test
  void testMergeFrom() {
    Layer dummyLayer = getDummyLayer();
    MapillaryLayer instance = MapillaryLayer.getInstance();
    assertThrows(UnsupportedOperationException.class, () -> instance.mergeFrom(dummyLayer));
  }

  @Test
  void testSetVisible() {
    MapillaryLayer.getInstance().getData()
      .addPrimitive(JsonImageDetailsDecoderTest.createImportedImage("", new LatLon(0.0, 0.0), 0.0, false));
    MapillaryLayer.getInstance().getData()
      .addPrimitive(JsonImageDetailsDecoderTest.createImportedImage("", new LatLon(0.0, 0.0), 0.0, false));
    VectorNode invisibleImage = JsonImageDetailsDecoderTest.createImportedImage("", new LatLon(0.0, 0.0), 0.0, false);
    invisibleImage.setVisible(false);
    MapillaryLayer.getInstance().getData().addPrimitive(invisibleImage);

    MapillaryLayer.getInstance().setVisible(false);
    for (INode img : MapillaryLayer.getInstance().getData().getNodes()) {
      if (MapillaryImageUtils.IS_IMAGE.test(img)) {
        assertFalse(img.isVisible());
      }
    }

    MapillaryLayer.getInstance().setVisible(true);
    for (INode img : MapillaryLayer.getInstance().getData().getNodes()) {
      if (MapillaryImageUtils.IS_IMAGE.test(img)) {
        assertTrue(img.isVisible());
      }
    }
  }

  @Test
  void testGetInfoComponent() {
    Object comp = MapillaryLayer.getInstance().getInfoComponent();
    assertTrue(comp instanceof String);
    assertTrue(((String) comp).length() >= 9);
  }

  @Test
  void testSetImageViewed() {
    INode image = JsonImageDetailsDecoderTest.createDownloadedImage("0", LatLon.ZERO, 0, false);
    assertFalse(MapillaryLayer.getInstance().setImageViewed(null),
      "An image should not be set as viewed if there is no image or dataset");
    assertFalse(MapillaryLayer.getInstance().setImageViewed(image),
      "An image should not be set as viewed if there is no dataset to edit");
    MainApplication.getLayerManager().addLayer(new OsmDataLayer(new DataSet(), "Test Layer", null));
    assertFalse(MapillaryLayer.getInstance().setImageViewed(null),
      "An image should not be set as viewed if there is no image (i.e., image is null)");
    assertTrue(MapillaryLayer.getInstance().setImageViewed(image),
      "An image should be set as viewed if there is an image and a dataset");
  }

  @Test
  void testGetChangesetSourceTag() {
    String actualChangesetSourceTag = MapillaryLayer.getInstance().getChangesetSourceTag();
    assertNull(actualChangesetSourceTag,
      "OpenStreetmap changeset source for Mapillary layer should be 'null' when there is no dataset");
    DataSet ds = new DataSet();
    Node node = new Node(LatLon.ZERO);
    ds.addPrimitive(node);
    node.setModified(true);
    INode image = JsonImageDetailsDecoderTest.createDownloadedImage("0", LatLon.ZERO, 0, false);
    MainApplication.getLayerManager().addLayer(new OsmDataLayer(ds, "Test Layer", null));
    MapillaryLayer.getInstance().setImageViewed(image);
    actualChangesetSourceTag = MapillaryLayer.getInstance().getChangesetSourceTag();
    assertEquals("Mapillary", actualChangesetSourceTag,
      "OpenStreetmap changeset source for Mapillary layer should be 'Mapillary'");
    node.setCoor(LatLon.NORTH_POLE);
    actualChangesetSourceTag = MapillaryLayer.getInstance().getChangesetSourceTag();
    assertNull(actualChangesetSourceTag,
      "OpenStreetmap changeset source for Mapillary layer should be 'null' when the viewed images are very far from the modified objects");
    node.setCoor(new LatLon(0.0049, 0.0049));
    actualChangesetSourceTag = MapillaryLayer.getInstance().getChangesetSourceTag();
    assertNull(actualChangesetSourceTag,
      "OpenStreetmap changeset source for Mapillary layer should be 'null' when the viewed images are more than 30.0m away (default)");
  }

  @Test
  void testClearInstance() {
    MapillaryLayer.getInstance();
    assertTrue(MapillaryLayer.hasInstance());
    MapillaryLayer.getInstance().destroy();
    assertFalse(MapillaryLayer.hasInstance());
    MapillaryLayer.getInstance();
    assertTrue(MapillaryLayer.hasInstance());
  }
}
