// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.layer;

import static com.github.tomakehurst.wiremock.core.WireMockConfiguration.wireMockConfig;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import javax.swing.Icon;

import com.github.tomakehurst.wiremock.WireMockServer;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.RegisterExtension;

import org.openstreetmap.josm.data.osm.DataSet;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.layer.OsmDataLayer;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.AwaitThreadFinish;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryKeys;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryTestRules;
import org.openstreetmap.josm.testutils.JOSMTestRules;
import org.openstreetmap.josm.testutils.annotations.BasicPreferences;
import org.openstreetmap.josm.tools.ImageProvider.ImageSizes;

@BasicPreferences
@AwaitThreadFinish
class PointObjectLayerTest {

  @RegisterExtension
  static JOSMTestRules rules = new MapillaryTestRules().timeout(20000).projection().main();

  private PointObjectLayer instance;
  private OsmDataLayer osm;

  private static WireMockServer wmRule;

  @BeforeAll
  static void beforeClass() {
    wmRule = new WireMockServer(wireMockConfig().dynamicPort());
    wmRule.start();
  }

  @AfterAll
  static void afterClass() {
    wmRule.stop();
  }

  @BeforeEach
  void setUp() {
    osm = new OsmDataLayer(new DataSet(), "Test", null);
    MainApplication.getLayerManager().addLayer(osm);
    instance = new PointObjectLayer(MapillaryKeys.MAPILLARY_POINT_OBJECTS);
  }

  @Test
  void testGetIcon() {
    Icon i = instance.getIcon();
    assertEquals(ImageSizes.LAYER.getAdjustedHeight(), i.getIconHeight());
    assertEquals(ImageSizes.LAYER.getAdjustedWidth(), i.getIconWidth());
  }

  @Test
  void testMergable() {
    assertFalse(instance.isMergable(null));
    instance.mergeFrom(null);
  }

  @Test
  void testInfoComponent() {
    assertNotNull(instance.getInfoComponent());
  }

  @Test
  void testTrivialMethods() {
    assertNotNull(instance.getToolTipText());
    instance.visitBoundingBox(null);
    assertEquals(10, instance.getMenuEntries().length);
  }
}
