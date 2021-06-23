// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.layer;

import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static com.github.tomakehurst.wiremock.client.WireMock.get;
import static com.github.tomakehurst.wiremock.client.WireMock.urlMatching;
import static com.github.tomakehurst.wiremock.core.WireMockConfiguration.wireMockConfig;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Paths;

import javax.swing.Icon;

import com.github.tomakehurst.wiremock.WireMockServer;
import com.github.tomakehurst.wiremock.matching.EqualToPattern;
import org.awaitility.Awaitility;
import org.awaitility.Durations;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.RegisterExtension;

import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.data.DataSource;
import org.openstreetmap.josm.data.osm.DataSet;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.layer.OsmDataLayer;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryKeys;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryTestRules;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil;
import org.openstreetmap.josm.testutils.JOSMTestRules;
import org.openstreetmap.josm.tools.ImageProvider.ImageSizes;

class PointObjectLayerTest {

  @RegisterExtension
  static JOSMTestRules rules = new MapillaryTestRules().timeout(20000).projection().main();

  private static String oldBaseUrl;

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
    oldBaseUrl = TestUtil.getApiV3BaseUrl();
    TestUtil.setAPIv3BaseUrl("http://localhost:" + wmRule.port() + "/");
    osm = new OsmDataLayer(new DataSet(), "Test", null);
    MainApplication.getLayerManager().addLayer(osm);
    instance = new PointObjectLayer(MapillaryKeys.MAPILLARY_POINT_OBJECTS);
  }

  @AfterEach
  void cleanUp() throws IllegalArgumentException {
    TestUtil.setAPIv3BaseUrl(oldBaseUrl);
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
