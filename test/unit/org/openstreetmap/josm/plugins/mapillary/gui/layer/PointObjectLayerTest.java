// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.layer;

import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static com.github.tomakehurst.wiremock.client.WireMock.get;
import static com.github.tomakehurst.wiremock.client.WireMock.stubFor;
import static com.github.tomakehurst.wiremock.client.WireMock.urlMatching;
import static com.github.tomakehurst.wiremock.core.WireMockConfiguration.wireMockConfig;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;

import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Paths;

import javax.swing.Icon;

import com.github.tomakehurst.wiremock.junit.WireMockRule;
import com.github.tomakehurst.wiremock.matching.EqualToPattern;
import org.awaitility.Awaitility;
import org.awaitility.Durations;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;

import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.data.DataSource;
import org.openstreetmap.josm.data.osm.DataSet;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.layer.OsmDataLayer;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil.MapillaryTestRules;
import org.openstreetmap.josm.testutils.JOSMTestRules;
import org.openstreetmap.josm.tools.ImageProvider.ImageSizes;

public class PointObjectLayerTest {

  @Rule
  public WireMockRule wmRule = new WireMockRule(wireMockConfig().dynamicPort());

  @Rule
  public JOSMTestRules rules = new MapillaryTestRules().timeout(20000).projection().main();

  private static String oldBaseUrl;

  private PointObjectLayer instance;
  private OsmDataLayer osm;

  @Before
  public void setUp() {
    oldBaseUrl = TestUtil.getApiV3BaseUrl();
    TestUtil.setAPIv3BaseUrl("http://localhost:" + wmRule.port() + "/");
    osm = new OsmDataLayer(new DataSet(), "Test", null);
    MainApplication.getLayerManager().addLayer(osm);
    instance = new PointObjectLayer(false);
  }

  @After
  public void cleanUp() throws IllegalArgumentException {
    TestUtil.setAPIv3BaseUrl(oldBaseUrl);
  }

  @Test
  public void testScheduleDownload() throws URISyntaxException, IOException {
    stubFor(
      get(urlMatching("/map_features\\?.+"))
        .withQueryParam("client_id", new EqualToPattern("UTZhSnNFdGpxSEFFREUwb01GYzlXZzpjNGViMzQxMTIzMjY0MjZm"))
        .withQueryParam("bbox", new EqualToPattern("1.0,1.0,1.0,1.0"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(Files.readAllBytes(
              Paths.get(PointObjectLayerTest.class.getResource("/api/v3/responses/searchMapObjects.json").toURI())))));
    stubFor(get(urlMatching("/me\\?.+"))
      .withQueryParam("client_id", new EqualToPattern("UTZhSnNFdGpxSEFFREUwb01GYzlXZzpjNGViMzQxMTIzMjY0MjZm"))
      .willReturn(aResponse().withStatus(404)));

    osm.getDataSet().addDataSource(new DataSource(new Bounds(1, 1, 1, 1), "1/1/1/1"));
    // Wait for a maximum of 5 sec for a result
    Awaitility.await().atMost(Durations.FIVE_SECONDS).until(() -> instance.getDataSet().allPrimitives().size() != 0);
    assertEquals(1, instance.getDataSet().allPrimitives().size());
  }

  @Test
  public void testGetIcon() {
    Icon i = instance.getIcon();
    assertEquals(ImageSizes.LAYER.getAdjustedHeight(), i.getIconHeight());
    assertEquals(ImageSizes.LAYER.getAdjustedWidth(), i.getIconWidth());
  }

  @Test
  public void testMergable() {
    assertFalse(instance.isMergable(null));
    instance.mergeFrom(null);
  }

  @Test
  public void testInfoComponent() {
    assertNotNull(instance.getInfoComponent());
  }

  @Test
  public void testTrivialMethods() {
    assertNotNull(instance.getToolTipText());
    instance.visitBoundingBox(null);
    assertEquals(10, instance.getMenuEntries().length);
  }
}
