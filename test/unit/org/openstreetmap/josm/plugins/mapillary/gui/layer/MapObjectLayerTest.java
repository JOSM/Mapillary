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
import static org.junit.Assert.assertNull;

import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Paths;

import javax.swing.Icon;

import com.github.tomakehurst.wiremock.junit.WireMockRule;
import com.github.tomakehurst.wiremock.matching.EqualToPattern;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;

import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapObjectLayer.STATUS;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil.MapillaryTestRules;
import org.openstreetmap.josm.testutils.JOSMTestRules;
import org.openstreetmap.josm.tools.ImageProvider.ImageSizes;

public class MapObjectLayerTest {

  @Rule
  public WireMockRule wmRule = new WireMockRule(wireMockConfig().dynamicPort());

  @Rule
  public JOSMTestRules rules = new MapillaryTestRules().timeout(20000);

  private static String oldBaseUrl;

  @Before
  public void setUp() {
    oldBaseUrl = TestUtil.getApiV3BaseUrl();
    TestUtil.setAPIv3BaseUrl("http://localhost:" + wmRule.port() + "/");
  }

  @After
  public void cleanUp() throws IllegalArgumentException {
    TestUtil.setAPIv3BaseUrl(oldBaseUrl);
  }

  @Test
  public void testStatusEnum() {
    assertEquals(4, STATUS.values().length);
    assertEquals(STATUS.COMPLETE, STATUS.valueOf("COMPLETE"));
  }

  @Test
  public void testScheduleDownload() throws InterruptedException, URISyntaxException, IOException {
    stubFor(
      get(urlMatching("/map_features\\?.+"))
        .withQueryParam("client_id", new EqualToPattern("T1Fzd20xZjdtR0s1VDk5OFNIOXpYdzoxNDYyOGRkYzUyYTFiMzgz"))
        .withQueryParam("bbox", new EqualToPattern("1.0,1.0,1.0,1.0"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(Files.readAllBytes(
              Paths.get(MapObjectLayerTest.class.getResource("/api/v3/responses/searchMapObjects.json").toURI())
            ))
        )
    );

    MapObjectLayer.getInstance().scheduleDownload(new Bounds(1,1,1,1));
    // Wait for a maximum of 5 sec for a result
    for (int i = 0; MapObjectLayer.getInstance().getObjectCount() <= 0 && i < 50; i++) {
      Thread.sleep(100);
    }
    assertEquals(1, MapObjectLayer.getInstance().getObjectCount());
  }

  @Test
  public void testGetIcon() {
    Icon i = MapObjectLayer.getInstance().getIcon();
    assertEquals(ImageSizes.LAYER.getAdjustedHeight(), i.getIconHeight());
    assertEquals(ImageSizes.LAYER.getAdjustedWidth(), i.getIconWidth());
  }

  @Test
  public void testMergable() {
    assertFalse(MapObjectLayer.getInstance().isMergable(null));
    MapObjectLayer.getInstance().mergeFrom(null);
  }

  @Test
  public void testInfoComponent() {
    assertNull(MapObjectLayer.getInstance().getInfoComponent());
  }

  @Test
  public void testTrivialMethods() {
    assertNotNull(MapObjectLayer.getInstance().getToolTipText());
    MapObjectLayer.getInstance().visitBoundingBox(null);
    assertEquals(0, MapObjectLayer.getInstance().getMenuEntries().length);
  }
}
