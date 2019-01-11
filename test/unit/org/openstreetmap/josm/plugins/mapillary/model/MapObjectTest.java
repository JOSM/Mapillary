// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.model;

import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static com.github.tomakehurst.wiremock.client.WireMock.get;
import static com.github.tomakehurst.wiremock.client.WireMock.stubFor;
import static com.github.tomakehurst.wiremock.client.WireMock.urlMatching;
import static com.github.tomakehurst.wiremock.core.WireMockConfiguration.wireMockConfig;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;

import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Paths;

import com.github.tomakehurst.wiremock.junit.WireMockRule;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;

import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.plugins.mapillary.cache.Caches;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil.MapillaryTestRules;
import org.openstreetmap.josm.testutils.JOSMTestRules;

public class MapObjectTest {

  @Rule
  public WireMockRule wmRule = new WireMockRule(wireMockConfig().dynamicPort());

  @Rule
  public JOSMTestRules rules = new MapillaryTestRules();

  private static MapObject mo1;
  private static MapObject mo2;
  private static MapObject mo3;

  private static String oldBaseUrl;
  private Object iconUnknownType;

  private static void initMapObjects() {
    mo1 = new MapObject(new LatLon(0, 0), "key1", "", "", 0, 0);
    mo2 = new MapObject(new LatLon(0, 0), "key2", "", "", 0, 0);
    mo3 = new MapObject(new LatLon(0, 0), "key1", "", "", 0, 0);
  }

  @Before
  public void setUp() throws IllegalArgumentException, IllegalAccessException {
    initMapObjects();

    oldBaseUrl = TestUtil.getMainWebsiteBaseUrl();
    TestUtil.setMainWebsiteBaseUrl("http://localhost:" + wmRule.port() + "/");

    iconUnknownType = TestUtil.getPrivateFieldValue(MapObject.class, null, "ICON_UNKNOWN_TYPE");
  }

  @After
  public void cleanUp() throws IllegalArgumentException, IllegalAccessException {
    TestUtil.setMainWebsiteBaseUrl(oldBaseUrl);
  }

  @SuppressWarnings({ "unused", "PMD.AvoidDuplicateLiterals" })
  @Test(expected = IllegalArgumentException.class)
  public void testIllArgEx1() {
    new MapObject(new LatLon(0, 0), null, "", "", 0, 0);
  }

  @SuppressWarnings("unused")
  @Test(expected = IllegalArgumentException.class)
  public void testIllArgEx2() {
    new MapObject(new LatLon(0, 0), "", null, "", 0, 0);
  }

  @SuppressWarnings("unused")
  @Test(expected = IllegalArgumentException.class)
  public void testIllArgEx3() {
    new MapObject(new LatLon(0, 0), "", "", null, 0, 0);
  }

  @SuppressWarnings("unused")
  @Test(expected = IllegalArgumentException.class)
  public void testIllArgEx4() {
    new MapObject(null, "", "", "", 0, 0);
  }

  @Test
  public void testIcon() throws URISyntaxException, IOException {
    stubFor(
      get(urlMatching("/developer/api-documentation/images/traffic_sign/[a-z]+\\.png"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(Files.readAllBytes(
              Paths.get(MapObject.class.getResource("/images/fake-avatar.png").toURI())
            ))
        )
    );

    final String mapIconKey = "iconkey";
    assertNotNull(MapObject.getIcon(mapIconKey));
    assertNotNull(Caches.MapObjectIconCache.getInstance().get(mapIconKey));
    assertNotNull(MapObject.getIcon(mapIconKey)); // To test a cache hit

    assertEquals(iconUnknownType, MapObject.getIcon("not-in-set"));
  }

  @Test
  public void testEquals() throws SecurityException, IllegalArgumentException {
    assertEquals(mo1, mo1);
    assertNotEquals(mo1, null);
    assertNotEquals(mo1, "");
    assertNotEquals(mo1, mo2);
    assertEquals(mo1, mo3);
    assertEquals(mo1.hashCode(), mo3.hashCode());
  }

}
