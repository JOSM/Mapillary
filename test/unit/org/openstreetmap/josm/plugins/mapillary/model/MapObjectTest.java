// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.model;

import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static com.github.tomakehurst.wiremock.client.WireMock.get;
import static com.github.tomakehurst.wiremock.client.WireMock.urlMatching;
import static com.github.tomakehurst.wiremock.core.WireMockConfiguration.wireMockConfig;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Paths;

import com.github.tomakehurst.wiremock.WireMockServer;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.RegisterExtension;

import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.plugins.mapillary.cache.Caches;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryTestRules;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil;
import org.openstreetmap.josm.testutils.JOSMTestRules;

class MapObjectTest {

  @RegisterExtension
  static JOSMTestRules rules = new MapillaryTestRules();

  private WireMockServer wmRule;

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

  @BeforeEach
  void setUp() throws IllegalArgumentException {
    wmRule = new WireMockServer(wireMockConfig().dynamicPort());
    wmRule.start();
    initMapObjects();

    oldBaseUrl = TestUtil.getMainWebsiteBaseUrl();
    TestUtil.setMainWebsiteBaseUrl("http://localhost:" + wmRule.port() + "/");

    iconUnknownType = TestUtil.getPrivateFieldValue(MapObject.class, null, "ICON_UNKNOWN_TYPE");
  }

  @AfterEach
  void cleanUp() throws IllegalArgumentException {
    wmRule.stop();
    TestUtil.setMainWebsiteBaseUrl(oldBaseUrl);
  }

  @SuppressWarnings("PMD.AvoidDuplicateLiterals")
  @Test
  void testIllArgEx1() {
    LatLon ll = new LatLon(0, 0);
    assertThrows(IllegalArgumentException.class, () -> new MapObject(ll, null, "", "", 0, 0));
  }

  @Test
  void testIllArgEx2() {
    LatLon ll = new LatLon(0, 0);
    assertThrows(IllegalArgumentException.class, () -> new MapObject(ll, "", null, "", 0, 0));
  }

  @Test
  void testIllArgEx3() {
    LatLon ll = new LatLon(0, 0);
    assertThrows(IllegalArgumentException.class, () -> new MapObject(ll, "", "", null, 0, 0));
  }

  @Test
  void testIllArgEx4() {
    assertThrows(IllegalArgumentException.class, () -> new MapObject(null, "", "", "", 0, 0));
  }

  @Test
  void testIcon() throws URISyntaxException, IOException {
    wmRule.addStubMapping(get(urlMatching("/developer/api-documentation/images/traffic_sign/[a-z]+\\.png"))
      .willReturn(aResponse().withStatus(200)
        .withBody(Files.readAllBytes(Paths.get(MapObject.class.getResource("/images/fake-avatar.png").toURI()))))
      .build());

    final String mapIconKey = "iconkey";
    assertNotNull(MapObject.getIcon(mapIconKey));
    assertNotNull(Caches.mapObjectIconCache.get(mapIconKey, () -> null));
    assertNotNull(MapObject.getIcon(mapIconKey)); // To test a cache hit

    assertEquals(iconUnknownType, MapObject.getIcon("not-in-set"));
  }

  @Test
  void testEquals() throws SecurityException, IllegalArgumentException {
    assertEquals(mo1, mo1);
    assertNotNull(mo1);
    assertNotEquals("", mo1);
    assertNotEquals(mo1, mo2);
    assertEquals(mo1, mo3);
    assertEquals(mo1.hashCode(), mo3.hashCode());
  }

}
