// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.io.download;

import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static com.github.tomakehurst.wiremock.client.WireMock.anyUrl;
import static com.github.tomakehurst.wiremock.client.WireMock.get;
import static com.github.tomakehurst.wiremock.client.WireMock.noContent;
import static com.github.tomakehurst.wiremock.client.WireMock.notFound;
import static com.github.tomakehurst.wiremock.client.WireMock.stubFor;
import static com.github.tomakehurst.wiremock.client.WireMock.urlMatching;
import static com.github.tomakehurst.wiremock.core.WireMockConfiguration.wireMockConfig;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.function.Function;

import com.github.tomakehurst.wiremock.junit.WireMockRule;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;

import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil.MapillaryTestRules;
import org.openstreetmap.josm.testutils.JOSMTestRules;

public class SequenceDownloadRunnableTest {

  @Rule
  public WireMockRule wmRule = new WireMockRule(wireMockConfig().dynamicPort());

  @Rule
  public JOSMTestRules rules = new MapillaryTestRules();

  private String oldBaseUrl;

  @Before
  public void setUp() {
    MapillaryLayer.getInstance().getData().remove(MapillaryLayer.getInstance().getData().getImages());
    assertEquals(0, MapillaryLayer.getInstance().getData().getImages().size());

    oldBaseUrl = TestUtil.getApiV3BaseUrl();
    TestUtil.setAPIv3BaseUrl("http://localhost:" + wmRule.port() + "/");
  }

  @After
  public void tearDown() {
    MainApplication.getLayerManager().resetState();
    TestUtil.setAPIv3BaseUrl(oldBaseUrl);
  }

  @Test
  public void testRun1() {
    testNumberOfDecodedImages(4, new Bounds(7.246497, 16.432955, 7.249027, 16.432976));
  }

  @Test
  public void testRun2() {
    testNumberOfDecodedImages(0, new Bounds(0, 0, 0, 0));
  }

  @Test
  public void testRun3() {
    stubFor(get(anyUrl()).willReturn(notFound()));
    final SequenceDownloadRunnable sdr = new SequenceDownloadRunnable(MapillaryLayer.getInstance().getData(), new Bounds(0, 0, 0, 0));
    sdr.run();
    assertEquals(0, MapillaryLayer.getInstance().getData().getImages().size());
    stubFor(get(anyUrl()).willReturn(noContent()));
    sdr.run();
    assertEquals(0, MapillaryLayer.getInstance().getData().getImages().size());
  }

  @Test
  public void testRun4() {
    MapillaryProperties.CUT_OFF_SEQUENCES_AT_BOUNDS.put(true);
    testNumberOfDecodedImages(4, new Bounds(7.246497, 16.432955, 7.249027, 16.432976));
  }

  @Test
  public void testRun5() {
    MapillaryProperties.CUT_OFF_SEQUENCES_AT_BOUNDS.put(true);
    testNumberOfDecodedImages(0, new Bounds(0, 0, 0, 0));
  }

  private static void testNumberOfDecodedImages(int expectedNumImgs, Bounds bounds) {
    try {
      stubFor(
        get(urlMatching("/sequences\\?.+"))
          .willReturn(aResponse().withStatus(200).withBody(
            Files.readAllBytes(Paths.get(SequenceDownloadRunnableTest.class.getResource("/api/v3/responses/searchSequences.json").toURI()))
          ))
      );
      stubFor(
        get(urlMatching("/users/[A-Za-z0-9-]+\\?.+"))
          .willReturn(aResponse().withStatus(200).withBody(
            Files.readAllBytes(Paths.get(SequenceDownloadRunnableTest.class.getResource("/api/v3/responses/userProfile.json").toURI()))
          ))
      );
    } catch (IOException | URISyntaxException e) {
      fail(e.getMessage());
    }

    final SequenceDownloadRunnable r = new SequenceDownloadRunnable(MapillaryLayer.getInstance().getData(), bounds);
    r.run();
    assertEquals(expectedNumImgs, MapillaryLayer.getInstance().getData().getImages().size());
  }
}
