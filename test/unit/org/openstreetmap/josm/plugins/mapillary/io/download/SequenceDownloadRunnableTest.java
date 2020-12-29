// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.io.download;

import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static com.github.tomakehurst.wiremock.client.WireMock.anyUrl;
import static com.github.tomakehurst.wiremock.client.WireMock.get;
import static com.github.tomakehurst.wiremock.client.WireMock.noContent;
import static com.github.tomakehurst.wiremock.client.WireMock.notFound;
import static com.github.tomakehurst.wiremock.client.WireMock.urlMatching;
import static com.github.tomakehurst.wiremock.core.WireMockConfiguration.wireMockConfig;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.time.Duration;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.ForkJoinTask;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import com.github.tomakehurst.wiremock.WireMockServer;
import org.awaitility.Awaitility;
import org.awaitility.Durations;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.RegisterExtension;

import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.gui.progress.NullProgressMonitor;
import org.openstreetmap.josm.plugins.mapillary.MapillaryData;
import org.openstreetmap.josm.plugins.mapillary.data.image.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.data.image.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.oauth.MapillaryUser;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryTestRules;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil;
import org.openstreetmap.josm.testutils.JOSMTestRules;

public class SequenceDownloadRunnableTest {
  public static final String DEFAULT_SEARCH_SEQUENCE_PATH = "/api/v3/responses/searchSequences.json";
  @RegisterExtension
  static JOSMTestRules rules = new MapillaryTestRules().preferences();

  /** It takes some time for the first test to fully initialize */
  private static final Duration MINIMUM_INITIALIZATION = Durations.FIVE_SECONDS;

  private static String oldBaseUrl;
  private static WireMockServer wmRule;

  @BeforeAll
  static void beforeAll() {
    wmRule = new WireMockServer(wireMockConfig().dynamicPort());
    wmRule.start();
    oldBaseUrl = TestUtil.getApiV3BaseUrl();
    TestUtil.setAPIv3BaseUrl(wmRule.baseUrl() + "/");
  }

  @BeforeEach
  void beforeEach() {
    wmRule.resetAll();
    if (MapillaryLayer.hasInstance()) {
      MapillaryLayer.getInstance().getData().clear();
    }
    MapillaryUser.setTokenValid(false);
    MapillaryLayer.getInstance().getData().remove(MapillaryLayer.getInstance().getData().getImages());
    assertEquals(0, MapillaryLayer.getInstance().getData().getImages().size());
  }

  @AfterEach
  void afterEach() {
    assertTrue(wmRule.findAllUnmatchedRequests().isEmpty(),
      wmRule.findAllUnmatchedRequests().stream().map(l -> l.getUrl()).collect(Collectors.joining("; ")));
    wmRule.resetAll();
  }

  @AfterAll
  static void afterAll() {
    wmRule.stop();
    TestUtil.setAPIv3BaseUrl(oldBaseUrl);
  }

  @Test
  void testRun1() {
    testNumberOfDecodedImages(wmRule, 4, new Bounds(7.246497, 16.432955, 7.249027, 16.432976));
  }

  @Test
  void testRun2() {
    testNumberOfDecodedImages(wmRule, 0, new Bounds(0, 0, 0, 0));
  }

  @Test
  void testRun3() {
    wmRule.addStubMapping(get(anyUrl()).willReturn(notFound()).build());
    final SequenceDownloadRunnable sdr = new SequenceDownloadRunnable(MapillaryLayer.getInstance().getData(),
      new Bounds(0, 0, 0, 0), NullProgressMonitor.INSTANCE);
    sdr.run();
    assertEquals(0, MapillaryLayer.getInstance().getData().getImages().size());
    wmRule.addStubMapping(get(anyUrl()).willReturn(noContent()).build());
    sdr.run();
    assertEquals(0, MapillaryLayer.getInstance().getData().getImages().size());
  }

  @Test
  void testRun4() {
    MapillaryProperties.CUT_OFF_SEQUENCES_AT_BOUNDS.put(true);
    testNumberOfDecodedImages(wmRule, 4, new Bounds(7.246497, 16.432955, 7.249027, 16.432976));
  }

  @Test
  void testRun5() {
    MapillaryProperties.CUT_OFF_SEQUENCES_AT_BOUNDS.put(true);
    testNumberOfDecodedImages(wmRule, 0, new Bounds(0, 0, 0.000001, 0));
  }

  @Test
  void testJosmTicket20211() {
    MapillaryProperties.CUT_OFF_SEQUENCES_AT_BOUNDS.put(true);
    MapillaryData data = MapillaryLayer.getInstance().getData();
    getSequences(wmRule, new Bounds(39.0649681, -108.5641782, 39.0656596, -108.5629087),
      "/api/v3/responses/searchSequencesTicket20211Area1.geojson");
    getSequences(wmRule, new Bounds(39.0649571, -108.5621009, 39.0656631, -108.5608057),
      "/api/v3/responses/searchSequencesTicket20211Area1.geojson");
    assertEquals(3, data.getImages().size());
    assertEquals(1, data.getSequences().size());
    for (MapillaryAbstractImage image : data.getImages()) {
      assertDoesNotThrow(() -> image.next(), ((MapillaryImage) image).getKey());
      assertDoesNotThrow(() -> image.previous());
    }
  }

  private static void testNumberOfDecodedImages(WireMockServer wiremockServer, int expectedNumImgs, Bounds bounds) {
    getSequences(wiremockServer, bounds, DEFAULT_SEARCH_SEQUENCE_PATH);
    MapillaryData data = MapillaryLayer.getInstance().getData();
    Awaitility.await().atMost(MINIMUM_INITIALIZATION).until(() -> expectedNumImgs == data.getImages().size());
    assertEquals(expectedNumImgs, data.getImages().size());
  }

  /**
   * Get sequences for the data
   *
   * @param wiremockServer The server in use
   * @param bounds The bounds to get
   */
  public static void getSequences(WireMockServer wiremockServer, Bounds bounds, String resourcePath) {
    try {
      wiremockServer.addStubMapping(get(urlMatching("/sequences\\?.+"))
        .willReturn(aResponse().withStatus(200).withBody(
          Files.readAllBytes(Paths.get(SequenceDownloadRunnableTest.class.getResource(resourcePath).toURI()))))
        .build());
      wiremockServer
        .addStubMapping(
          get(urlMatching("/users/[A-Za-z0-9-]+\\?.+"))
            .willReturn(aResponse().withStatus(200)
              .withBody(Files.readAllBytes(Paths
                .get(SequenceDownloadRunnableTest.class.getResource("/api/v3/responses/userProfile.json").toURI()))))
            .build());
    } catch (IOException | URISyntaxException e) {
      fail(e.getMessage());
    }

    final SequenceDownloadRunnable r = new SequenceDownloadRunnable(MapillaryLayer.getInstance().getData(), bounds,
      NullProgressMonitor.INSTANCE);
    r.run();
    // Ensure that the ForkJoinPool finishes.
    ForkJoinPool pool = ForkJoinTask.inForkJoinPool() ? ForkJoinTask.getPool()
      : MapillaryUtils.getForkJoinPool(r.getClass());
    pool.awaitQuiescence(1, TimeUnit.SECONDS);
  }
}
