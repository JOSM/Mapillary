// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui;

import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static com.github.tomakehurst.wiremock.client.WireMock.get;
import static com.github.tomakehurst.wiremock.client.WireMock.notFound;
import static com.github.tomakehurst.wiremock.client.WireMock.urlMatching;
import static com.github.tomakehurst.wiremock.core.WireMockConfiguration.wireMockConfig;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collection;
import java.util.concurrent.ForkJoinPool;
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
import org.openstreetmap.josm.gui.util.GuiHelper;
import org.openstreetmap.josm.plugins.mapillary.MapillaryData;
import org.openstreetmap.josm.plugins.mapillary.cache.MapillaryCache;
import org.openstreetmap.josm.plugins.mapillary.data.image.Detections;
import org.openstreetmap.josm.plugins.mapillary.data.image.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.io.download.SequenceDownloadRunnableTest;
import org.openstreetmap.josm.plugins.mapillary.oauth.MapillaryUser;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryTestRules;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil;
import org.openstreetmap.josm.testutils.JOSMTestRules;
import org.openstreetmap.josm.tools.JosmRuntimeException;
import org.openstreetmap.josm.tools.Logging;

/**
 * Test class for {@link MapillaryMainDialog}.
 */
class MapillaryMainDialogTest {
  @RegisterExtension
  static JOSMTestRules josmTestRules = new MapillaryTestRules().assertionsInEDT().main();
  private static String oldBaseUrl;
  private static WireMockServer wmRule;

  @BeforeAll
  static void beforeAll() {
    wmRule = new WireMockServer(wireMockConfig().dynamicPort());
    wmRule.start();
    oldBaseUrl = TestUtil.getApiV3BaseUrl();
    TestUtil.setAPIv3BaseUrl(wmRule.baseUrl() + '/');
  }

  @BeforeEach
  void setUp() {
    if (MapillaryLayer.hasInstance()) {
      MapillaryLayer.getInstance().getData().clear();
    }
    MapillaryUser.setTokenValid(false);
    MapillaryLayer.getInstance().getData().remove(MapillaryLayer.getInstance().getData().getImages());
    assertEquals(0, MapillaryLayer.getInstance().getData().getImages().size());
    Logging.clearLastErrorAndWarnings();
  }

  @AfterEach
  void afterEach() {
    MapillaryUtils.forkJoinPoolsAwaitQuiescence(500, TimeUnit.MILLISECONDS);
    ForkJoinPool.commonPool().awaitQuiescence(500, TimeUnit.MILLISECONDS);
    assertTrue(wmRule.findAllUnmatchedRequests().isEmpty(),
      wmRule.findAllUnmatchedRequests().stream().map(l -> l.getUrl()).collect(Collectors.joining("; ")));
    wmRule.resetAll();
    Collection<String> warnings = Logging.getLastErrorAndWarnings().stream()
      .filter(string -> !string.contains("Unknown detection \"")).collect(Collectors.toList());
    assertTrue(warnings.isEmpty(), String.join("; ", warnings));
  }

  @AfterAll
  static void tearDown() {
    wmRule.stop();
    TestUtil.setAPIv3BaseUrl(oldBaseUrl);
  }

  /**
   * Non-regression test for <a href="https://josm.openstreetmap.de/ticket/20309">Bug #20309</a>
   */
  @Test
  void testNullPointExceptionTicket20309() throws ReflectiveOperationException {
    addMappings();
    MapillaryData data = MapillaryLayer.getInstance().getData();
    SequenceDownloadRunnableTest.getSequences(wmRule, new Bounds(7.246497, 16.432955, 7.249027, 16.432976),
      SequenceDownloadRunnableTest.DEFAULT_SEARCH_SEQUENCE_PATH);
    // The dialog currently uses the default ForkJoinPool
    MapillaryMainDialog dialog = MapillaryMainDialog.getInstance();
    Field imageField = dialog.getClass().getDeclaredField("image");
    Method updateDetectionsMethod = dialog.getClass().getDeclaredMethod("updateDetections", MapillaryCache.class,
      MapillaryAbstractImage.class, Collection.class);

    imageField.setAccessible(true);
    updateDetectionsMethod.setAccessible(true);

    imageField.set(dialog, null);
    MapillaryUtils.forkJoinPoolsAwaitQuiescence(100, TimeUnit.MILLISECONDS);
    MapillaryAbstractImage image = data.getImage("QEVZ1tp-PmrwtqhSwdW9fQ");
    assertNotNull(image);
    // Call twice due to getting image detections on first call
    assertDoesNotThrow(() -> updateDetectionsMethod.invoke(dialog, null, image, new ArrayList<Detections>()));
    MapillaryUtils.forkJoinPoolsAwaitQuiescence(100, TimeUnit.MILLISECONDS);
    assertDoesNotThrow(() -> GuiHelper.runInEDTAndWaitWithException(() -> {
      try {
        updateDetectionsMethod.invoke(dialog,
          new MapillaryCache("LwrHXqFRN_pszCopTKHF_Q", MapillaryCache.Type.THUMBNAIL), image,
          ((Detections) image).getDetections());
      } catch (ReflectiveOperationException e) {
        fail(e);
      }
    }));
    MapillaryUtils.forkJoinPoolsAwaitQuiescence(100, TimeUnit.MILLISECONDS);
  }

  /**
   * Non-regression test for <a href="https://github.com/JOSM/Mapillary/issues/165">GitHub Bug #165</a>
   */
  @Test
  void testNullPointerExceptionGithub165() throws ReflectiveOperationException {
    addMappings();
    MapillaryData data = MapillaryLayer.getInstance().getData();
    SequenceDownloadRunnableTest.getSequences(wmRule, new Bounds(7.246497, 16.432955, 7.249027, 16.432976),
      SequenceDownloadRunnableTest.DEFAULT_SEARCH_SEQUENCE_PATH);
    // The dialog currently uses the default ForkJoinPool
    MapillaryMainDialog dialog = MapillaryMainDialog.getInstance();

    assertDoesNotThrow(() -> GuiHelper.runInEDTAndWaitWithException(() -> dialog.updateImage()));
    MapillaryUtils.forkJoinPoolsAwaitQuiescence(100, TimeUnit.MILLISECONDS);
    MapillaryAbstractImage image1 = data.getImage("QEVZ1tp-PmrwtqhSwdW9fQ");
    dialog.setImage(image1);
    MapillaryAbstractImage image2 = data.getImage("Aufjv2hdCKwg9LySWWVSwg");
    assertDoesNotThrow(() -> GuiHelper.runInEDTAndWaitWithException(() -> dialog.updateImage()));
    MapillaryUtils.forkJoinPoolsAwaitQuiescence(100, TimeUnit.MILLISECONDS);
    GuiHelper.runInEDT(() -> {
      throw new JosmRuntimeException("test");
    });
    Thread.getDefaultUncaughtExceptionHandler();
    Awaitility.await().atLeast(Durations.ONE_SECOND).catchUncaughtExceptions().ignoreNoExceptions();
  }

  private static void addMappings() {
    wmRule.addStubMapping(get(urlMatching("/images/.*")).willReturn(notFound().withStatus(404)).build());
    try {
      wmRule
        .addStubMapping(
          get(urlMatching("/image_detections.*"))
            .willReturn(
              aResponse().withStatus(200)
                .withBody(Files.readAllBytes(Paths.get(MapillaryMainDialogTest.class
                  .getResource("/api/v3/responses/image_detections_QEVZ1tp-PmrwtqhSwdW9fQ.geojson").toURI()))))
            .build());
    } catch (Exception e) {
      fail(e);
    }
  }
}
