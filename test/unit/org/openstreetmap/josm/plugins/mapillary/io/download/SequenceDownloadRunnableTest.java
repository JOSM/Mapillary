// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.io.download;

import static org.junit.Assert.assertEquals;

import java.lang.reflect.Field;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.function.Function;

import org.junit.Before;
import org.junit.Test;
import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.plugins.mapillary.AbstractTest;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil;

public class SequenceDownloadRunnableTest extends AbstractTest {

  private static final Function<Bounds, URL> SEARCH_SEQUENCES_URL_GEN = b -> {
    return SequenceDownloadRunnableTest.class.getResource("/api/v3/responses/searchSequences.json");
  };
  private Field urlGenField;

  @Before
  public void setUp() {
    MapillaryLayer.getInstance().getData().remove(MapillaryLayer.getInstance().getData().getImages());
    assertEquals(0, MapillaryLayer.getInstance().getData().getImages().size());

    urlGenField = TestUtil.getAccessibleField(SequenceDownloadRunnable.class, "urlGen");
  }

  @Test
  public void testRun1() throws IllegalArgumentException, IllegalAccessException {
    testNumberOfDecodedImages(4, SEARCH_SEQUENCES_URL_GEN, new Bounds(7.246497, 16.432955, 7.249027, 16.432976));
  }

  @Test
  public void testRun2() throws IllegalArgumentException, IllegalAccessException {
    testNumberOfDecodedImages(0, SEARCH_SEQUENCES_URL_GEN, new Bounds(0, 0, 0, 0));
  }

  @Test
  public void testRun3() throws IllegalArgumentException, IllegalAccessException {
    testNumberOfDecodedImages(0, b -> {
      try { return new URL("https://mapillary/nonexistentURL"); } catch (MalformedURLException e) {} return null;
    }, new Bounds(0, 0, 0, 0));
  }

  @Test
  public void testRun4() throws IllegalArgumentException, IllegalAccessException {
    MapillaryProperties.CUT_OFF_SEQUENCES_AT_BOUNDS.put(true);
    testNumberOfDecodedImages(4, SEARCH_SEQUENCES_URL_GEN, new Bounds(7.246497, 16.432955, 7.249027, 16.432976));
  }

  @Test
  public void testRun5() throws IllegalArgumentException, IllegalAccessException {
    MapillaryProperties.CUT_OFF_SEQUENCES_AT_BOUNDS.put(true);
    testNumberOfDecodedImages(0, SEARCH_SEQUENCES_URL_GEN, new Bounds(0, 0, 0, 0));
  }

  private void testNumberOfDecodedImages(int expectedNumImgs, Function<Bounds, URL> urlGen, Bounds bounds)
      throws IllegalArgumentException, IllegalAccessException {
    SequenceDownloadRunnable r = new SequenceDownloadRunnable(MapillaryLayer.getInstance().getData(), bounds);
    urlGenField.set(null, urlGen);
    r.run();
    assertEquals(expectedNumImgs, MapillaryLayer.getInstance().getData().getImages().size());
  }
}
