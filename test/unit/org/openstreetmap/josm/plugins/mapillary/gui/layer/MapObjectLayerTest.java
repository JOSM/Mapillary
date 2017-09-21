// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.layer;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.lang.reflect.Field;
import java.net.URL;
import java.util.function.Function;

import javax.swing.Icon;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;

import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapObjectLayer.STATUS;
import org.openstreetmap.josm.plugins.mapillary.io.download.MapObjectDownloadRunnable;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil.MapillaryTestRules;
import org.openstreetmap.josm.testutils.JOSMTestRules;
import org.openstreetmap.josm.tools.ImageProvider.ImageSizes;

public class MapObjectLayerTest {

  @Rule
  public JOSMTestRules rules = new MapillaryTestRules().platform();

  private static Field urlGen;
  private static Object urlGenValue;

  @BeforeClass
  public static void setUp() throws IllegalAccessException {
    urlGen = TestUtil.getAccessibleField(MapObjectDownloadRunnable.class, "URL_GEN");

    urlGenValue = urlGen.get(null);
    urlGen.set(null, (Function<Bounds, URL>) str -> {
      return MapObjectLayer.class.getResource("/api/v3/responses/searchMapObjects.json");
    });
  }

  @AfterClass
  public static void cleanUp() throws IllegalArgumentException, IllegalAccessException {
    urlGen.set(null, urlGenValue);
  }

  @Test
  public void testStatusEnum() {
    assertEquals(4, STATUS.values().length);
    assertEquals(STATUS.COMPLETE, STATUS.valueOf("COMPLETE"));
  }

  @Test
  public void testScheduleDownload() throws InterruptedException {
    MapObjectLayer.getInstance().scheduleDownload(new Bounds(1,1,1,1));
    // Wait for a maximum of 10 sec for a result
    for (int i = 0; MapObjectLayer.getInstance().getObjectCount() <= 0 && i < 100; i++) {
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
