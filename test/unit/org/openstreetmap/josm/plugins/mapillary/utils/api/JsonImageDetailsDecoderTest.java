// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import static org.junit.Assert.assertEquals;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Set;

import javax.json.Json;
import javax.json.JsonObject;

import org.junit.Rule;
import org.junit.Test;

import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryData;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImportedImage;
import org.openstreetmap.josm.plugins.mapillary.utils.JsonUtil;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil.MapillaryTestRules;
import org.openstreetmap.josm.testutils.JOSMTestRules;

public class JsonImageDetailsDecoderTest {

  @Rule
  public JOSMTestRules rules = new MapillaryTestRules().preferences();

  @Test
  public void testUtilityClass() {
    TestUtil.testUtilityClass(JsonImageDetailsDecoder.class);
  }

  @Test
  public void testDecodeImageInfos() {
    JsonObject searchImagesResponse = Json.createReader(
      JsonImageDetailsDecoderTest.class.getResourceAsStream("/api/v3/responses/searchImages.json")
    ).readObject();
    MapillaryData data = new MapillaryDataMock();
    MapillaryImage img1 = new MapillaryImage("_yA5uXuSNugmsK5VucU6Bg", new LatLon(0, 0), 0, false);
    MapillaryImage img2 = new MapillaryImage("nmF-Wq4EvVTgAUmBicSCCg", new LatLon(0, 0), 0, false);
    MapillaryImage img3 = new MapillaryImage("arbitrary_key", new LatLon(0, 0), 0, false);
    MapillaryAbstractImage img4 = new MapillaryImportedImage(new LatLon(0, 0), 0, null);
    img4.setCapturedAt(0);
    data.add(img1);
    data.add(img2);
    data.add(img3);
    data.add(img4);
    JsonImageDetailsDecoder.decodeImageInfos(searchImagesResponse, data);
    assertEquals(1_491_803_490_334L, img1.getCapturedAt()); // 2017-04-10T05:51:30.334Z
    assertEquals(1_491_803_486_853L, img2.getCapturedAt()); // 2017-04-10T05:51:26.853Z
    assertEquals(0L, img3.getCapturedAt());
    assertEquals(0L, img4.getCapturedAt());
  }

  @Test
  public void testInvalidImageInfos() {
    MapillaryDataMock data = new MapillaryDataMock();
    JsonImageDetailsDecoder.decodeImageInfos(null, data);
    JsonImageDetailsDecoder.decodeImageInfos(JsonUtil.string2jsonObject("{}"), null);
    JsonImageDetailsDecoder.decodeImageInfos(JsonUtil.string2jsonObject("{}"), data);
    JsonImageDetailsDecoder.decodeImageInfos(JsonUtil.string2jsonObject("{\"type\":\"FeatureCollection\", \"features\":0}"), data);
    JsonImageDetailsDecoder.decodeImageInfos(JsonUtil.string2jsonObject("{\"type\":\"FeatureCollection\", \"features\":[0, null]}"), data);
    assertEquals(0, data.getNumImageRerievals());
  }

  @Test
  public void testInvalidImageInfo() throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
    Method decodeImageInfo = JsonImageDetailsDecoder.class.getDeclaredMethod("decodeImageInfo", JsonObject.class, MapillaryData.class);
    MapillaryDataMock data = new MapillaryDataMock();
    decodeImageInfo.setAccessible(true);
    decodeImageInfo.invoke(null, null, data);
    decodeImageInfo.invoke(null, JsonUtil.string2jsonObject("{}"), null);
    decodeImageInfo.invoke(null, JsonUtil.string2jsonObject("{\"properties\":null}"), data);
    decodeImageInfo.invoke(null, JsonUtil.string2jsonObject("{\"properties\":{}}"), data);
    decodeImageInfo.invoke(null, JsonUtil.string2jsonObject("{\"properties\":{\"key\":\"arbitrary_key\"}}"), data);
    assertEquals(0, data.getNumImageRerievals());
  }

  private static class MapillaryDataMock extends MapillaryData {
    private int imageRetrievals;

    /**
     * Returns how often the method {@link #getImages()} has been accessed for this instance.
     * @return
     */
    public int getNumImageRerievals() {
      return imageRetrievals;
    }

    @Override
    public Set<MapillaryAbstractImage> getImages() {
      imageRetrievals++;
      return super.getImages();
    }
  }
}
