// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.RegisterExtension;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.data.vector.VectorDataSet;
import org.openstreetmap.josm.data.vector.VectorNode;
import org.openstreetmap.josm.plugins.mapillary.utils.JsonUtil;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryKeys;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryTestRules;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil;
import org.openstreetmap.josm.testutils.JOSMTestRules;

import javax.json.Json;
import javax.json.JsonObject;
import javax.json.JsonReader;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.time.Instant;
import java.util.Collection;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class JsonImageDetailsDecoderTest {

  @RegisterExtension
  static JOSMTestRules rules = new MapillaryTestRules().preferences();

  @Test
  void testUtilityClass() {
    TestUtil.testUtilityClass(JsonImageDetailsDecoder.class);
  }

  @Test
  void testDecodeImageInfos() throws IOException {
    try (
      InputStream stream = JsonImageDetailsDecoderTest.class.getResourceAsStream("/api/v3/responses/searchImages.json");
      JsonReader reader = Json.createReader(stream)) {
      JsonObject searchImagesResponse = reader.readObject();
      VectorDataSet data = new VectorDataMock();
      VectorNode img1 = createDownloadedImage("_yA5uXuSNugmsK5VucU6Bg", new LatLon(0, 0), 0, false);
      VectorNode img2 = createDownloadedImage("nmF-Wq4EvVTgAUmBicSCCg", new LatLon(0, 0), 0, false);
      VectorNode img3 = createDownloadedImage("arbitrary_key", new LatLon(0, 0), 0, false);
      VectorNode img4 = createImportedImage(null, new LatLon(0, 0), 0, false);
      img4.put(MapillaryImageUtils.CREATED_AT, "1");

      data.addPrimitive(img1);
      data.addPrimitive(img2);
      data.addPrimitive(img3);
      data.addPrimitive(img4);
      JsonImageDetailsDecoder.decodeImageInfos(searchImagesResponse, data);
      assertEquals(Instant.ofEpochMilli(1_491_803_490_334L), MapillaryImageUtils.getCapturedAt(img1)); // 2017-04-10T05:51:30.334Z
      assertEquals(Instant.ofEpochMilli(1_491_803_486_853L), MapillaryImageUtils.getCapturedAt(img2)); // 2017-04-10T05:51:26.853Z
      assertEquals(Instant.ofEpochMilli(0L), MapillaryImageUtils.getCapturedAt(img3));
      assertEquals(Instant.ofEpochMilli(1L), MapillaryImageUtils.getCapturedAt(img4));
    }
  }

  public static VectorNode createDownloadedImage(String key, LatLon latLon, double cameraAngle, boolean pano) {
    VectorNode image = new VectorNode("test");
    image.put(MapillaryImageUtils.KEY, key);
    image.setCoor(latLon);
    image.put(MapillaryImageUtils.CAMERA_ANGLE, Double.toString(cameraAngle));
    image.put(MapillaryImageUtils.PANORAMIC, pano ? MapillaryKeys.PANORAMIC_TRUE : MapillaryKeys.PANORAMIC_FALSE);
    return image;
  }

  public static VectorNode createImportedImage(String file, LatLon latLon, double cameraAngle, boolean pano) {
    VectorNode image = new VectorNode("test");
    image.put(MapillaryImageUtils.IMPORTED_KEY, file);
    image.setCoor(latLon);
    image.put(MapillaryImageUtils.CAMERA_ANGLE, Double.toString(cameraAngle));
    image.put(MapillaryImageUtils.PANORAMIC, pano ? MapillaryKeys.PANORAMIC_TRUE : MapillaryKeys.PANORAMIC_FALSE);
    return image;
  }

  @Test
  void testInvalidImageInfos() {
    VectorDataMock data = new VectorDataMock();
    JsonImageDetailsDecoder.decodeImageInfos(null, data);
    JsonImageDetailsDecoder.decodeImageInfos(JsonUtil.string2jsonObject("{}"), null);
    JsonImageDetailsDecoder.decodeImageInfos(JsonUtil.string2jsonObject("{}"), data);
    JsonImageDetailsDecoder
      .decodeImageInfos(JsonUtil.string2jsonObject("{\"type\":\"FeatureCollection\", \"features\":0}"), data);
    JsonImageDetailsDecoder
      .decodeImageInfos(JsonUtil.string2jsonObject("{\"type\":\"FeatureCollection\", \"features\":[0, null]}"), data);
    assertEquals(0, data.getNumImageRetrievals());
  }

  @Test
  void testInvalidImageInfo() throws NoSuchMethodException, SecurityException, IllegalAccessException,
    IllegalArgumentException, InvocationTargetException {
    Method decodeImageInfo = JsonImageDetailsDecoder.class.getDeclaredMethod("decodeImageInfo", JsonObject.class,
      org.openstreetmap.josm.data.vector.VectorDataSet.class);
    VectorDataMock data = new VectorDataMock();
    decodeImageInfo.setAccessible(true);
    decodeImageInfo.invoke(null, null, data);
    decodeImageInfo.invoke(null, JsonUtil.string2jsonObject("{}"), null);
    decodeImageInfo.invoke(null, JsonUtil.string2jsonObject("{\"properties\":null}"), data);
    decodeImageInfo.invoke(null, JsonUtil.string2jsonObject("{\"properties\":{}}"), data);
    decodeImageInfo.invoke(null, JsonUtil.string2jsonObject("{\"properties\":{\"key\":\"arbitrary_key\"}}"), data);
    assertEquals(0, data.getNumImageRetrievals());
  }

  static class VectorDataMock extends org.openstreetmap.josm.data.vector.VectorDataSet {
    private int imageRetrievals;

    /**
     * @return how often the method {@link #getNodes()} has been accessed for this instance.
     */
    public int getNumImageRetrievals() {
      return imageRetrievals;
    }

    @Override
    public synchronized Collection<VectorNode> getNodes() {
      imageRetrievals++;
      return super.getNodes();
    }
  }
}
