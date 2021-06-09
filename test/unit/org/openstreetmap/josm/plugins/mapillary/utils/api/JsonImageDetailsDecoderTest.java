// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.RegisterExtension;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.data.vector.VectorDataSet;
import org.openstreetmap.josm.data.vector.VectorNode;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.MapillaryURLWireMock;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.MapillaryURLWireMockErrors;
import org.openstreetmap.josm.plugins.mapillary.utils.JsonUtil;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryKeys;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryTestRules;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil;
import org.openstreetmap.josm.testutils.JOSMTestRules;
import org.openstreetmap.josm.tools.HttpClient;

import javax.json.Json;
import javax.json.JsonObject;
import javax.json.JsonReader;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URL;
import java.time.Instant;
import java.util.Collection;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;

@MapillaryURLWireMock
public class JsonImageDetailsDecoderTest {

  @RegisterExtension
  static JOSMTestRules rules = new MapillaryTestRules().preferences();

  @Test
  void testUtilityClass() {
    TestUtil.testUtilityClass(JsonImageDetailsDecoder.class);
  }

  @ParameterizedTest(name = "{index}: using computed locations: {0}")
  @ValueSource(booleans = { true, false })
  void testDecodeImageInfos(boolean computedLocations) throws IOException {
    MapillaryProperties.USE_COMPUTED_LOCATIONS.put(computedLocations);
    final String[] images = new String[] { "135511895288847" };

    final VectorDataSet data = new VectorDataMock();
    Stream.of(images).map(image -> createDownloadedImage(image, LatLon.ZERO, 0, false)).forEach(data::addPrimitive);

    for (String image : images) {
      final URL url = new URL(
        MapillaryURL.APIv4.getImageInformation(image, MapillaryURL.APIv4.ImageProperties.values()));
      final HttpClient client = HttpClient.create(url);
      final HttpClient.Response response = client.connect();
      JsonReader reader = Json.createReader(response.getContentReader());
      JsonDecoder.decodeData(reader.readObject(), value -> JsonImageDetailsDecoder.decodeImageInfos(value, data));
    }

    final VectorNode i_135511895288847 = data.getNodes().stream()
      .filter(image -> "135511895288847".equals(MapillaryImageUtils.getKey(image))).findFirst().get();
    // JOSM currently (2021-06-09) only stores timestamps to the second level, not millisecond level
    assertEquals(Instant.ofEpochMilli(1_563_721_072_184L).getEpochSecond(),
      MapillaryImageUtils.getDate(i_135511895288847).getEpochSecond());
    assertEquals(i_135511895288847.getInstant(), MapillaryImageUtils.getDate(i_135511895288847));
    assertEquals("135511895288847", MapillaryImageUtils.getKey(i_135511895288847));
    final double delta = 0.000_000_000_1;
    assertEquals(1399.5043095825, Double.parseDouble(i_135511895288847.get("computed_altitude")), delta);
    assertEquals(1364.617, Double.parseDouble(i_135511895288847.get("altitude")), delta);
    assertEquals(0.7561310782241f, MapillaryImageUtils.getQuality(i_135511895288847), delta);
    assertEquals("7nfcwfvjdtphz7yj6zat6a", MapillaryImageUtils.getSequenceKey(i_135511895288847));
    if (Boolean.TRUE.equals(MapillaryProperties.USE_COMPUTED_LOCATIONS.get())) {
      assertEquals(Math.toRadians(1.2865829255371), MapillaryImageUtils.getAngle(i_135511895288847), delta);
      assertEquals(-108.57081597085, i_135511895288847.lon(), delta);
      assertEquals(39.068354912098, i_135511895288847.lat(), delta);
    } else {
      assertEquals(Math.toRadians(336.74), MapillaryImageUtils.getAngle(i_135511895288847), delta);
      assertEquals(-108.57081597222, i_135511895288847.lon(), delta);
      assertEquals(39.068354972222, i_135511895288847.lat(), delta);
    }
  }

  @Test
  @MapillaryURLWireMockErrors(MapillaryURLWireMockErrors.Type.APPLICATION_REQUEST_LIMIT_REACHED)
  void testDecodeImageInfosWithFetchErrorsApplicationRequestLimitReached() throws IOException {
    final String[] images = new String[] { "148137757289079", "311799370533334", "4235112816526838", "464249047982277",
      "308609047601518", "135511895288847", "311681117131457", };

    final VectorDataSet data = new VectorDataMock();
    Stream.of(images).map(image -> createDownloadedImage(image, LatLon.ZERO, 0, false)).forEach(data::addPrimitive);

    for (String image : images) {
      final URL url = new URL(
        MapillaryURL.APIv4.getImageInformation(image, MapillaryURL.APIv4.ImageProperties.values()));
      final HttpClient client = HttpClient.create(url);
      final HttpClient.Response response = client.connect();
      JsonReader reader = Json.createReader(response.getContentReader());
      assertDoesNotThrow(() -> JsonDecoder.decodeData(reader.readObject(),
        value -> JsonImageDetailsDecoder.decodeImageInfos(value, data)));
    }
  }

  public static VectorNode createDownloadedImage(String key, LatLon latLon, double cameraAngle, boolean pano) {
    VectorNode image = new VectorNode("test");
    image.put(MapillaryURL.APIv4.ImageProperties.ID.toString(), key);
    image.setCoor(latLon);
    image.put(MapillaryURL.APIv4.ImageProperties.COMPASS_ANGLE.toString(), Double.toString(cameraAngle));
    image.put(MapillaryURL.APIv4.ImageProperties.IS_PANO.toString(),
      pano ? MapillaryKeys.PANORAMIC_TRUE : MapillaryKeys.PANORAMIC_FALSE);
    return image;
  }

  public static VectorNode createImportedImage(String file, LatLon latLon, double cameraAngle, boolean pano) {
    VectorNode image = new VectorNode("test");
    image.put(MapillaryImageUtils.IMPORTED_KEY, file);
    image.setCoor(latLon);
    image.put(MapillaryURL.APIv4.ImageProperties.COMPASS_ANGLE.toString(), Double.toString(cameraAngle));
    image.put(MapillaryURL.APIv4.ImageProperties.IS_PANO.toString(),
      pano ? MapillaryKeys.PANORAMIC_TRUE : MapillaryKeys.PANORAMIC_FALSE);
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
