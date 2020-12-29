// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.UnsupportedEncodingException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.Collection;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.RegisterExtension;

import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.testutils.JOSMTestRules;

class MapillaryURLTest {

  private static final String CLIENT_ID_QUERY_PART = "client_id=UTZhSnNFdGpxSEFFREUwb01GYzlXZzpjNGViMzQxMTIzMjY0MjZm";
  private static final String SORT_BY_KEY = "sort_by=key";

  static class APIv3 {
    @RegisterExtension
    static JOSMTestRules rules = new MapillaryTestRules().preferences();

    @Test
    void testSearchDetections() {
      final String expectedLayerParameter = "layers=trafficsigns";
      assertUrlEquals(MapillaryURL.APIv3.searchDetections(null), "https://a.mapillary.com/v3/image_detections",
        CLIENT_ID_QUERY_PART, expectedLayerParameter, SORT_BY_KEY);
    }

    @Test
    void testSearchImages() {
      assertUrlEquals(MapillaryURL.APIv3.searchImages(null), "https://a.mapillary.com/v3/images", CLIENT_ID_QUERY_PART);
    }

    @Test
    void testSearchSequences() throws UnsupportedEncodingException {
      assertUrlEquals(MapillaryURL.APIv3.searchSequences(new Bounds(new LatLon(1, 2), new LatLon(3, 4), true)),
        "https://a.mapillary.com/v3/sequences", CLIENT_ID_QUERY_PART,
        "bbox=" + URLEncoder.encode("2.0,1.0,4.0,3.0", StandardCharsets.UTF_8.name()));

      assertUrlEquals(MapillaryURL.APIv3.searchSequences(null), "https://a.mapillary.com/v3/sequences",
        CLIENT_ID_QUERY_PART);
    }

    @Test
    void testSearchMapObjects() throws UnsupportedEncodingException {
      final String expectedBaseUrl = "https://a.mapillary.com/v3/map_features";
      final String expectedLayerParameter = "layers=" + URLEncoder.encode("trafficsigns", "UTF-8");
      assertUrlEquals(MapillaryURL.APIv3.searchMapObjects(new Bounds(new LatLon(1, 2), new LatLon(3, 4), true)),
        expectedBaseUrl, CLIENT_ID_QUERY_PART, expectedLayerParameter, SORT_BY_KEY,
        "bbox=" + URLEncoder.encode("2.0,1.0,4.0,3.0", StandardCharsets.UTF_8.name()));

      assertUrlEquals(MapillaryURL.APIv3.searchMapObjects(null), expectedBaseUrl, CLIENT_ID_QUERY_PART,
        expectedLayerParameter, SORT_BY_KEY);
    }

    @Test
    void testSubmitChangeset() throws MalformedURLException {
      assertEquals(new URL("https://a.mapillary.com/v3/changesets?" + CLIENT_ID_QUERY_PART),
        MapillaryURL.APIv3.submitChangeset());
    }

    @Test
    void testParseNextFromHeaderValue() throws MalformedURLException {
      String headerVal = "<https://a.mapillary.com/v3/sequences?page=1&per_page=200&client_id=UTZhSnNFdGpxSEFFREUwb01GYzlXZzpjNGViMzQxMTIzMjY0MjZm>; rel=\"first\", "
        + "<https://a.mapillary.com/v3/sequences?page=2&per_page=200&client_id=UTZhSnNFdGpxSEFFREUwb01GYzlXZzpjNGViMzQxMTIzMjY0MjZm>; rel=\"prev\", "
        + "<https://a.mapillary.com/v3/sequences?page=4&per_page=200&client_id=UTZhSnNFdGpxSEFFREUwb01GYzlXZzpjNGViMzQxMTIzMjY0MjZm>; rel=\"next\"";
      assertEquals(new URL(
        "https://a.mapillary.com/v3/sequences?page=4&per_page=200&client_id=UTZhSnNFdGpxSEFFREUwb01GYzlXZzpjNGViMzQxMTIzMjY0MjZm"),
        MapillaryURL.APIv3.parseNextFromLinkHeaderValue(headerVal));
    }

    @Test
    void testParseNextFromHeaderValue2() throws MalformedURLException {
      String headerVal = "<https://urlFirst>; rel=\"first\", " + "rel = \"next\" ; < ; , "
        + "rel = \"next\" ; <https://urlNext> , " + "<https://urlPrev>; rel=\"prev\"";
      assertEquals(new URL("https://urlNext"), MapillaryURL.APIv3.parseNextFromLinkHeaderValue(headerVal));
    }

    @Test
    void testParseNextFromHeaderValueNull() {
      assertNull(MapillaryURL.APIv3.parseNextFromLinkHeaderValue(null));
    }

    @Test
    void testParseNextFromHeaderValueMalformed() {
      assertNull(MapillaryURL.APIv3.parseNextFromLinkHeaderValue("<###>; rel=\"next\", blub"));
    }
  }

  static class Cloudfront {
    @Test
    void testThumbnail() {
      assertUrlEquals(MapillaryURL.Cloudfront.thumbnail("arbitrary_key", true),
        "https://d1cuyjsrcm0gby.cloudfront.net/arbitrary_key/thumb-2048.jpg");
      assertUrlEquals(MapillaryURL.Cloudfront.thumbnail("arbitrary_key2", false),
        "https://d1cuyjsrcm0gby.cloudfront.net/arbitrary_key2/thumb-320.jpg");
    }
  }

  @Test
  void testBrowseImageURL() throws MalformedURLException {
    assertEquals(new URL("https://www.mapillary.com/map/im/1234567890123456789012"),
      MapillaryURL.MainWebsite.browseImage("1234567890123456789012"));
  }

  @Test
  void testIllegalBrowseImageURL() {
    assertThrows(IllegalArgumentException.class, () -> MapillaryURL.MainWebsite.browseImage(null));
  }

  @Test
  void testConnectURL() {
    assertUrlEquals(MapillaryURL.MainWebsite.connect("http://redirect-host/ä"), "https://www.mapillary.com/connect",
      CLIENT_ID_QUERY_PART, "scope=user%3Aread+public%3Aupload+public%3Awrite+private%3Aread", "response_type=token",
      "redirect_uri=http%3A%2F%2Fredirect-host%2F%C3%A4");

    assertUrlEquals(MapillaryURL.MainWebsite.connect(null), "https://www.mapillary.com/connect", CLIENT_ID_QUERY_PART,
      "scope=user%3Aread+public%3Aupload+public%3Awrite+private%3Aread", "response_type=token");

    assertUrlEquals(MapillaryURL.MainWebsite.connect(""), "https://www.mapillary.com/connect", CLIENT_ID_QUERY_PART,
      "scope=user%3Aread+public%3Aupload+public%3Awrite+private%3Aread", "response_type=token");
  }

  @Test
  void testUploadSecretsURL() throws MalformedURLException {
    assertEquals(new URL("https://a.mapillary.com/v3/me/uploads?" + CLIENT_ID_QUERY_PART),
      MapillaryURL.APIv3.uploadSecretsURL());
  }

  @Test
  void testUserURL() throws MalformedURLException {
    assertEquals(new URL("https://a.mapillary.com/v3/me?" + CLIENT_ID_QUERY_PART), MapillaryURL.APIv3.userURL());
  }

  @Test
  void testString2MalformedURL() throws IllegalAccessException, IllegalArgumentException, InvocationTargetException,
    NoSuchMethodException, SecurityException {
    Method method = MapillaryURL.class.getDeclaredMethod("string2URL", String[].class);
    method.setAccessible(true);
    assertNull(method.invoke(null, new Object[] { new String[] { "malformed URL" } })); // this simply invokes
                                                                                        // string2URL("malformed URL")
    assertNull(method.invoke(null, new Object[] { null })); // invokes string2URL(null)
  }

  @Test
  void testUtilityClass() {
    TestUtil.testUtilityClass(MapillaryURL.class);
    TestUtil.testUtilityClass(MapillaryURL.APIv3.class);
    TestUtil.testUtilityClass(MapillaryURL.Cloudfront.class);
    TestUtil.testUtilityClass(MapillaryURL.MainWebsite.class);
  }

  protected static void assertUrlEquals(Collection<URL> actualUrl, String expectedBaseUrl, String... expectedParams) {
    for (URL url : actualUrl) {
      assertUrlEquals(url, expectedBaseUrl, expectedParams);
    }
  }

  protected static void assertUrlEquals(URL actualUrl, String expectedBaseUrl, String... expectedParams) {
    final String actualUrlString = actualUrl.toString();
    assertEquals(expectedBaseUrl,
      actualUrlString.contains("?") ? actualUrlString.substring(0, actualUrlString.indexOf('?')) : actualUrlString);
    String[] actualParams = actualUrl.getQuery() == null ? new String[0] : actualUrl.getQuery().split("&");
    assertEquals(expectedParams.length, actualParams.length);
    for (String expectedParam : expectedParams) {
      boolean parameterIsPresent = false;
      for (int acIndex = 0; !parameterIsPresent && acIndex < actualParams.length; acIndex++) {
        parameterIsPresent = actualParams[acIndex].equals(expectedParam);
      }
      assertTrue(parameterIsPresent,
        expectedParam + " was expected in the query string of " + actualUrl.toString() + " but wasn't there.");
    }
  }
}
