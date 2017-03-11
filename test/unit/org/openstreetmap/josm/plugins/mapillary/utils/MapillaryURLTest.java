// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.UnsupportedEncodingException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;

import org.junit.Test;
import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL.IMAGE_SELECTOR;

public class MapillaryURLTest {
  private static final String CLIENT_ID_QUERY_PART = "client_id=T1Fzd20xZjdtR0s1VDk5OFNIOXpYdzoxNDYyOGRkYzUyYTFiMzgz";
  private static final String LIMIT_20_QUERY_PART = "limit=20";

  public static class APIv3 {
    @Test
    public void testSearchSequences() throws UnsupportedEncodingException {
      assertUrlEquals(
        MapillaryURL.APIv3.searchSequences(new Bounds(new LatLon(1, 2), new LatLon(3, 4), true)),
        "https://a.mapillary.com/v3/sequences",
        CLIENT_ID_QUERY_PART,
        "bbox=" + URLEncoder.encode("2.0,1.0,4.0,3.0", StandardCharsets.UTF_8.name())
      );

      assertUrlEquals(MapillaryURL.APIv3.searchSequences(null), "https://a.mapillary.com/v3/sequences", CLIENT_ID_QUERY_PART);
    }

    @Test
    public void testSubmitChangeset() throws MalformedURLException {
      assertEquals(
        new URL("https://a.mapillary.com/v3/changeset?" + CLIENT_ID_QUERY_PART),
        MapillaryURL.APIv3.submitChangeset()
      );
    }

    @Test
    public void testParseNextFromHeaderValue() throws MalformedURLException {
      String headerVal =
        "<https://a.mapillary.com/v3/sequences?page=1&per_page=200&client_id=TG1sUUxGQlBiYWx2V05NM0pQNUVMQTo2NTU3NTBiNTk1NzM1Y2U2>; rel=\"first\", " +
        "<https://a.mapillary.com/v3/sequences?page=2&per_page=200&client_id=TG1sUUxGQlBiYWx2V05NM0pQNUVMQTo2NTU3NTBiNTk1NzM1Y2U2>; rel=\"prev\", " +
        "<https://a.mapillary.com/v3/sequences?page=4&per_page=200&client_id=TG1sUUxGQlBiYWx2V05NM0pQNUVMQTo2NTU3NTBiNTk1NzM1Y2U2>; rel=\"next\"";
      assertEquals(
        new URL("https://a.mapillary.com/v3/sequences?page=4&per_page=200&client_id=TG1sUUxGQlBiYWx2V05NM0pQNUVMQTo2NTU3NTBiNTk1NzM1Y2U2"),
        MapillaryURL.APIv3.parseNextFromLinkHeaderValue(headerVal)
      );
    }

    @Test
    public void testParseNextFromHeaderValue2() throws MalformedURLException {
      String headerVal =
        "<https://urlFirst>; rel=\"first\", " +
        "rel = \"next\" ; < ; , " +
        "rel = \"next\" ; <https://urlNext> , " +
        "<https://urlPrev>; rel=\"prev\"";
      assertEquals(new URL("https://urlNext"), MapillaryURL.APIv3.parseNextFromLinkHeaderValue(headerVal));
    }

    @Test
    public void testParseNextFromHeaderValueNull() {
      assertEquals(null, MapillaryURL.APIv3.parseNextFromLinkHeaderValue(null));
    }

    @Test
    public void testParseNextFromHeaderValueMalformed() {
      assertEquals(null, MapillaryURL.APIv3.parseNextFromLinkHeaderValue("<###>; rel=\"next\", blub"));
    }
  }

  @Test
  public void testBrowseImageURL() throws MalformedURLException {
    assertEquals(
        new URL("https://www.mapillary.com/map/im/1234567890123456789012"),
        MapillaryURL.MainWebsite.browseImage("1234567890123456789012")
    );
  }

  @Test(expected = IllegalArgumentException.class)
  public void testIllegalBrowseImageURL() {
    MapillaryURL.MainWebsite.browseImage(null);
  }

  @Test
  public void testConnectURL() {
    assertUrlEquals(
        MapillaryURL.MainWebsite.connect("http://redirect-host/ä"),
        "https://www.mapillary.com/connect",
        CLIENT_ID_QUERY_PART,
        "scope=user%3Aread+public%3Aupload+public%3Awrite",
        "response_type=token",
        "redirect_uri=http%3A%2F%2Fredirect-host%2F%C3%A4"
    );

    assertUrlEquals(
        MapillaryURL.MainWebsite.connect(null),
        "https://www.mapillary.com/connect",
        CLIENT_ID_QUERY_PART,
        "scope=user%3Aread+public%3Aupload+public%3Awrite",
        "response_type=token"
    );

    assertUrlEquals(
        MapillaryURL.MainWebsite.connect(""),
        "https://www.mapillary.com/connect",
        CLIENT_ID_QUERY_PART,
        "scope=user%3Aread+public%3Aupload+public%3Awrite",
        "response_type=token"
    );
  }

  @Test
  public void testSearchImageURL() {
    assertUrlEquals(
        MapillaryURL.searchImageInfoURL(new Bounds(1.1, 2.22, 3.333, 4.4444), 42, null),
        "https://a.mapillary.com/v2/search/im",
        CLIENT_ID_QUERY_PART,
        "min_lon=2.220000",
        "max_lon=4.444400",
        "min_lat=1.100000",
        "max_lat=3.333000",
        LIMIT_20_QUERY_PART,
        "page=42"
    );
    assertUrlEquals(
        MapillaryURL.searchImageInfoURL(null, -73, null),
        "https://a.mapillary.com/v2/search/im",
        CLIENT_ID_QUERY_PART,
        LIMIT_20_QUERY_PART,
        "page=-73"
    );
  }

  @Test
  public void testSearchSequenceURL() {
    assertUrlEquals(
        MapillaryURL.searchSequenceURL(new Bounds(-55.55555, -66.666666, 77.7777777, 88.88888888, false), 42),
        "https://a.mapillary.com/v2/search/s",
        CLIENT_ID_QUERY_PART,
        "min_lon=-66.666666",
        "max_lon=88.888889",
        "min_lat=-55.555550",
        "max_lat=77.777778",
        "limit=10",
        "page=42"
    );
    assertUrlEquals(
        MapillaryURL.searchSequenceURL(null, -73),
        "https://a.mapillary.com/v2/search/s",
        CLIENT_ID_QUERY_PART,
        "limit=10",
        "page=-73"
    );
  }

  @Test
  public void testSearchTrafficSignURL() {
    assertUrlEquals(
        MapillaryURL.searchImageInfoURL(new Bounds(1.1, 2.22, 3.333, 4.4444), -42, IMAGE_SELECTOR.OBJ_REC_ONLY),
        "https://a.mapillary.com/v2/search/im/or",
        CLIENT_ID_QUERY_PART,
        "min_lon=2.220000",
        "max_lon=4.444400",
        "min_lat=1.100000",
        "max_lat=3.333000",
        LIMIT_20_QUERY_PART,
        "page=-42"
    );
    assertUrlEquals(
        MapillaryURL.searchImageInfoURL(null, 73, IMAGE_SELECTOR.OBJ_REC_ONLY),
        "https://a.mapillary.com/v2/search/im/or",
        CLIENT_ID_QUERY_PART,
        LIMIT_20_QUERY_PART,
        "page=73"
    );
  }

  @Test
  public void testUploadSecretsURL() throws MalformedURLException {
    assertEquals(
        new URL("https://a.mapillary.com/v2/me/uploads/secrets?"+CLIENT_ID_QUERY_PART),
        MapillaryURL.uploadSecretsURL()
    );
  }

  @Test
  public void testUserURL() throws MalformedURLException {
    assertEquals(
        new URL("https://a.mapillary.com/v2/me?"+CLIENT_ID_QUERY_PART),
        MapillaryURL.userURL()
    );
  }

  @Test
  public void testString2MalformedURL()
      throws IllegalAccessException, IllegalArgumentException, InvocationTargetException, NoSuchMethodException, SecurityException {
    Method method = MapillaryURL.class.getDeclaredMethod("string2URL", String[].class);
    method.setAccessible(true);
    assertNull(method.invoke(null, new Object[]{new String[]{"malformed URL"}})); // this simply invokes string2URL("malformed URL")
  }

  @Test
  public void testUtilityClass() {
    TestUtil.testUtilityClass(MapillaryURL.class);
    TestUtil.testUtilityClass(MapillaryURL.APIv3.class);
    TestUtil.testUtilityClass(MapillaryURL.MainWebsite.class);
  }

  private static void assertUrlEquals(URL actualUrl, String expectedBaseUrl, String... expectedParams) {
    assertEquals(expectedBaseUrl, actualUrl.toString().substring(0, actualUrl.toString().indexOf('?')));
    String[] actualParams = actualUrl.getQuery().split("&");
    assertEquals(expectedParams.length, actualParams.length);
    for (int exIndex = 0; exIndex < expectedParams.length; exIndex++) {
      boolean parameterIsPresent = false;
      for (int acIndex = 0; !parameterIsPresent && acIndex < actualParams.length; acIndex++) {
        parameterIsPresent |= actualParams[acIndex].equals(expectedParams[exIndex]);
      }
      assertTrue(
          expectedParams[exIndex] + " was expected in the query string of " + actualUrl.toString() + " but wasn't there.",
          parameterIsPresent
      );
    }
  }
}
