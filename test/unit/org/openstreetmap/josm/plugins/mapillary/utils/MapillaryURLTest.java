// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.MalformedURLException;
import java.net.URL;

import org.junit.Test;
import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL.IMAGE_SELECTOR;

public class MapillaryURLTest {
  private static final String CLIENT_ID_QUERY_PART = "client_id=T1Fzd20xZjdtR0s1VDk5OFNIOXpYdzoxNDYyOGRkYzUyYTFiMzgz";
  private static final String LIMIT_20_QUERY_PART = "limit=20";

  @Test
  public void testBrowseEditURL() throws MalformedURLException {
    assertEquals(
        new URL("https://www.mapillary.com/map/e/1234567890123456789012"),
        MapillaryURL.browseEditURL("1234567890123456789012")
    );
  }

  @Test(expected = IllegalArgumentException.class)
  public void testIllegalBrowseEditURL() {
    MapillaryURL.browseEditURL(null);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testIllegalBrowseEditURL2() {
    MapillaryURL.browseEditURL("123456789012345678901");
  }

  @Test(expected = IllegalArgumentException.class)
  public void testIllegalBrowseEditURL3() {
    MapillaryURL.browseEditURL("12345678901234567890123");
  }

  @Test(expected = IllegalArgumentException.class)
  public void testIllegalBrowseEditURL4() {
    MapillaryURL.browseEditURL("123456789012345678901+");
  }

  @Test
  public void testBrowseImageURL() throws MalformedURLException {
    assertEquals(
        new URL("https://www.mapillary.com/map/im/1234567890123456789012"),
        MapillaryURL.browseImageURL("1234567890123456789012")
    );
  }

  @Test(expected = IllegalArgumentException.class)
  public void testIllegalBrowseImageURL() {
    MapillaryURL.browseImageURL(null);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testIllegalBrowseImageURL2() {
    MapillaryURL.browseImageURL("123456789012345678901");
  }

  @Test(expected = IllegalArgumentException.class)
  public void testIllegalBrowseImageURL3() {
    MapillaryURL.browseImageURL("12345678901234567890123");
  }

  @Test(expected = IllegalArgumentException.class)
  public void testIllegalBrowseImageURL4() {
    MapillaryURL.browseImageURL("123456789012345678901+");
  }

  @Test
  public void testBrowseUploadImageURL() throws MalformedURLException {
    assertEquals(new URL("https://www.mapillary.com/map/upload/im"), MapillaryURL.browseUploadImageURL());
  }

  @Test
  public void testConnectURL() {
    assertUrlEquals(
        MapillaryURL.connectURL("http://redirect-host/ä"),
        "https://www.mapillary.com/connect",
        CLIENT_ID_QUERY_PART,
        "scope=user%3Aread+public%3Aupload+public%3Awrite",
        "response_type=token",
        "redirect_uri=http%3A%2F%2Fredirect-host%2F%C3%A4"
    );

    assertUrlEquals(
        MapillaryURL.connectURL(null),
        "https://www.mapillary.com/connect",
        CLIENT_ID_QUERY_PART,
        "scope=user%3Aread+public%3Aupload+public%3Awrite",
        "response_type=token"
    );

    assertUrlEquals(
        MapillaryURL.connectURL(""),
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
