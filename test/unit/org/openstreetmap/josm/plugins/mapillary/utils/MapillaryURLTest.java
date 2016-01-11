package org.openstreetmap.josm.plugins.mapillary.utils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.MalformedURLException;
import java.net.URL;

import org.junit.Test;
import org.openstreetmap.josm.data.Bounds;

public class MapillaryURLTest {
  @Test
  public void testBrowseEditURL() throws MalformedURLException {
    assertEquals(
        new URL("https://www.mapillary.com/map/e/1234567890123456789012"),
        MapillaryURL.browseEditURL("1234567890123456789012")
    );
    try {
      MapillaryURL.browseEditURL(null);
      fail();
    } catch (IllegalArgumentException e) {}
    try {
      MapillaryURL.browseEditURL("123456789012345678901");
      fail();
    } catch (IllegalArgumentException e) {}
    try {
      MapillaryURL.browseEditURL("123456789012345678901+");
      fail();
    } catch (IllegalArgumentException e) {}
  }

  @Test
  public void testBrowseImageURL() throws MalformedURLException {
    assertEquals(
        new URL("https://www.mapillary.com/map/im/1234567890123456789012"),
        MapillaryURL.browseImageURL("1234567890123456789012")
    );
    try {
      MapillaryURL.browseImageURL(null);
      fail();
    } catch (IllegalArgumentException e) {}
    try {
      MapillaryURL.browseImageURL("123456789012345678901");
      fail();
    } catch (IllegalArgumentException e) {}
    try {
      MapillaryURL.browseImageURL("123456789012345678901+");
      fail();
    } catch (IllegalArgumentException e) {}
  }

  @Test
  public void testBrowseUploadImageURL() throws MalformedURLException {
    assertEquals(new URL("https://www.mapillary.com/map/upload/im/"), MapillaryURL.browseUploadImageURL());
  }

  @Test
  public void testConnectURL() throws MalformedURLException {
    assertUrlEquals(
        MapillaryURL.connectURL("http://redirect-host/ä"),
        "https://www.mapillary.com/connect",
        "client_id=T1Fzd20xZjdtR0s1VDk5OFNIOXpYdzoxNDYyOGRkYzUyYTFiMzgz",
        "scope=user%3Aread+public%3Aupload+public%3Awrite",
        "response_type=token",
        "redirect_uri=http%3A%2F%2Fredirect-host%2F%C3%A4"
    );

    assertUrlEquals(
        MapillaryURL.connectURL(null),
        "https://www.mapillary.com/connect",
        "client_id=T1Fzd20xZjdtR0s1VDk5OFNIOXpYdzoxNDYyOGRkYzUyYTFiMzgz",
        "scope=user%3Aread+public%3Aupload+public%3Awrite",
        "response_type=token"
    );

    assertUrlEquals(
        MapillaryURL.connectURL(""),
        "https://www.mapillary.com/connect",
        "client_id=T1Fzd20xZjdtR0s1VDk5OFNIOXpYdzoxNDYyOGRkYzUyYTFiMzgz",
        "scope=user%3Aread+public%3Aupload+public%3Awrite",
        "response_type=token"
    );
  }

  @Test
  public void testSearchImageURL() throws MalformedURLException {
    assertUrlEquals(
        MapillaryURL.searchImageURL(new Bounds(1.1, 2.22, 3.333, 4.4444), 42),
        "https://a.mapillary.com/v2/search/im/",
        "client_id=T1Fzd20xZjdtR0s1VDk5OFNIOXpYdzoxNDYyOGRkYzUyYTFiMzgz",
        "min_lon=2.220000",
        "max_lon=4.444400",
        "min_lat=1.100000",
        "max_lat=3.333000",
        "limit=20",
        "page=42"
    );
    assertUrlEquals(
        MapillaryURL.searchImageURL(null, -73),
        "https://a.mapillary.com/v2/search/im/",
        "client_id=T1Fzd20xZjdtR0s1VDk5OFNIOXpYdzoxNDYyOGRkYzUyYTFiMzgz",
        "limit=20",
        "page=-73"
    );
  }

  @Test
  public void testSearchSequenceURL() throws MalformedURLException {
    assertUrlEquals(
        MapillaryURL.searchSequenceURL(new Bounds(-55.55555, -66.666666, 77.7777777, 88.88888888, false), 42),
        "https://a.mapillary.com/v2/search/s/",
        "client_id=T1Fzd20xZjdtR0s1VDk5OFNIOXpYdzoxNDYyOGRkYzUyYTFiMzgz",
        "min_lon=-66.666666",
        "max_lon=88.888889",
        "min_lat=-55.555550",
        "max_lat=77.777778",
        "limit=10",
        "page=42"
    );
    assertUrlEquals(
        MapillaryURL.searchSequenceURL(null, -73),
        "https://a.mapillary.com/v2/search/s/",
        "client_id=T1Fzd20xZjdtR0s1VDk5OFNIOXpYdzoxNDYyOGRkYzUyYTFiMzgz",
        "limit=10",
        "page=-73"
    );
  }

  @Test
  public void testSearchTrafficSignURL() throws MalformedURLException {
    assertUrlEquals(
        MapillaryURL.searchTrafficSignURL(new Bounds(1.1, 2.22, 3.333, 4.4444), -42),
        "https://a.mapillary.com/v2/search/im/or/",
        "client_id=T1Fzd20xZjdtR0s1VDk5OFNIOXpYdzoxNDYyOGRkYzUyYTFiMzgz",
        "min_lon=2.220000",
        "max_lon=4.444400",
        "min_lat=1.100000",
        "max_lat=3.333000",
        "limit=20",
        "page=-42"
    );
    assertUrlEquals(
        MapillaryURL.searchTrafficSignURL(null, 73),
        "https://a.mapillary.com/v2/search/im/or/",
        "client_id=T1Fzd20xZjdtR0s1VDk5OFNIOXpYdzoxNDYyOGRkYzUyYTFiMzgz",
        "limit=20",
        "page=73"
    );
  }

  @Test
  public void testUploadSecretsURL() throws MalformedURLException {
    assertEquals(
        new URL("https://a.mapillary.com/v2/me/uploads/secrets/?client_id=T1Fzd20xZjdtR0s1VDk5OFNIOXpYdzoxNDYyOGRkYzUyYTFiMzgz"),
        MapillaryURL.uploadSecretsURL()
    );
  }

  @Test
  public void testUserURL() throws MalformedURLException {
    assertEquals(
        new URL("https://a.mapillary.com/v2/me/?client_id=T1Fzd20xZjdtR0s1VDk5OFNIOXpYdzoxNDYyOGRkYzUyYTFiMzgz"),
        MapillaryURL.userURL()
    );
  }

  @Test
  public void testString2MalformedURL()
      throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
    Method method = MapillaryURL.class.getDeclaredMethod("string2URL", String.class);
    method.setAccessible(true);
    assertNull(method.invoke(null, "bla"));
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
          expectedParams[exIndex] + " was expected in the query string of " + actualUrl.toString() + " but wasn't there." ,
          parameterIsPresent
      );
    }
  }
}
