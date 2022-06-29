// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.MalformedURLException;
import java.net.URL;

import org.junit.jupiter.api.Test;

class MapillaryURLTest {

    private static final String CLIENT_ID_QUERY_PART = "client_id=" + MapillaryURL.APIv4.CLIENT_ID;

    // TODO Test APIv4 when actually available

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
            CLIENT_ID_QUERY_PART, "scope=read", "response_type=code",
            "redirect_uri=http%3A%2F%2Fredirect-host%2F%C3%A4");

        assertUrlEquals(MapillaryURL.MainWebsite.connect(null), "https://www.mapillary.com/connect",
            CLIENT_ID_QUERY_PART, "scope=read", "response_type=code");

        assertUrlEquals(MapillaryURL.MainWebsite.connect(""), "https://www.mapillary.com/connect", CLIENT_ID_QUERY_PART,
            "scope=read", "response_type=code");
    }

    @Test
    void testString2MalformedURL() throws IllegalAccessException, IllegalArgumentException, InvocationTargetException,
        NoSuchMethodException, SecurityException {
        Method method = MapillaryURL.class.getDeclaredMethod("string2URL", String[].class);
        method.setAccessible(true);
        assertNull(method.invoke(null, new Object[] { new String[] { "malformed URL" } })); // this simply invokes
                                                                                            // string2URL("malformed
                                                                                            // URL")
        assertNull(method.invoke(null, new Object[] { null })); // invokes string2URL(null)
    }

    @Test
    void testUtilityClass() {
        TestUtil.testUtilityClass(MapillaryURL.class);
        TestUtil.testUtilityClass(MapillaryURL.APIv4.class);
        TestUtil.testUtilityClass(MapillaryURL.MainWebsite.class);
    }

    protected static void assertUrlEquals(URL actualUrl, String expectedBaseUrl, String... expectedParams) {
        final String actualUrlString = actualUrl.toString();
        assertEquals(expectedBaseUrl,
            actualUrlString.contains("?") ? actualUrlString.substring(0, actualUrlString.indexOf('?'))
                : actualUrlString);
        String[] actualParams = actualUrl.getQuery() == null ? new String[0] : actualUrl.getQuery().split("&");
        assertEquals(expectedParams.length, actualParams.length);
        for (String expectedParam : expectedParams) {
            boolean parameterIsPresent = false;
            for (int acIndex = 0; !parameterIsPresent && acIndex < actualParams.length; acIndex++) {
                parameterIsPresent = actualParams[acIndex].equals(expectedParam);
            }
            assertTrue(parameterIsPresent,
                expectedParam + " was expected in the query string of " + actualUrl + " but wasn't there.");
        }
    }
}
