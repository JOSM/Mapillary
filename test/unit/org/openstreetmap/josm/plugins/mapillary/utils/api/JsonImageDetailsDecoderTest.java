// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.util.Collection;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.LongStream;

import javax.json.Json;
import javax.json.JsonObject;
import javax.json.JsonReader;

import org.awaitility.Awaitility;
import org.awaitility.Durations;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.RepeatedTest;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.data.vector.VectorDataSet;
import org.openstreetmap.josm.data.vector.VectorNode;
import org.openstreetmap.josm.gui.util.GuiHelper;
import org.openstreetmap.josm.plugins.mapillary.oauth.OAuthUtils;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.MapillaryURLWireMock;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.MapillaryURLWireMockErrors;
import org.openstreetmap.josm.plugins.mapillary.utils.JsonUtil;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryKeys;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil;
import org.openstreetmap.josm.testutils.annotations.BasicPreferences;
import org.openstreetmap.josm.testutils.annotations.HTTP;
import org.openstreetmap.josm.tools.Logging;

@BasicPreferences
@HTTP
@MapillaryURLWireMock
public class JsonImageDetailsDecoderTest {
    @Test
    void testUtilityClass() {
        TestUtil.testUtilityClass(JsonImageDetailsDecoder.class);
    }

    @ParameterizedTest(name = "{index}: using computed locations: {0}")
    @ValueSource(booleans = { true, false })
    @Disabled("JOSM Jenkins server throws IOException (#21121)")
    void testDecodeImageInfos(boolean computedLocations) throws IOException {
        MapillaryProperties.USE_COMPUTED_LOCATIONS.put(computedLocations);
        final long[] images = new long[] { 135511895288847L };

        final VectorDataSet data = new VectorDataMock();
        LongStream.of(images)
            .mapToObj(image -> createDownloadedImage(image, new LatLon(39.068354912098, -108.57081597085), 0, false))
            .forEach(data::addPrimitive);

        for (long image : images) {
            final URL url = new URL(
                MapillaryURL.APIv4.getImageInformation(image, MapillaryImageUtils.ImageProperties.values()));
            JsonDecoder.decodeData(OAuthUtils.getWithHeader(url),
                value -> JsonImageDetailsDecoder.decodeImageInfos(value, data));
        }

        final VectorNode i_135511895288847 = data.getNodes().stream().filter(image -> 135511895288847L == image.getId())
            .findFirst().orElse(null);
        assertNotNull(i_135511895288847);
        // JOSM currently (2021-06-09) only stores timestamps to the second level, not millisecond level
        assertEquals(Instant.ofEpochMilli(1_563_721_072_184L).getEpochSecond(),
            MapillaryImageUtils.getDate(i_135511895288847).getEpochSecond());
        assertEquals(i_135511895288847.getInstant(), MapillaryImageUtils.getDate(i_135511895288847));
        assertEquals(135511895288847L, i_135511895288847.getUniqueId());
        final double delta = 0.000_000_000_1;
        assertEquals(1399.5043095825, Double.parseDouble(i_135511895288847.get("computed_altitude")), delta);
        assertEquals(1364.617, Double.parseDouble(i_135511895288847.get("altitude")), delta);
        assertEquals(0.7561310782241d, MapillaryImageUtils.getQuality(i_135511895288847), delta);
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

    /**
     * This is a non-regression test for JOSM #21265
     */
    @RepeatedTest(5)
    void testComputedGeometryPreferenceNoComputedGeometry() {
        MapillaryProperties.USE_COMPUTED_LOCATIONS.put(true);
        final String jsonString = "{" + "\"data\": [{" + "\"altitude\": 75.864,"
            + "\"thumb_2048_url\": \"https://example.org/thumb_2048_url\"," + "\"captured_at\": 1622825444500,"
            + "\"compass_angle\": 0," + "\"exif_orientation\": 1," + "\"geometry\": {" + "\"type\": \"Point\","
            + "\"coordinates\": [13.7501999, 52.328779699972]" + "}," + "\"id\": \"159513816194268\","
            + "\"quality_score\": 0.3649667405765," + "\"sequence\": \"uzciytp7uccdlxn016q0i9\","
            + "\"thumb_256_url\": \"https://example.org/thumb_256_url\"" + "}]}";
        try (JsonReader jsonReader = Json
            .createReader(new ByteArrayInputStream(jsonString.getBytes(StandardCharsets.UTF_8)))) {
            final VectorDataSet data = new VectorDataMock();
            Logging.clearLastErrorAndWarnings();
            assertDoesNotThrow(() -> JsonDecoder.decodeData(jsonReader.readObject(),
                json -> JsonImageDetailsDecoder.decodeImageInfos(json, data)));
            // This is needed to ensure that the EDT finishes.
            AtomicBoolean edtFinished = new AtomicBoolean();
            GuiHelper.runInEDTAndWait(() -> edtFinished.set(true));
            Awaitility.await().atLeast(Durations.ONE_HUNDRED_MILLISECONDS).atMost(Durations.ONE_SECOND)
                .until(edtFinished::get);
            assertTrue(Logging.getLastErrorAndWarnings().isEmpty());
        }
    }

    @Test
    @MapillaryURLWireMockErrors(MapillaryURLWireMockErrors.Type.APPLICATION_REQUEST_LIMIT_REACHED)
    @Disabled("JOSM Jenkins server throws IOException (#21121)")
    void testDecodeImageInfosWithFetchErrorsApplicationRequestLimitReached() throws IOException {
        final long[] images = new long[] { 148137757289079L, 311799370533334L, 4235112816526838L, 464249047982277L,
            308609047601518L, 135511895288847L, 311681117131457L, };

        final VectorDataSet data = new VectorDataMock();
        LongStream.of(images).mapToObj(image -> createDownloadedImage(image, LatLon.ZERO, 0, false))
            .forEach(data::addPrimitive);

        for (long image : images) {
            final URL url = new URL(
                MapillaryURL.APIv4.getImageInformation(image, MapillaryImageUtils.ImageProperties.values()));
            assertDoesNotThrow(() -> JsonDecoder.decodeData(OAuthUtils.getWithHeader(url),
                value -> JsonImageDetailsDecoder.decodeImageInfos(value, data)));
        }
    }

    public static VectorNode createDownloadedImage(String key, LatLon latLon, double cameraAngle, boolean pano) {
        return createDownloadedImage(Long.parseLong(key), latLon, cameraAngle, pano);
    }

    public static VectorNode createDownloadedImage(long key, LatLon latLon, double cameraAngle, boolean pano) {
        VectorNode image = new VectorNode("test");
        image.setOsmId(key, 1);
        image.put(MapillaryImageUtils.ImageProperties.ID.toString(), Long.toString(key));
        image.setCoor(latLon);
        image.put(MapillaryImageUtils.ImageProperties.COMPASS_ANGLE.toString(), Double.toString(cameraAngle));
        image.put(MapillaryImageUtils.ImageProperties.IS_PANO.toString(),
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
        JsonImageDetailsDecoder.decodeImageInfos(
            JsonUtil.string2jsonObject("{\"type\":\"FeatureCollection\", \"features\":[0, null]}"), data);
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
