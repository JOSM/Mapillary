package org.openstreetmap.josm.plugins.mapillary.io.remotecontrol;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.openstreetmap.josm.tools.I18n.tr;

import java.net.UnknownHostException;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.RegisterExtension;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.io.remotecontrol.RemoteControl;
import org.openstreetmap.josm.io.remotecontrol.handler.RequestHandler;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.AwaitThreadFinish;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.MapillaryCaches;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.MapillaryLayerAnnotation;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.MapillaryURLWireMock;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.testutils.JOSMTestRules;
import org.openstreetmap.josm.testutils.annotations.BasicPreferences;

/**
 * Test class for {@link MapillaryRemoteControl}
 *
 * @author Taylor Smock
 */
@AwaitThreadFinish
@BasicPreferences
@MapillaryURLWireMock
@MapillaryLayerAnnotation
class MapillaryRemoteControlTest {
    // Needed for layer clearing. And Main.
    @RegisterExtension
    static JOSMTestRules josmTestRules = new JOSMTestRules().main().projection();

    @Test
    void testGetMandatoryParams() {
        assertEquals(0, new MapillaryRemoteControl().getMandatoryParams().length);
    }

    @Test
    void testGetOptionalParams() {
        assertArrayEquals(new String[] { "photo", "sequence" }, new MapillaryRemoteControl().getOptionalParams());
    }

    @Test
    void testGetPermissionMessage() throws UnknownHostException, RequestHandler.RequestHandlerBadRequestException {
        final MapillaryRemoteControl mapillaryRemoteControl = new MapillaryRemoteControl();
        final String br = "<br />";
        mapillaryRemoteControl.setUrl(RemoteControl.getInet4Address() + "/photo?photo=Mapillary/135511895288847");
        assertDoesNotThrow(mapillaryRemoteControl::getPermissionMessage);
        mapillaryRemoteControl.validateRequest(); // needed to set variables
        assertEquals(tr("Remote Control has been asked to load:") + br + tr("Image: ") + "Mapillary/135511895288847",
            mapillaryRemoteControl.getPermissionMessage());

        mapillaryRemoteControl
            .setUrl(RemoteControl.getInet4Address() + "/photo?sequence=Mapillary/7nfcwfvjdtphz7yj6zat6a");
        mapillaryRemoteControl.validateRequest();
        assertEquals(
            tr("Remote Control has been asked to load:") + br + tr("Sequence: ") + "Mapillary/7nfcwfvjdtphz7yj6zat6a",
            mapillaryRemoteControl.getPermissionMessage());
    }

    @Test
    void testGetPermissionPref() {
        final MapillaryRemoteControl mapillaryRemoteControl = new MapillaryRemoteControl();
        assertNotNull(mapillaryRemoteControl.getPermissionPref());
        assertSame(mapillaryRemoteControl.getPermissionPref(), mapillaryRemoteControl.getPermissionPref());
    }

    @Test
    void testGetUsage() {
        assertEquals("downloads street level images (currently supports Mapillary)",
            new MapillaryRemoteControl().getUsage());
    }

    @Test
    void testGetUsageExamples() {
        assertArrayEquals(new String[] { "/photo", "/photo?photo=Mapillary/135511895288847",
            "photo?sequence=Mapillary/7nfcwfvjdtphz7yj6zat6a" }, new MapillaryRemoteControl().getUsageExamples());
    }

    static Stream<Arguments> testHandleRequest() {
        return Stream.of(Arguments.of("photo=Mapillary/135511895288847", true),
            Arguments.of("sequence=Mapillary/7nfcwfvjdtphz7yj6zat6a", true),
            Arguments.of("photo=Mapillary/135511895288847", false),
            Arguments.of("sequence=Mapillary/7nfcwfvjdtphz7yj6zat6a", false));
    }

    @MapillaryCaches
    @ParameterizedTest
    @MethodSource
    void testHandleRequest(final String request, final boolean createLayerFirst)
        throws UnknownHostException, RequestHandler.RequestHandlerBadRequestException {
        if (createLayerFirst) {
            assertTrue(MainApplication.getLayerManager().containsLayer(MapillaryLayer.getInstance()),
                "getInstance should add the layer");
            assertEquals(1, MainApplication.getLayerManager().getLayersOfType(MapillaryLayer.class).size());
            assertTrue(MapillaryLayer.getInstance().getData().allPrimitives().isEmpty());
        } else {
            assertTrue(MainApplication.getLayerManager().getLayersOfType(MapillaryLayer.class).isEmpty());
        }

        final MapillaryRemoteControl mapillaryRemoteControl = new MapillaryRemoteControl();
        mapillaryRemoteControl.setUrl(RemoteControl.getInet4Address() + "/photo?" + request);
        mapillaryRemoteControl.validateRequest();
        mapillaryRemoteControl.handleRequest();

        final String id = request.replaceAll(".*=Mapillary/", "");
        if (request.startsWith("photo")) {
            assertEquals(Long.valueOf(id), MapillaryLayer.getInstance().getImage().getId());
        } else {
            assertEquals(id, MapillaryImageUtils.getSequenceKey(MapillaryLayer.getInstance().getImage()));
        }
        assertEquals(1, MainApplication.getLayerManager().getLayersOfType(MapillaryLayer.class).size());
    }

    @Test
    void testValidateRequest() throws UnknownHostException, RequestHandler.RequestHandlerBadRequestException {
        final MapillaryRemoteControl mapillaryRemoteControl = new MapillaryRemoteControl();
        mapillaryRemoteControl.setUrl(RemoteControl.getInet4Address() + "/photo?photo=Mapillary/135511895288847");
        assertDoesNotThrow(mapillaryRemoteControl::validateRequest);
        mapillaryRemoteControl
            .setUrl(RemoteControl.getInet4Address() + "/photo?sequence=Mapillary/7nfcwfvjdtphz7yj6zat6a");
        assertDoesNotThrow(mapillaryRemoteControl::validateRequest);

        mapillaryRemoteControl.setUrl(RemoteControl.getInet4Address() + "/photo");
        RequestHandler.RequestHandlerBadRequestException exception = assertThrows(
            RequestHandler.RequestHandlerBadRequestException.class, mapillaryRemoteControl::validateRequest);
        assertEquals(tr("At least one of the optional arguments must be used ({0})",
            String.join(", ", mapillaryRemoteControl.getOptionalParams())), exception.getMessage());

        mapillaryRemoteControl.setUrl(RemoteControl.getInet4Address() + "/photo?sequence=MPL/123");
        exception = assertThrows(RequestHandler.RequestHandlerBadRequestException.class,
            mapillaryRemoteControl::validateRequest);
        assertEquals(tr("Only Mapillary sequences are supported at this time, use sequence=Mapillary/<sequence_id>"),
            exception.getMessage());

        mapillaryRemoteControl.setUrl(RemoteControl.getInet4Address() + "/photo?photo=MPL/123");
        exception = assertThrows(RequestHandler.RequestHandlerBadRequestException.class,
            mapillaryRemoteControl::validateRequest);
        assertEquals(tr("Only Mapillary images are supported at this time, use photo=Mapillary/<image_id>"),
            exception.getMessage());
    }
}
