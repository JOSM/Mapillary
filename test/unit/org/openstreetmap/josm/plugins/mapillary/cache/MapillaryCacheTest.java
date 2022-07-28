// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.cache;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.stream.Collectors;

import junit.framework.AssertionFailedError;
import org.awaitility.Awaitility;
import org.awaitility.Durations;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.RegisterExtension;
import org.openstreetmap.josm.TestUtils;
import org.openstreetmap.josm.data.cache.ICachedLoaderListener;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.data.vector.VectorDataSet;
import org.openstreetmap.josm.data.vector.VectorNode;
import org.openstreetmap.josm.data.vector.VectorWay;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.MapillaryNode;
import org.openstreetmap.josm.plugins.mapillary.gui.workers.MapillaryNodeDownloader;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.AwaitThreadFinish;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.MapillaryCaches;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.MapillaryURLWireMock;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.MapillaryURLWireMockErrors;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillarySequenceUtils;
import org.openstreetmap.josm.testutils.JOSMTestRules;
import org.openstreetmap.josm.testutils.annotations.BasicPreferences;
import org.openstreetmap.josm.tools.Logging;

@AwaitThreadFinish
@BasicPreferences
@MapillaryCaches
@MapillaryURLWireMock
class MapillaryCacheTest {

    @RegisterExtension
    static JOSMTestRules rules = new JOSMTestRules().main().projection();

    @Test
    void test() throws ExecutionException, InterruptedException, TimeoutException {
        // Use 135511895288847 since that is an image we have real information for
        MapillaryNodeDownloader downloader = new MapillaryNodeDownloader(135511895288847L, s -> {
            /* Do nothing */});
        downloader.execute();
        MapillaryNode image = downloader.get(10, TimeUnit.SECONDS);
        MapillaryCache cache = new MapillaryCache(image, MapillaryCache.Type.ORIGINAL);
        assertNotNull(cache.getUrl());
        assertNotNull(cache.getCacheKey());

        assertFalse(cache.isObjectLoadable());

        assertThrows(NullPointerException.class, () -> new MapillaryCache(null, MapillaryCache.Type.ORIGINAL));
    }

    /**
     * Non-regression test for JOSM #21035: IAE due to API request limit reached
     * IAE: No url returned at {@link CacheUtils#submit(INode, MapillaryCache.Type, ICachedLoaderListener)}
     */
    @Test
    @MapillaryURLWireMockErrors(MapillaryURLWireMockErrors.Type.APPLICATION_REQUEST_LIMIT_REACHED)
    void testNonRegression21035() {
        final VectorDataSet vectorDataSet = createSampleData();
        final VectorNode image2 = vectorDataSet.getNodes().stream().filter(node -> 311799370533334L == node.getId())
            .findFirst().orElseThrow(() -> new AssertionFailedError("Image 311799370533334 not found"));
        Logging.clearLastErrorAndWarnings();
        MapillaryCache.cacheSurroundingImages(image2);
        Awaitility.await().pollDelay(Durations.ONE_HUNDRED_MILLISECONDS).catchUncaughtExceptions().ignoreNoExceptions()
            .until(() -> true);
        final List<String> errors = new ArrayList<>(Logging.getLastErrorAndWarnings());
        errors.removeIf(string -> !string.contains("Exception"));
        assertTrue(errors.isEmpty(), errors.stream().collect(Collectors.joining(System.lineSeparator())));
    }

    /**
     * Non-regression test for IAE found when creating test for {@link #testNonRegression21035()}.
     */
    @Test
    @MapillaryURLWireMockErrors(MapillaryURLWireMockErrors.Type.APPLICATION_REQUEST_LIMIT_REACHED)
    void testIAEInSubmit() {
        final VectorDataSet vectorDataSet = createSampleData();
        final VectorNode image2 = vectorDataSet.getNodes().stream().filter(node -> 311799370533334L == node.getId())
            .findFirst().orElseThrow(() -> new AssertionFailedError("Image 311799370533334 not found"));
        Logging.clearLastErrorAndWarnings();
        CacheUtils.submit(image2, MapillaryCache.Type.ORIGINAL, null);
        Awaitility.await().pollDelay(Durations.ONE_HUNDRED_MILLISECONDS).catchUncaughtExceptions().ignoreNoExceptions()
            .until(() -> true);
        final List<String> errors = new ArrayList<>(Logging.getLastErrorAndWarnings());
        errors.removeIf(string -> !string.contains("IllegalArgumentException"));
        assertTrue(errors.isEmpty(), errors.stream().collect(Collectors.joining(System.lineSeparator())));
    }

    private static VectorDataSet createSampleData() {
        final VectorDataSet vectorDataSet = new VectorDataSet();
        final VectorNode image1 = new VectorNode("mapillary-images");
        final VectorNode image2 = new VectorNode(image1.getLayer());
        final VectorNode image3 = new VectorNode(image1.getLayer());
        final String idKey = MapillaryImageUtils.ImageProperties.ID.toString();
        image1.put(idKey, "148137757289079");
        image2.put(idKey, "311799370533334");
        image3.put(idKey, "4235112816526838");
        for (VectorNode i : Arrays.asList(image1, image2, image3)) {
            i.setOsmId(MapillaryImageUtils.getKey(i), 1);
        }
        image1.setCoor(new LatLon(39.065738749246, -108.57077016445));
        image2.setCoor(new LatLon(39.065986975316, -108.57079091664));
        image3.setCoor(new LatLon(39.066878001959, -108.57081199999));
        final VectorWay sequence = new VectorWay("mapillary-sequences");
        sequence.put(MapillarySequenceUtils.KEY, "7nfcwfvjdtphz7yj6zat6a");
        sequence.setNodes(Arrays.asList(image1, image2, image3));
        sequence.getNodes().forEach(vectorDataSet::addPrimitive);
        vectorDataSet.addPrimitive(sequence);
        return vectorDataSet;
    }

    @Test
    void testMapillaryCacheTypes() {
        TestUtils.superficialEnumCodeCoverage(MapillaryCache.Type.class);
    }
}
