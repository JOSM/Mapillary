// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.io.export;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.logging.LogRecord;
import java.util.stream.Collectors;

import javax.imageio.ImageIO;

import com.github.tomakehurst.wiremock.client.WireMock;
import com.github.tomakehurst.wiremock.junit5.WireMockRuntimeInfo;
import com.github.tomakehurst.wiremock.junit5.WireMockTest;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.plugins.mapillary.cache.MapillaryCache;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.MapillaryNode;
import org.openstreetmap.josm.plugins.mapillary.spi.preferences.MapillaryConfig;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.LoggingHandler;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.MapillaryURLWireMock;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.testutils.annotations.BasicPreferences;
import org.openstreetmap.josm.testutils.annotations.HTTP;
import org.openstreetmap.josm.tools.ImageProvider;

/**
 * Test class for {@link MapillaryExportManager}
 */
@BasicPreferences
@WireMockTest
@HTTP
class MapillaryExportManagerTest {
    @TempDir
    Path temporaryDirectory;

    @AfterEach
    void tearDown() {
        MapillaryConfig.setUrlsProvider(new MapillaryURLWireMock.NullMapillaryUrl());
    }

    /**
     * Non-regression test for #22471
     */
    @Test
    @LoggingHandler
    void testNonRegression22471(WireMockRuntimeInfo wireMockRuntimeInfo, LoggingHandler.TestHandler handler)
        throws IOException {
        MapillaryConfig
            .setUrlsProvider(new MapillaryURLWireMock.WireMockServerMapillaryUrl(wireMockRuntimeInfo.getHttpBaseUrl()));
        // This needs to be a bit more than the queue limit. Which is the thread limit.
        int images = 200 * MapillaryCache.THREAD_LIMIT.get();
        List<MapillaryNode> nodes = generateMapillaryNodes(wireMockRuntimeInfo, images);
        MapillaryExportManager<MapillaryNode> manager = new MapillaryExportManager<>(nodes,
            temporaryDirectory.toString());
        manager.getProgressMonitor().beginTask("testNonRegression22471");
        manager.realRun();
        List<LogRecord> records = new ArrayList<>(handler.getRecords());
        records.removeIf(record -> record.getMessage() != null && record.getMessage().contains("HTTP/1.1 200"));
        assertTrue(records.isEmpty(),
            records.stream().map(LogRecord::getMessage).filter(Objects::nonNull).collect(Collectors.joining(", ")));
        List<Path> files = new ArrayList<>();
        Files.walkFileTree(this.temporaryDirectory, new SimpleFileVisitor<Path>() {
            @Override
            public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
                if (file.toFile().isFile()) {
                    files.add(file);
                }
                return super.visitFile(file, attrs);
            }
        });
        assertEquals(images, files.size(), files.stream().map(Path::toString).collect(Collectors.joining(", ")));
    }

    @Test
    @LoggingHandler
    void testNoBlocking(WireMockRuntimeInfo wireMockRuntimeInfo, LoggingHandler.TestHandler handler) {
        MapillaryConfig
            .setUrlsProvider(new MapillaryURLWireMock.WireMockServerMapillaryUrl(wireMockRuntimeInfo.getHttpBaseUrl()));
        List<MapillaryNode> nodes = generateMapillaryNodes(wireMockRuntimeInfo, 10);
        nodes.forEach(node -> node.setKeys(null));
        MapillaryExportManager<MapillaryNode> manager = new MapillaryExportManager<>(nodes,
            temporaryDirectory.toString());
        Future<?> future = MainApplication.worker.submit(manager);
        assertDoesNotThrow(() -> future.get(10, TimeUnit.SECONDS));
        assertTrue(handler.getRecords().isEmpty());
    }

    private static List<MapillaryNode> generateMapillaryNodes(WireMockRuntimeInfo wireMockRuntimeInfo, int count) {
        List<MapillaryNode> nodes = new ArrayList<>(count);
        Image image = ImageProvider.getEmpty(ImageProvider.ImageSizes.SMALLICON).getImage();
        BufferedImage bufferedImage = new BufferedImage(image.getWidth(null), image.getHeight(null),
            BufferedImage.TYPE_BYTE_GRAY);
        Graphics2D g2d = bufferedImage.createGraphics();
        g2d.drawImage(image, 0, 0, null);
        g2d.dispose();
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        assertDoesNotThrow(() -> ImageIO.write(bufferedImage, "jpg", baos));
        byte[] emptyImage = baos.toByteArray();
        for (int i = 1; i <= count; i++) {
            MapillaryNode node = new MapillaryNode();
            node.setOsmId(i, 1);
            node.setCoor(new LatLon(i / 100d, i / 100d));
            node.put(MapillaryImageUtils.ImageProperties.ID.toString(), Integer.toString(i));
            node.put(MapillaryImageUtils.ImageProperties.THUMB_ORIGINAL_URL.toString(),
                wireMockRuntimeInfo.getHttpBaseUrl() + "/image/" + i);
            node.put(MapillaryImageUtils.ImageProperties.SEQUENCE_ID.toString(), "test-sequence");
            wireMockRuntimeInfo.getWireMock()
                .register(WireMock.get("/image/" + i).willReturn(WireMock.aResponse().withBody(emptyImage)));
            nodes.add(node);
        }
        return nodes;
    }
}
