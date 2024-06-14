// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.layer.pointcloud;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.net.URI;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.zip.InflaterInputStream;

import org.apache.commons.jcs3.access.behavior.ICacheAccess;
import org.openstreetmap.gui.jmapviewer.Tile;
import org.openstreetmap.gui.jmapviewer.interfaces.TileJob;
import org.openstreetmap.gui.jmapviewer.interfaces.TileLoaderListener;
import org.openstreetmap.josm.data.cache.CacheEntry;
import org.openstreetmap.josm.data.imagery.TileJobOptions;
import org.openstreetmap.josm.plugins.mapillary.io.PointCloudParser;
import org.openstreetmap.josm.tools.HttpClient;
import org.openstreetmap.josm.tools.JosmRuntimeException;

/**
 * A job to load a tile from a point cloud
 */
public class MapillaryPointCloudTileJob implements TileJob {
    private final ThreadPoolExecutor executor;
    private final TileLoaderListener listener;
    private final ICacheAccess<String, CacheEntry> cache;
    private final TileJobOptions options;
    private final Tile tile;

    /**
     * A job to load a tile from a point cloud
     *
     * @param executor that will be executing the jobs
     * @param listener The listener to notify when a tile is loaded
     * @param cache cache instance that we will work on
     * @param options options of the request
     * @param tile The tile for the request
     */
    public MapillaryPointCloudTileJob(ThreadPoolExecutor executor, TileLoaderListener listener,
        ICacheAccess<String, CacheEntry> cache, TileJobOptions options, Tile tile) {
        this.executor = executor;
        this.listener = listener;
        this.cache = cache;
        this.options = options;
        this.tile = tile;
    }

    @Override
    public void submit() {
        submit(false);
    }

    @Override
    public void submit(boolean force) {
        this.executor.submit(this);
    }

    @Override
    public void run() {
        final var id = this.tile.getTileSource().getId();
        final CacheEntry cacheEntry;
        synchronized (id) { // Yes, we want to synchronize on the id, which **should** be the same memory object
            cacheEntry = cache.get(id, () -> getCacheEntry(id));
        }
        if (cacheEntry == null) {
            this.listener.tileLoadingFinished(this.tile, false);
        } else {
            try (var iis = new InflaterInputStream(new ByteArrayInputStream(cacheEntry.getContent()))) {
                final var reconstruction = PointCloudParser.parse(iis);
                throw new UnsupportedOperationException("TODO: paint this somehow");
            } catch (IOException e) {
                // This should never happen since we are using a ByteArrayInputStream
                throw new UncheckedIOException(e);
            }
            // this.listener.tileLoadingFinished(this.tile, true);
        }
    }

    private static CacheEntry getCacheEntry(String url) {
        try {
            final var client = HttpClient.create(URI.create(url).toURL());
            try {
                final var response = client.connect();
                return new CacheEntry(response.getContent().readAllBytes());
            } finally {
                client.disconnect();
            }
        } catch (IOException e) {
            throw new JosmRuntimeException(e);
        }
    }
}
