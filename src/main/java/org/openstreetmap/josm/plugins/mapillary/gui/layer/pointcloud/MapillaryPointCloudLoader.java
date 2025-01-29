// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.layer.pointcloud;

import java.util.Collection;
import java.util.HashSet;
import java.util.concurrent.ThreadPoolExecutor;

import org.apache.commons.jcs3.access.behavior.ICacheAccess;
import org.openstreetmap.gui.jmapviewer.Tile;
import org.openstreetmap.gui.jmapviewer.interfaces.TileJob;
import org.openstreetmap.gui.jmapviewer.interfaces.TileLoader;
import org.openstreetmap.gui.jmapviewer.interfaces.TileLoaderListener;
import org.openstreetmap.josm.data.cache.CacheEntry;
import org.openstreetmap.josm.data.imagery.TMSCachedTileLoader;
import org.openstreetmap.josm.data.imagery.TileJobOptions;

public class MapillaryPointCloudLoader implements TileLoader {
    /** The executor for fetching the tiles. */
    private static final ThreadPoolExecutor EXECUTOR = TMSCachedTileLoader
        .getNewThreadPoolExecutor("mapillary:pointcloud");
    /** The current jobs for the loader */
    private final Collection<TileJob> jobs = new HashSet<>();
    /** The cache for downloaded tiles */
    private final ICacheAccess<String, CacheEntry> cache;
    /** The options for the tile loader */
    private final TileJobOptions options;
    /** The listener for when jobs finish */
    private final TileLoaderListener listener;

    /**
     * Create a new tile loader
     *
     * @param listener The listener to call when tiles are finished loading
     * @param cache cache instance that we will work on
     * @param options options of the request
     */
    public MapillaryPointCloudLoader(TileLoaderListener listener, ICacheAccess<String, CacheEntry> cache,
        TileJobOptions options) {
        this.options = options;
        this.cache = cache;
        this.listener = listener;
    }

    @Override
    public TileJob createTileLoaderJob(Tile tile) {
        var job = new MapillaryPointCloudTileJob(EXECUTOR, listener, cache, options, tile);
        jobs.add(job);
        return job;
    }

    @Override
    public void cancelOutstandingTasks() {
        this.jobs.clear();
    }
}
