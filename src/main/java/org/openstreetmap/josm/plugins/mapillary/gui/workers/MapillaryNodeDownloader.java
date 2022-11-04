// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.workers;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.ExecutionException;
import java.util.function.Consumer;

import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.MapillaryDownloader;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.MapillaryNode;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.tools.JosmRuntimeException;

/**
 * Download a singular node. This is faster than downloading large sequences.
 */
public class MapillaryNodeDownloader extends MapillaryUIDownloader<MapillaryNode, Void> {
    private static final List<MapillaryNodeDownloader> downloadList = new ArrayList<>();
    private final long node;
    private final Consumer<MapillaryNode> onFinish;

    /**
     * Download a singular node. This is faster than downloading large sequences.
     *
     * @param node The node to download
     * @param onFinish The consumer to call on finish
     */
    public MapillaryNodeDownloader(INode node, Consumer<MapillaryNode> onFinish) {
        this(MapillaryImageUtils.getKey(node, true), onFinish);
    }

    /**
     * Download a singular node. This is faster than downloading large sequences.
     *
     * @param id The node id to download
     * @param onFinish The consumer to call on finish
     */
    public MapillaryNodeDownloader(long id, Consumer<MapillaryNode> onFinish) {
        Objects.requireNonNull(onFinish);
        downloadList.add(this);
        this.node = id;
        this.onFinish = onFinish;
    }

    @Override
    protected MapillaryNode doInBackground() {
        return MapillaryDownloader.downloadImages(node).values().stream().flatMap(Collection::stream)
            .filter(n -> n.getOsmId() == node).findFirst().orElse(null);
    }

    @Override
    protected void done() {
        super.done();
        try {
            this.onFinish.accept(this.get());
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new JosmRuntimeException(e);
        } catch (ExecutionException e) {
            throw new JosmRuntimeException(e);
        }
    }

    /**
     * Kill all download threads
     */
    static void killAll() {
        for (MapillaryNodeDownloader downloader : downloadList) {
            downloader.cancel(true);
        }
    }
}
