// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.workers;

import static org.openstreetmap.josm.plugins.mapillary.gui.workers.MapillaryNodesDownloader.realDownloadImages;

import java.util.Collection;
import java.util.Objects;
import java.util.concurrent.ExecutionException;
import java.util.function.Consumer;

import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.MapillaryNode;
import org.openstreetmap.josm.tools.JosmRuntimeException;

/**
 * Download a singular node. This is faster than downloading large sequences.
 */
public class MapillaryNodeDownloader extends MapillaryDownloader<MapillaryNode, Void> {
    private final long node;
    private final Consumer<MapillaryNode> onFinish;

    /**
     * Download a singular node. This is faster than downloading large sequences.
     *
     * @param node The node to download
     * @param onFinish The consumer to call on finish
     */
    public MapillaryNodeDownloader(INode node, Consumer<MapillaryNode> onFinish) {
        this(node.getOsmId(), onFinish);
    }

    /**
     * Download a singular node. This is faster than downloading large sequences.
     *
     * @param id The node id to download
     * @param onFinish The consumer to call on finish
     */
    public MapillaryNodeDownloader(long id, Consumer<MapillaryNode> onFinish) {
        Objects.requireNonNull(onFinish);
        this.node = id;
        this.onFinish = onFinish;
    }

    @Override
    protected MapillaryNode doInBackground() {
        return realDownloadImages(node).values().stream().flatMap(Collection::stream).filter(n -> n.getOsmId() == node)
            .findFirst().orElse(null);
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
}
