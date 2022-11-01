// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.workers;

import java.util.List;
import java.util.Objects;
import java.util.concurrent.ExecutionException;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import org.openstreetmap.josm.plugins.mapillary.data.mapillary.MapillaryDownloader;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.MapillaryNode;
import org.openstreetmap.josm.tools.JosmRuntimeException;

/**
 * Download nodes asynchronously
 */
public class MapillaryNodesDownloader extends MapillaryUIDownloader<List<MapillaryNode>, Void> {
    private final Consumer<List<MapillaryNode>> onFinish;
    private final long[] images;

    public MapillaryNodesDownloader(Consumer<List<MapillaryNode>> onFinish, long... images) {
        Objects.requireNonNull(onFinish);
        Objects.requireNonNull(images);
        this.onFinish = onFinish;
        this.images = images.clone();
    }

    @Override
    protected List<MapillaryNode> doInBackground() {
        return MapillaryDownloader.downloadImages(images).values().stream().flatMap(List::stream)
            .collect(Collectors.toList());
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
