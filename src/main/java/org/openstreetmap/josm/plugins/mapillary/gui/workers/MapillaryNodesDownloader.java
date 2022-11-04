// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.workers;

import java.util.ArrayList;
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
    private static final List<MapillaryNodesDownloader> downloadList = new ArrayList<>();
    private final Consumer<List<MapillaryNode>> onFinish;
    private final long[] images;

    public MapillaryNodesDownloader(Consumer<List<MapillaryNode>> onFinish, long... images) {
        Objects.requireNonNull(onFinish);
        Objects.requireNonNull(images);
        downloadList.add(this);
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
        try {
            super.done();
            this.onFinish.accept(this.get());
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new JosmRuntimeException(e);
        } catch (ExecutionException e) {
            throw new JosmRuntimeException(e);
        } finally {
            downloadList.remove(this);
        }
    }

    /**
     * Kill all download threads
     */
    public static void killAll() {
        // Kill the sequence downloader first -- it may cause nodes to be downloaded
        MapillarySequenceDownloader.killAll();
        // Kill individual node downloads
        MapillaryNodeDownloader.killAll();
        for (MapillaryNodesDownloader downloader : downloadList) {
            downloader.cancel(true);
        }
    }
}
