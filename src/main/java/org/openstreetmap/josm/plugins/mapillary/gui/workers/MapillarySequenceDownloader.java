// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.workers;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.locks.Lock;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import org.openstreetmap.josm.data.vector.VectorDataSet;
import org.openstreetmap.josm.data.vector.VectorNode;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.MapillaryDownloader;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.MapillaryNode;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.MapillarySequence;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.spi.preferences.Config;
import org.openstreetmap.josm.tools.JosmRuntimeException;
import org.openstreetmap.josm.tools.Logging;
import org.openstreetmap.josm.tools.Pair;

/**
 * Download sequences asynchronously
 */
public class MapillarySequenceDownloader extends MapillaryUIDownloader<MapillarySequence, MapillarySequence> {
    private static final byte STEP = 25;
    private static MapillarySequenceDownloader currentSequence;

    private final Consumer<MapillarySequence> updater;
    private long node;
    private final String sequenceKey;
    private MapillaryNode[] currentNodes;

    private final AtomicInteger counter = new AtomicInteger();

    /**
     * Update the current download sequence
     *
     * @param mapillarySequenceDownloader The new sequence downloader
     */
    protected static synchronized void updateCurrentSequenceDownload(
        MapillarySequenceDownloader mapillarySequenceDownloader) {
        if (currentSequence != null) {
            currentSequence.cancel(true);
        }
        currentSequence = mapillarySequenceDownloader;
    }

    /**
     * Kill all sequence downloads
     */
    static synchronized void killAll() {
        if (currentSequence != null) {
            currentSequence.cancel(true);
            currentSequence = null;
        }
    }

    /**
     * Create a new sequence downloader
     *
     * @param image The image whose sequence should be downloaded
     * @param updater The method to call to update the UI
     */
    public MapillarySequenceDownloader(MapillaryNode image, Consumer<MapillarySequence> updater) {
        this(MapillaryImageUtils.getSequenceKey(image), updater);
        this.node = MapillaryImageUtils.getKey(image);
    }

    /**
     * Create a new sequence downloader
     *
     * @param sequence The sequence to download
     * @param updater The method to call to update the UI
     */
    public MapillarySequenceDownloader(String sequence, Consumer<MapillarySequence> updater) {
        Objects.requireNonNull(sequence);
        Objects.requireNonNull(updater);
        this.updater = updater;
        this.sequenceKey = sequence;
    }

    @Override
    protected MapillarySequence doInBackground() {
        updateCurrentSequenceDownload(this);
        final long[] images = MapillaryDownloader.downloadSequence(this.sequenceKey);
        // This will be null if the image is null
        this.currentNodes = new MapillaryNode[images.length];
        int location = -1;
        for (int i = 0; i < images.length; i++) {
            if (images[i] == this.node) {
                location = i;
                break;
            }
        }

        List<Pair<Integer, Runnable>> organizedGet = new ArrayList<>();
        for (int i = 0; i <= images.length / STEP; i++) {
            final long[] imagesToGet = Arrays.copyOfRange(images, i * STEP, Math.min((i + 1) * STEP, images.length));
            if (imagesToGet.length > 0) {
                final int j = i;
                final int indexDifference = Math.min(Math.abs(j * STEP - location),
                    Math.abs((j + 1) * STEP - location));
                if (!this.isCancelled()) {
                    organizedGet.add(new Pair<>(indexDifference, () -> getImageRange(j, imagesToGet, images)));
                }
            }
        }
        organizedGet.sort(Comparator.comparingInt(pair -> pair.a));
        for (Pair<Integer, Runnable> pair : organizedGet) {
            if (!this.isCancelled()) {
                pair.b.run();
            }
        }
        return this.getCurrentSequence();
    }

    private void getImageRange(int i, long[] imagesToGet, long[] images) {
        int retries = 0;
        Map<String, List<MapillaryNode>> map = MapillaryDownloader.downloadImages(imagesToGet);
        while (retries < Config.getPref().getInt("mapillary.image.retries", 10) && (map.size() != 1
            || !map.containsKey(this.sequenceKey) || map.get(this.sequenceKey).size() != imagesToGet.length)) {
            map = MapillaryDownloader.downloadImages(imagesToGet);
            retries++;
        }
        if (map.size() != 1 || !map.containsKey(this.sequenceKey)) {
            throw new IllegalArgumentException(
                "Mapillary sequence " + this.sequenceKey + " did not download any images");
        } else if (map.get(this.sequenceKey).size() != imagesToGet.length) {
            throw new IllegalArgumentException(
                "Mapillary sequence " + this.sequenceKey + " did not download the expected number of images");
        }
        List<MapillaryNode> nodes = map.get(this.sequenceKey);
        for (int j = i * STEP; j < Math.min((i + 1) * STEP, images.length); j++) {
            MapillaryNode mapillaryNode = nodes.get(j - i * STEP);
            if (MapillaryImageUtils.getKey(mapillaryNode) == 0) {
                throw new IllegalArgumentException("There shouldn't be any images with id=0 in a sequence");
            }
            this.currentNodes[j] = mapillaryNode;
        }
        publish(this.getCurrentSequence());
    }

    private synchronized MapillarySequence getCurrentSequence() {
        MapillarySequence seq = new MapillarySequence(this.sequenceKey);
        seq.setNodes(Arrays.stream(this.currentNodes).filter(Objects::nonNull).map(n -> {
            if (n.isReferredByWays(0)) {
                return n;
            }
            MapillaryNode tNode = new MapillaryNode(n);
            tNode.setOsmId(n.getOsmId(), n.getVersion());
            tNode.setKeys(n.getKeys());
            return tNode;
        }).collect(Collectors.toList()));
        return seq;
    }

    @Override
    protected void done() {
        super.done();
        // If this was cancelled, don't update the consumer
        if (this.isCancelled()) {
            Logging.info("Mapillary: {0} sequence download was cancelled", this.sequenceKey);
            return;
        }
        try {
            this.updater.accept(this.get());
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new JosmRuntimeException(e);
        } catch (ExecutionException e) {
            throw new JosmRuntimeException(e);
        }
    }

    @Override
    protected void process(List<MapillarySequence> chunks) {
        super.process(chunks);
        VectorDataSet ds = MapillaryLayer.getInstance().getData();
        // Technically a writeLock would be better, but we cannot get that with the current VectorDataSet
        // implementation.
        final Lock dsLock = ds.getReadLock();
        try {
            dsLock.lock();
            for (MapillarySequence seq : chunks) {
                for (MapillaryNode oldNode : seq.getNodes()) {
                    VectorNode oldPrimitive = (VectorNode) ds.getPrimitiveById(oldNode);
                    if (oldPrimitive != null) {
                        oldPrimitive.putAll(oldNode.getKeys());
                        oldPrimitive.setCoor(oldNode.getCoor());
                    }
                }
            }
        } finally {
            dsLock.unlock();
        }
        // The counter just avoids many resets of the imagery window in short order
        if (!chunks.isEmpty() && counter.getAndAdd(chunks.size()) < 3) {
            this.updater.accept(chunks.get(chunks.size() - 1));
        }
    }
}
