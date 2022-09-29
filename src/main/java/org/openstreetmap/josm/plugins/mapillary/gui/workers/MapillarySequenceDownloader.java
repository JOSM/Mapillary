// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.workers;

import static org.openstreetmap.josm.plugins.mapillary.gui.workers.MapillaryNodesDownloader.realDownloadImages;

import java.io.ByteArrayInputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import javax.annotation.Nonnull;
import javax.json.Json;
import javax.json.JsonObject;
import javax.json.JsonReader;

import org.openstreetmap.josm.data.osm.IWay;
import org.openstreetmap.josm.data.vector.VectorDataSet;
import org.openstreetmap.josm.data.vector.VectorNode;
import org.openstreetmap.josm.plugins.mapillary.cache.Caches;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.MapillaryNode;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.MapillarySequence;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.spi.preferences.MapillaryConfig;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillarySequenceUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.api.JsonDecoder;
import org.openstreetmap.josm.plugins.mapillary.utils.api.JsonSequencesDecoder;
import org.openstreetmap.josm.tools.JosmRuntimeException;
import org.openstreetmap.josm.tools.Logging;
import org.openstreetmap.josm.tools.Pair;
import org.openstreetmap.josm.tools.Utils;

/**
 * Download sequences asynchronously
 */
public class MapillarySequenceDownloader extends MapillaryDownloader<MapillarySequence, MapillarySequence> {
    private static final long[] EMPTY_LONG = new long[0];
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
        final long[] images = downloadSequence(this.sequenceKey);
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
                organizedGet.add(new Pair<>(indexDifference, () -> getImageRange(j, imagesToGet, images)));
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
        Map<String, List<MapillaryNode>> map = realDownloadImages(imagesToGet);
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
        for (MapillarySequence seq : chunks) {
            for (MapillaryNode oldNode : seq.getNodes()) {
                VectorNode oldPrimitive = (VectorNode) ds.getPrimitiveById(oldNode);
                if (oldPrimitive != null) {
                    oldPrimitive.putAll(oldNode.getKeys());
                    oldPrimitive.setCoor(oldNode.getCoor());
                }
            }
        }
        // The counter just avoids many resets of the imagery window in short order
        if (!chunks.isEmpty() && counter.getAndAdd(chunks.size()) < 3) {
            this.updater.accept(chunks.get(chunks.size() - 1));
        }
    }

    /**
     * Download a specific set of sequences
     *
     * @param sequence The sequences to download
     * @return The downloaded sequences
     */
    private static long[] downloadSequence(@Nonnull String sequence) {
        // prevent infinite loops. See #20470.
        if (Arrays.stream(Thread.currentThread().getStackTrace())
            .skip(/* getStackTrace, downloadSequences(sequences), downloadSequences(force, sequences) */ 3)
            .filter(element -> MapillaryDownloader.class.getName().equals(element.getClassName()))
            .filter(element -> "downloadSequences".equals(element.getMethodName())).count() > 2) {
            return EMPTY_LONG;
        }
        String strippedSequence = Utils.strip(sequence);
        if (MapillaryLayer.hasInstance() && MapillaryLayer.getInstance().getImage() != null) {
            IWay<?> way = MapillaryLayer.getInstance().getImage().getReferrers().stream().filter(IWay.class::isInstance)
                .map(IWay.class::cast).filter(w -> strippedSequence.equals(MapillarySequenceUtils.getKey(w)))
                .findFirst().orElse(null);
            if (way != null && way.getNodesCount() >= 2) {
                return way.getNodeIds().stream().mapToLong(Long::longValue).toArray();
            }
        }
        final String url = MapillaryConfig.getUrls().getImagesBySequences(sequence);
        final String response = Caches.META_DATA_CACHE.get(url, () -> {
            final JsonObject urlResponse = getUrlResponse(url);
            return urlResponse == null ? null : urlResponse.toString();
        });
        if (response != null) {
            try (JsonReader reader = Json
                .createReader(new ByteArrayInputStream(response.getBytes(StandardCharsets.UTF_8)))) {
                JsonObject obj = reader.readObject();
                return JsonDecoder.decodeData(obj, JsonSequencesDecoder::decodeSequence).stream()
                    .mapToLong(Long::longValue).toArray();
            }
        }
        return EMPTY_LONG;
    }
}
