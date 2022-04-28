// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.io.download;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.ForkJoinTask;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.LongStream;
import java.util.stream.Stream;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import javax.json.Json;
import javax.json.JsonNumber;
import javax.json.JsonObject;
import javax.json.JsonReader;
import javax.json.JsonString;
import javax.json.JsonValue;

import org.openstreetmap.josm.data.osm.IWay;
import org.openstreetmap.josm.data.vector.VectorDataSet;
import org.openstreetmap.josm.data.vector.VectorNode;
import org.openstreetmap.josm.plugins.mapillary.cache.Caches;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.MapillaryNode;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.MapillarySequence;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.oauth.OAuthUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillarySequenceUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL;
import org.openstreetmap.josm.plugins.mapillary.utils.api.JsonDecoder;
import org.openstreetmap.josm.plugins.mapillary.utils.api.JsonImageDetailsDecoder;
import org.openstreetmap.josm.plugins.mapillary.utils.api.JsonSequencesDecoder;
import org.openstreetmap.josm.tools.Logging;

/**
 * Class that concentrates all the ways of downloading of the plugin. All the
 * download petitions will be managed one by one.
 *
 * @author nokutu
 */
public final class MapillaryDownloader {
    private MapillaryDownloader() {
        // Private constructor to avoid instantiation
    }

    /**
     * Download a specific set of images
     *
     * @param images The images to download
     * @return The downloaded images
     */
    public static Map<String, Collection<MapillaryNode>> downloadImages(long... images) {
        return downloadImages(null, null, images);
    }

    /**
     * Download a specific set of images
     *
     * @param prioritizedImage The image to prioritize
     * @param updater The method to call when the surrounding nodes have been pulled
     * @param images The images to download
     * @return The downloaded images
     */
    public static Map<String, Collection<MapillaryNode>> downloadImages(@Nullable MapillaryNode prioritizedImage,
        @Nullable Consumer<Collection<MapillarySequence>> updater, long... images) {
        final byte step = 50;
        final Map<String, Collection<MapillaryNode>> returnMap = new HashMap<>();
        // This will be null if the image is null
        final long prioritizedId = MapillaryImageUtils.getKey(prioritizedImage);

        Collection<ForkJoinTask<Map<String, List<MapillaryNode>>>> tasks = new ArrayList<>(images.length / step);
        AtomicReference<ForkJoinTask<Map<String, List<MapillaryNode>>>> prioritizedTask = new AtomicReference<>();
        for (int i = 0; i <= images.length / step; i++) {
            final long[] imagesToGet = Arrays.copyOfRange(images, i * step, Math.min((i + 1) * step, images.length));
            final boolean containsPrioritizedImage = updater != null && prioritizedId > 0
                && LongStream.of(imagesToGet).anyMatch(id -> id == prioritizedId);
            final ForkJoinTask<Map<String, List<MapillaryNode>>> task = ForkJoinTask.adapt(() -> {
                Map<String, List<MapillaryNode>> map = realDownloadImages(imagesToGet);
                if (containsPrioritizedImage && map.size() == 1) {
                    final Map.Entry<String, List<MapillaryNode>> entry = map.entrySet().iterator().next();
                    final MapillarySequence sequence = new MapillarySequence(entry.getKey());
                    sequence.setNodes(new ArrayList<>(entry.getValue()));
                    updater.accept(Collections.singleton(sequence));
                    return Collections.singletonMap(entry.getKey(),
                        entry.getValue().stream().map(MapillaryNode::new).collect(Collectors.toList()));
                }
                return map;
            });
            tasks.add(task);
            if (containsPrioritizedImage) {
                task.fork();
                prioritizedTask.set(task);
            }
        }
        tasks.stream().filter(task -> !Objects.equals(task, prioritizedTask.get())).forEach(ForkJoinTask::fork);
        for (ForkJoinTask<Map<String, List<MapillaryNode>>> task : tasks) {
            task.join().forEach((sequence, nodes) -> returnMap
                .computeIfAbsent(sequence, s -> new ArrayList<>(nodes.size())).addAll(nodes));
        }
        return returnMap;
    }

    /**
     * Download a specific image
     *
     * @param image The image to download
     * @return The downloaded image
     */
    public static MapillaryNode downloadImage(long image) {
        return downloadImages(image).values().stream().flatMap(Collection::stream).findFirst().orElse(null);
    }

    /**
     * Download a specific set of images
     *
     * @param images The images to download and update
     */
    public static Collection<MapillaryNode> downloadImages(final VectorNode... images) {
        final Map<VectorDataSet, List<VectorNode>> groups = Stream.of(images)
            .collect(Collectors.groupingBy(VectorNode::getDataSet));
        final List<MapillaryNode> nodes = new ArrayList<>(images.length);
        for (Map.Entry<VectorDataSet, List<VectorNode>> entry : groups.entrySet()) {
            final long[] ids = entry.getValue().stream().mapToLong(MapillaryImageUtils::getKey).filter(i -> i > 0)
                .toArray();
            downloadImages(ids).values().forEach(nodes::addAll);
        }
        return nodes;
    }

    private static Map<String, List<MapillaryNode>> realDownloadImages(final long... images) {
        if (images.length == 0) {
            return Collections.emptyMap();
        }
        final Caches.MapillaryCacheAccess<String> metaDataCache = Caches.META_DATA_CACHE;
        String url = MapillaryURL.APIv4.getImageInformation(images);
        String stringJson = metaDataCache.get(url, () -> {
            final JsonObject jsonObject = getUrlResponse(url);
            return jsonObject != null ? jsonObject.toString() : null;
        });
        final List<MapillaryNode> nodes;
        if (stringJson != null) {
            try (JsonReader jsonReader = Json
                .createReader(new ByteArrayInputStream(stringJson.getBytes(StandardCharsets.UTF_8)))) {
                final JsonObject jsonObject = jsonReader.readObject();
                nodes = new ArrayList<>(JsonDecoder.decodeData(jsonObject, JsonImageDetailsDecoder::decodeImageInfos));
                final List<Long> imageList = Arrays.stream(images).boxed().collect(Collectors.toList());
                // Needed just in case return order is important (i.e., this is being used for sequence creation)
                nodes.sort(Comparator.comparingInt(img -> imageList.indexOf(MapillaryImageUtils.getKey(img))));
                // OK. Cache each image separately as well.
                if (images.length > 1) {
                    separatelyCacheDownloadedImages(jsonObject);
                }
            }
        } else {
            nodes = Collections.emptyList();
        }
        return Collections.unmodifiableMap(nodes.stream()/*
                                                          * .sorted(Comparator.comparingLong(image ->
                                                          * MapillaryImageUtils.getDate(image).toEpochMilli()))
                                                          */
            .collect(Collector.of(
                HashMap<String, List<MapillaryNode>>::new, (map, node) -> map
                    .computeIfAbsent(MapillaryImageUtils.getSequenceKey(node), key -> new ArrayList<>()).add(node),
                (rMap, oMap) -> {
                    rMap.putAll(oMap);
                    return rMap;
                })));
    }

    private static void separatelyCacheDownloadedImages(final JsonObject jsonObject) {
        final String dataString = "data";
        if (jsonObject.containsKey(dataString)
            && jsonObject.get(dataString).getValueType() == JsonValue.ValueType.ARRAY) {
            for (JsonObject entry : jsonObject.get(dataString).asJsonArray().getValuesAs(JsonObject.class)) {
                final JsonValue idValue = entry.get("id");
                final long id;
                if (idValue.getValueType() == JsonValue.ValueType.STRING) {
                    id = Long.parseLong(((JsonString) idValue).getString());
                } else if (idValue.getValueType() == JsonValue.ValueType.NUMBER) {
                    id = ((JsonNumber) idValue).longValue();
                } else {
                    throw new IllegalArgumentException("id value not understood: " + jsonObject);
                }
                final String entryUrl = MapillaryURL.APIv4.getImageInformation(id);
                Caches.META_DATA_CACHE.getICacheAccess().put(entryUrl, entry.toString());
            }
        }
    }

    /**
     * Download a specific set of sequences
     *
     * @param sequences The sequences to download
     * @return The downloaded sequences
     */
    public static Collection<MapillarySequence> downloadSequences(String... sequences) {
        return downloadSequences(null, null, true, sequences);
    }

    /**
     * Download a specific set of sequences, prioritizing the images around the specified image
     *
     * @param image The image to prioritize
     * @param updater The method to call when the surrounding nodes have been pulled
     * @param sequences The sequences to get
     * @return The downloaded sequences
     */
    public static Collection<MapillarySequence> downloadSequences(MapillaryNode image,
        Consumer<Collection<MapillarySequence>> updater, String... sequences) {
        return downloadSequences(image, updater, true, sequences);
    }

    /**
     * Download a specific set of sequences
     *
     * @param image The image to prioritize
     * @param updater The method to call when the surrounding nodes have been pulled
     * @param sequences The sequences to download
     * @param force Force the download if the sequence has already been downloaded once.
     * @return The downloaded sequences
     */
    private static Collection<MapillarySequence> downloadSequences(@Nullable MapillaryNode image,
        @Nullable Consumer<Collection<MapillarySequence>> updater, boolean force, String... sequences) {
        // prevent infinite loops. See #20470.
        if (Arrays.stream(Thread.currentThread().getStackTrace())
            .skip(/* getStackTrace, downloadSequences(sequences), downloadSequences(force, sequences) */ 3)
            .filter(element -> MapillaryDownloader.class.getName().equals(element.getClassName()))
            .filter(element -> "downloadSequences".equals(element.getMethodName())).count() > 2) {
            return Collections.emptyList();
        }
        String[] toGet = sequences != null
            ? Stream.of(sequences).filter(Objects::nonNull).filter(s -> !s.trim().isEmpty()).toArray(String[]::new)
            : new String[0];
        if (MapillaryLayer.hasInstance() && !force) {
            Set<String> previousSequences = MapillaryLayer.getInstance().getImage().getReferrers().stream()
                .filter(IWay.class::isInstance).map(IWay.class::cast).map(MapillarySequenceUtils::getKey)
                .collect(Collectors.toSet());
            toGet = Stream.of(toGet).filter(seq -> !previousSequences.contains(seq)).toArray(String[]::new);
        }
        if (toGet.length > 0) {
            return Stream.of(toGet).map(MapillaryURL.APIv4::getImagesBySequences)
                .map(url -> Caches.META_DATA_CACHE.get(url, () -> {
                    final JsonObject response = getUrlResponse(url);
                    return response == null ? null : response.toString();
                })).filter(Objects::nonNull).map(string -> {
                    try (JsonReader reader = Json
                        .createReader(new ByteArrayInputStream(string.getBytes(StandardCharsets.UTF_8)))) {
                        return reader.readObject();
                    }
                })
                .flatMap(jsonObject -> JsonDecoder
                    .decodeData(jsonObject, json -> JsonSequencesDecoder.decodeSequence(json, image, updater)).stream())
                .collect(Collectors.toSet());
        }
        return Collections.emptyList();
    }

    @Nullable
    private static JsonObject getUrlResponse(@Nonnull URL url) {
        try {
            return OAuthUtils.getWithHeader(url);
        } catch (IOException e) {
            Logging.trace(e);
            return null;
        }
    }

    @Nullable
    private static JsonObject getUrlResponse(@Nonnull String url) {
        try {
            return getUrlResponse(new URL(url));
        } catch (MalformedURLException e) {
            Logging.error(e);
        }
        return null;
    }
}
