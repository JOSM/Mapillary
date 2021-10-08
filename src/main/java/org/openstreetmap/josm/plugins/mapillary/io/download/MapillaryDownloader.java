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
import java.util.function.Consumer;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import javax.json.Json;
import javax.json.JsonNumber;
import javax.json.JsonObject;
import javax.json.JsonReader;
import javax.json.JsonString;
import javax.json.JsonValue;

import org.openstreetmap.josm.data.vector.VectorDataSet;
import org.openstreetmap.josm.data.vector.VectorNode;
import org.openstreetmap.josm.data.vector.VectorWay;
import org.openstreetmap.josm.plugins.mapillary.cache.Caches;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.oauth.OAuthUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillarySequenceUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL;
import org.openstreetmap.josm.plugins.mapillary.utils.SynchronizedLocalObject;
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
    public static Map<String, Collection<VectorNode>> downloadImages(long... images) {
        return realDownloadImages(MapillaryLayer.getInstance().getData(), images);
    }

    /**
     * Download a specific set of images
     *
     * @param images The images to download and update
     */
    public static void downloadImages(final VectorNode... images) {
        final Map<VectorDataSet, List<VectorNode>> groups = Stream.of(images)
            .collect(Collectors.groupingBy(VectorNode::getDataSet));
        for (Map.Entry<VectorDataSet, List<VectorNode>> entry : groups.entrySet()) {
            realDownloadImages(entry.getKey(),
                entry.getValue().stream().mapToLong(MapillaryImageUtils::getKey).toArray());
        }
    }

    private static Map<String, Collection<VectorNode>> realDownloadImages(final VectorDataSet dataSet,
        final long... images) {
        if (images.length == 0) {
            return Collections.emptyMap();
        }
        final Caches.MapillaryCacheAccess<String> metaDataCache = Caches.META_DATA_CACHE;
        String url = MapillaryURL.APIv4.getImageInformation(images);
        String stringJson = metaDataCache.get(url, () -> {
            final JsonObject jsonObject = getUrlResponse(url);
            return jsonObject != null ? jsonObject.toString() : null;
        });
        final Collection<VectorNode> nodes;
        if (stringJson != null) {
            try (JsonReader jsonReader = Json
                .createReader(new ByteArrayInputStream(stringJson.getBytes(StandardCharsets.UTF_8)))) {
                final JsonObject jsonObject = jsonReader.readObject();
                nodes = JsonDecoder.decodeData(jsonObject,
                    json -> JsonImageDetailsDecoder.decodeImageInfos(json, dataSet));
                // OK. Cache each image separately as well.
                if (images.length > 1) {
                    separatelyCacheDownloadedImages(jsonObject);
                }
            }
        } else {
            nodes = Collections.emptyList();
        }
        final Map<String, Collection<VectorNode>> sequenceMap = Collections.unmodifiableMap(
            nodes.stream().sorted(Comparator.comparingLong(image -> MapillaryImageUtils.getDate(image).toEpochMilli()))
                .collect(Collector.of(HashMap<String, Collection<VectorNode>>::new,
                    (map, node) -> map
                        .computeIfAbsent(MapillaryImageUtils.getSequenceKey(node), key -> new ArrayList<>()).add(node),
                    (rMap, oMap) -> {
                        rMap.putAll(oMap);
                        return rMap;
                    })));
        sequenceMap.forEach((sequenceKey, vectorNodes) -> {
            Set<VectorWay> ways = vectorNodes.stream().map(VectorNode::getReferrers).filter(VectorWay.class::isInstance)
                .map(VectorWay.class::cast).collect(Collectors.toSet());
            if (ways.size() != 1) {
                ways.forEach(way -> way.setDeleted(true));
                ways.forEach(way -> way.setNodes(Collections.emptyList()));
                ways.stream().filter(way -> way.getDataSet() != null)
                    .forEach(way -> way.getDataSet().removePrimitive(way));
                SynchronizedLocalObject<VectorDataSet> synchronizedDataSet = new SynchronizedLocalObject<>(dataSet);
                vectorNodes.stream().map(MapillaryImageUtils::getSequenceKey).distinct()
                    .map(MapillaryDownloader::downloadSequences).flatMap(Collection::stream).forEach(way -> {
                        // We need to clear the cached bbox
                        final VectorDataSet ds = way.getDataSet();
                        // We specifically want to synchronize on the datasets, as those are what matter
                        if (ds != null && ds.containsWay(way)) {
                            SynchronizedLocalObject<VectorDataSet> synchDs = new SynchronizedLocalObject<>(ds);
                            synchDs.execute((Consumer<VectorDataSet>) vds -> vds.removePrimitive(way));
                        }
                        synchronizedDataSet.execute((Consumer<VectorDataSet>) vds -> vds.addPrimitive(way));
                    });
            }
        });
        return sequenceMap;
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
    public static Collection<VectorWay> downloadSequences(String... sequences) {
        return downloadSequences(true, sequences);
    }

    /**
     * Download a specific set of sequences
     *
     * @param sequences The sequences to download
     * @param force Force the download if the sequence has already been downloaded once.
     * @return The downloaded sequences
     */
    public static Collection<VectorWay> downloadSequences(boolean force, String... sequences) {
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
            VectorDataSet data = MapillaryLayer.getInstance().getData();
            Set<String> previousSequences = data.getWays().stream().map(MapillarySequenceUtils::getKey)
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
                .flatMap(
                    jsonObject -> JsonDecoder.decodeData(jsonObject, JsonSequencesDecoder::decodeSequence).stream())
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
