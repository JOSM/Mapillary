// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.data.mapillary;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collector;
import java.util.stream.Collectors;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.json.Json;
import jakarta.json.JsonNumber;
import jakarta.json.JsonObject;
import jakarta.json.JsonReader;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;
import org.openstreetmap.josm.data.osm.IWay;
import org.openstreetmap.josm.plugins.mapillary.cache.Caches;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.oauth.OAuthUtils;
import org.openstreetmap.josm.plugins.mapillary.spi.preferences.MapillaryConfig;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillarySequenceUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.api.JsonDecoder;
import org.openstreetmap.josm.plugins.mapillary.utils.api.JsonImageDetailsDecoder;
import org.openstreetmap.josm.plugins.mapillary.utils.api.JsonSequencesDecoder;
import org.openstreetmap.josm.tools.Logging;
import org.openstreetmap.josm.tools.Utils;

/**
 * A class for downloading mapillary image/sequence information. NOT ASYNC!
 */
public final class MapillaryDownloader {
    private MapillaryDownloader() {
        /* Hide constructor */}

    private static final long[] EMPTY_LONG = new long[0];

    /**
     * Download images
     *
     * @param images The images to download
     * @return A map of sequence string to the images in the sequence
     */
    public static Map<String, List<MapillaryNode>> downloadImages(final long... images) {
        if (images.length == 0) {
            return Collections.emptyMap();
        }
        final Caches.MapillaryCacheAccess<String> metaDataCache = Caches.META_DATA_CACHE;
        String url = MapillaryConfig.getUrls().getImageInformation(images);
        String stringJson = metaDataCache.get(url, () -> {
            final JsonObject jsonObject = getUrlResponse(url);
            return jsonObject != null ? jsonObject.toString() : null;
        });
        if (stringJson == null) {
            metaDataCache.getICacheAccess().remove(url);
        }
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

    /**
     * Download a specific set of sequences
     *
     * @param sequence The sequences to download
     * @return The downloaded sequences
     */
    public static long[] downloadSequence(@Nonnull String sequence) {
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
                final String entryUrl = MapillaryConfig.getUrls().getImageInformation(id);
                Caches.META_DATA_CACHE.getICacheAccess().put(entryUrl, entry.toString());
            }
        }
    }

    @Nullable
    static JsonObject getUrlResponse(@Nonnull URI url) {
        try {
            return OAuthUtils.getWithHeader(url);
        } catch (IOException e) {
            Logging.trace(e);
            return null;
        }
    }

    @Nullable
    static JsonObject getUrlResponse(@Nonnull String url) {
        try {
            return getUrlResponse(URI.create(url));
        } catch (IllegalArgumentException e) {
            Logging.error(e);
        }
        return null;
    }
}
