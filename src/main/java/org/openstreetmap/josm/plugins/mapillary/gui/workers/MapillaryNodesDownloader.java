// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.workers;

import java.io.ByteArrayInputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ExecutionException;
import java.util.function.Consumer;
import java.util.stream.Collector;
import java.util.stream.Collectors;

import javax.json.Json;
import javax.json.JsonNumber;
import javax.json.JsonObject;
import javax.json.JsonReader;
import javax.json.JsonString;
import javax.json.JsonValue;

import org.openstreetmap.josm.plugins.mapillary.cache.Caches;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.MapillaryNode;
import org.openstreetmap.josm.plugins.mapillary.spi.preferences.MapillaryConfig;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.api.JsonDecoder;
import org.openstreetmap.josm.plugins.mapillary.utils.api.JsonImageDetailsDecoder;
import org.openstreetmap.josm.tools.JosmRuntimeException;

/**
 * Download nodes asynchronously
 */
public class MapillaryNodesDownloader extends MapillaryDownloader<List<MapillaryNode>, Void> {
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
        return realDownloadImages(images).values().stream().flatMap(List::stream).collect(Collectors.toList());
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

    static Map<String, List<MapillaryNode>> realDownloadImages(final long... images) {
        if (images.length == 0) {
            return Collections.emptyMap();
        }
        final Caches.MapillaryCacheAccess<String> metaDataCache = Caches.META_DATA_CACHE;
        String url = MapillaryConfig.getUrls().getImageInformation(images);
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
                final String entryUrl = MapillaryConfig.getUrls().getImageInformation(id);
                Caches.META_DATA_CACHE.getICacheAccess().put(entryUrl, entry.toString());
            }
        }
    }
}
