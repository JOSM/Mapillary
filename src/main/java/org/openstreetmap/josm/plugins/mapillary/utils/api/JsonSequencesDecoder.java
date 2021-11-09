// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.LongStream;

import javax.json.JsonArray;
import javax.json.JsonNumber;
import javax.json.JsonObject;
import javax.json.JsonString;
import javax.json.JsonValue;

import org.openstreetmap.josm.data.osm.IPrimitive;
import org.openstreetmap.josm.data.osm.IWay;
import org.openstreetmap.josm.data.osm.OsmPrimitiveType;
import org.openstreetmap.josm.data.vector.VectorDataSet;
import org.openstreetmap.josm.data.vector.VectorNode;
import org.openstreetmap.josm.data.vector.VectorWay;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.io.download.MapillaryDownloader;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL;
import org.openstreetmap.josm.plugins.mapillary.utils.VectorDataSetUtils;
import org.openstreetmap.josm.tools.Logging;

/**
 * Decodes the JSON returned by {@link MapillaryURL.APIv4} into Java objects.
 * Takes a {@link JsonObject} and {@link #decodeSequence(JsonValue)} tries to convert it to a
 * {@link IWay}.
 */
public final class JsonSequencesDecoder {
    private JsonSequencesDecoder() {
        // Private constructor to avoid instantiation
    }

    /**
     * Parses a given {@link JsonObject} as an array of image identifiers into a {@link IWay}.
     *
     * @param json the {@link JsonObject} to be parsed
     * @return a singleton list of {@link IWay} that is parsed from the given {@link JsonObject}. If mandatory
     *         information
     *         is
     *         missing from the JSON or it's not meeting the expecting format in another way, an empty list will be
     *         returned.
     */
    public static List<VectorWay> decodeSequence(final JsonValue json) {
        /*
         * The response looks like:
         * {"data":[{"id":"0"},{"id":"1"},{"id":"2"},...]}
         * We just have the "data" value
         */
        if (!(json instanceof JsonArray)) {
            Logging.error("Mapillary: The sequence endpoint just returns an array of picture ids");
            return Collections.emptyList();
        }
        final long[] imageIds = ((JsonArray) json)
            .getValuesAs(value -> value instanceof JsonObject ? (JsonObject) value : null).stream()
            .filter(Objects::nonNull).filter(image -> image.containsKey("id")).map(image -> image.get("id"))
            .filter(jsonObject -> jsonObject instanceof JsonString || jsonObject instanceof JsonNumber)
            .mapToLong(value -> value instanceof JsonString ? Long.parseLong(((JsonString) value).getString())
                : ((JsonNumber) value).longValue())
            .distinct().toArray();
        final VectorDataSet data = MapillaryLayer.getInstance().getData();
        final long[] currentImageIds = VectorDataSetUtils
            .tryRead(data,
                () -> LongStream.of(imageIds).mapToObj(id -> data.getPrimitiveById(id, OsmPrimitiveType.NODE))
                    .filter(Objects::nonNull).mapToLong(IPrimitive::getUniqueId)
                    .filter(i -> LongStream.of(imageIds).anyMatch(l -> i == l)).sorted().toArray())
            .orElseGet(() -> new long[0]);
        MapillaryDownloader.downloadImages(
            LongStream.of(imageIds).filter(id -> LongStream.of(currentImageIds).noneMatch(i -> i == id)).toArray());
        final List<VectorNode> nodes = VectorDataSetUtils
            .tryRead(data,
                () -> LongStream.of(imageIds).mapToObj(id -> data.getPrimitiveById(id, OsmPrimitiveType.NODE))
                    .filter(VectorNode.class::isInstance).map(VectorNode.class::cast).collect(Collectors.toList()))
            .orElseGet(Collections::emptyList);
        if (nodes.isEmpty()) {
            Logging.error("Mapillary: The sequence does not have any nodes");
            return Collections.emptyList();
        }
        final VectorWay sequence = nodes.stream().map(MapillaryImageUtils::getSequence)
            .filter(VectorWay.class::isInstance).map(VectorWay.class::cast)
            .max(Comparator.comparingInt(IWay::getNodesCount)).orElseGet(() -> new VectorWay("mapillary-sequences"));
        synchronized (JsonSequencesDecoder.class) {
            VectorDataSetUtils.tryWrite(data, () -> {
                nodes.stream().map(VectorNode::getReferrers).flatMap(Collection::stream)
                    .filter(VectorWay.class::isInstance).map(VectorWay.class::cast).distinct()
                    .forEach(way -> way.setNodes(Collections.emptyList()));
                sequence.setNodes(nodes);
            });
        }
        return Collections.singletonList(sequence);
    }
}
