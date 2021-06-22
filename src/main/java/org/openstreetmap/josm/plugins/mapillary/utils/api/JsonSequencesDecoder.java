// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import org.openstreetmap.josm.data.osm.IWay;
import org.openstreetmap.josm.data.vector.VectorNode;
import org.openstreetmap.josm.data.vector.VectorWay;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.io.download.MapillaryDownloader;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.tools.Logging;

import javax.json.JsonArray;
import javax.json.JsonNumber;
import javax.json.JsonObject;
import javax.json.JsonString;
import javax.json.JsonValue;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * Decodes the JSON returned by {@link APIv4} into Java objects.
 * Takes a {@link JsonObject} and {@link #decodeSequence(JsonObject)} tries to convert it to a
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
   * @return a singleton list of {@link IWay} that is parsed from the given {@link JsonObject}. If mandatory information
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
    final List<String> imageIds = ((JsonArray) json)
      .getValuesAs(value -> value instanceof JsonObject ? (JsonObject) value : null).stream().filter(Objects::nonNull)
      .filter(image -> image.containsKey("id")).map(image -> image.get("id"))
      .filter(jsonObject -> jsonObject instanceof JsonString || jsonObject instanceof JsonNumber)
      .map(value -> value instanceof JsonString ? ((JsonString) value).getString() : ((JsonNumber) value).toString())
      .collect(Collectors.toList());
    final Collection<String> currentImageIds = MapillaryLayer.getInstance().getData().getNodes().stream()
      .map(MapillaryImageUtils::getKey).filter(imageIds::contains).collect(Collectors.toSet());
    MapillaryDownloader
      .downloadImages(imageIds.stream().filter(id -> !currentImageIds.contains(id)).toArray(String[]::new));
    final List<VectorNode> nodes = MapillaryLayer.getInstance().getData().getNodes().stream()
      .filter(node -> imageIds.contains(MapillaryImageUtils.getKey(node)))
      .sorted(Comparator.comparingInt(node -> imageIds.indexOf(MapillaryImageUtils.getKey(node))))
      .collect(Collectors.toList());
    if (nodes.isEmpty()) {
      Logging.error("Mapillary: The sequence does not have any nodes");
      return Collections.emptyList();
    }
    final VectorWay sequence = nodes.stream().map(MapillaryImageUtils::getSequence).filter(VectorWay.class::isInstance)
      .map(VectorWay.class::cast).max(Comparator.comparingInt(IWay::getNodesCount))
      .orElseGet(() -> new VectorWay("mapillary-sequences"));
    synchronized (JsonSequencesDecoder.class) {
      nodes.stream().map(VectorNode::getReferrers).flatMap(Collection::stream).filter(VectorWay.class::isInstance)
        .map(VectorWay.class::cast).distinct().forEach(way -> way.setNodes(Collections.emptyList()));
      sequence.setNodes(nodes);
    }
    return Collections.singletonList(sequence);
  }
}
