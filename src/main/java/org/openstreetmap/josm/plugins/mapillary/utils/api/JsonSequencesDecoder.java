// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import org.openstreetmap.josm.data.osm.IWay;
import org.openstreetmap.josm.data.vector.VectorNode;
import org.openstreetmap.josm.data.vector.VectorWay;
import org.openstreetmap.josm.plugins.mapillary.io.download.MapillaryDownloader;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillarySequenceUtils;

import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.JsonValue;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
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
      throw new IllegalArgumentException("The sequence endpoint just returns an array of picture ids");
    }
    final List<String> imageIds = ((JsonArray) json).getValuesAs(JsonObject.class).stream()
      .filter(image -> image.containsKey("id")).map(image -> image.getString("id")).collect(Collectors.toList());
    final Map<String, Collection<VectorNode>> downloadedImages = MapillaryDownloader
      .downloadImages(imageIds.toArray(new String[0]));
    final Optional<Map.Entry<String, Collection<VectorNode>>> bestSequenceEntry = downloadedImages.entrySet().stream()
      .max(Comparator.comparing(entry -> entry.getValue().size()));
    if (bestSequenceEntry.isPresent()) {
      final List<VectorWay> ways = bestSequenceEntry.get().getValue().stream().map(MapillaryImageUtils::getSequence)
        .filter(VectorWay.class::isInstance).map(VectorWay.class::cast).distinct().collect(Collectors.toList());
      if (ways.size() == 1) {
        return Collections.singletonList(ways.get(0));
      } else if (ways.stream().map(MapillarySequenceUtils::getKey).distinct().count() == 1) {
        synchronized (JsonSequencesDecoder.class) {
          final VectorWay first = ways.get(0);
          ways.remove(first);
          ways.forEach(way -> way.setNodes(Collections.emptyList()));
          ways.forEach(way -> way.setDeleted(true));
          ways.forEach(way -> way.setVisible(false));
          first.setNodes(bestSequenceEntry.get().getValue().stream()
            .sorted(Comparator.comparingInt(image -> imageIds.indexOf(MapillaryImageUtils.getKey(image))))
            .collect(Collectors.toList()));
          return Collections.singletonList(first);
        }
      }
    }
    return Collections.emptyList();
  }
}
