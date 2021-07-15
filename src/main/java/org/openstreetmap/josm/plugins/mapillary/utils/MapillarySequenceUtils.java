package org.openstreetmap.josm.plugins.mapillary.utils;

import org.apache.commons.jcs3.access.CacheAccess;
import org.openstreetmap.josm.data.cache.JCSCacheManager;
import org.openstreetmap.josm.data.osm.BBox;
import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.data.osm.IWay;
import org.openstreetmap.josm.data.vector.VectorWay;
import org.openstreetmap.josm.plugins.mapillary.cache.Caches;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.oauth.OAuthUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.api.JsonDecoder;
import org.openstreetmap.josm.plugins.mapillary.utils.api.JsonSequencesDecoder;
import org.openstreetmap.josm.tools.Logging;
import org.openstreetmap.josm.tools.Utils;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import javax.json.Json;
import javax.json.JsonObject;
import javax.json.JsonReader;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

public class MapillarySequenceUtils {
  /**
   * An enum for next/previous, largely to help ensure that booleans aren't accidentally reversed
   */
  public enum NextOrPrevious {
    /** Get the "next" image */
    NEXT,
    /** Get the "previous" image */
    PREVIOUS;
  }

  private static final CacheAccess<String, IWay<?>> SEQUENCE_CACHE = JCSCacheManager.getCache("mapillary:sequences");

  /** The unique identifier for the sequence */
  public static final String KEY = "id";
  /** The user who created the sequence */
  public static final String CREATED_BY = "created_by";
  /** The group that owns the sequence */
  public static final String OWNED_BY = "owned_by";
  /** The time that the sequence was created at */
  public static final String CREATED_AT = "created_at";

  /**
   * Get the next or previous picture
   *
   * @param node The node
   * @param next {@code true} for next, {@code false} for previous
   * @return The expected node, if it exists
   */
  @Nullable
  public static INode getNextOrPrevious(@Nonnull INode node, @Nullable NextOrPrevious next) {
    Collection<IWay<?>> connectedWays = node.getReferrers().stream().filter(IWay.class::isInstance)
      .map(IWay.class::cast).map(way -> (IWay<?>) way).distinct().collect(Collectors.toList());
    if (connectedWays.isEmpty() || connectedWays.size() > 2) {
      return null;
    }
    final int initialSize = connectedWays.stream().mapToInt(IWay::getRealNodesCount).sum() - 1;
    List<INode> nodes = new ArrayList<>(Math.max(initialSize, 0));
    for (IWay<?> way : connectedWays) {
      if (nodes.isEmpty()) {
        nodes.addAll(way.getNodes());
      } else if (way.firstNode().equals(nodes.get(nodes.size() - 1))) {
        List<INode> newNodes = new ArrayList<>(way.getNodes());
        newNodes.removeIf(node::equals);
        nodes.addAll(newNodes);
      } else if (way.lastNode().equals(nodes.get(0))) {
        List<INode> newNodes = new ArrayList<>(way.getNodes());
        newNodes.removeIf(node::equals);
        nodes.addAll(newNodes);
        nodes.addAll(0, newNodes);
      }
    }
    final INode nodeOnCurrentSequence;
    int index = nodes.indexOf(node);
    if (next == NextOrPrevious.NEXT && index + 1 < nodes.size()) {
      nodeOnCurrentSequence = nodes.get(index + 1);
    } else if (next == NextOrPrevious.PREVIOUS && index > 0) {
      nodeOnCurrentSequence = nodes.get(index - 1);
    } else {
      nodeOnCurrentSequence = null;
    }
    final IWay<?> way;
    if (connectedWays.size() == 1) {
      way = connectedWays.iterator().next();
    } else if (next == NextOrPrevious.NEXT) {
      way = connectedWays.stream().filter(way2 -> way2.firstNode().equals(node)).findFirst().orElse(null);
    } else if (next == NextOrPrevious.PREVIOUS) {
      way = connectedWays.stream().filter(way2 -> way2.lastNode().equals(node)).findFirst().orElse(null);
    } else {
      way = null;
    }
    if (nodeOnCurrentSequence != null && !MapillaryImageUtils.IS_IMAGE.test(nodeOnCurrentSequence) && way != null
      && way.isFirstLastNode(nodeOnCurrentSequence)) {
      // We are probably on a tile boundary.
      final BBox searchBBox = new BBox();
      searchBBox.addLatLon(nodeOnCurrentSequence.getCoor(), 0.001);
      List<IWay<?>> ways = new ArrayList<>(node.getDataSet().searchWays(searchBBox));
      final String sequenceKey = MapillaryImageUtils.getSequenceKey(node) != null
        ? MapillaryImageUtils.getSequenceKey(node)
        : "";
      ways.removeIf(tWay -> !tWay.hasKeys());
      ways.removeIf(tWay -> !sequenceKey.equals(tWay.get(KEY)));
      // Deliberate reference equality
      ways.removeIf(tWay -> way == tWay);
      ways = ways.stream().distinct().collect(Collectors.toList());
      if (ways.size() == 1) {
        IWay<?> newWay = ways.get(0);
        INode newNode = newWay.getNodes().stream()
          .filter(tNode -> nodeOnCurrentSequence.getCoor().equals(tNode.getCoor())).findFirst().orElse(null);
        if (MapillaryImageUtils.IS_IMAGE.test(newNode)) {
          return newNode;
        } else if (newNode != null) {
          return getNextOrPrevious(newNode, next);
        }
      }
    }
    return nodeOnCurrentSequence;
  }

  /**
   * Get the key for a sequence
   *
   * @param sequence The sequence with a key
   * @return The sequence key, or {@code ""} if no key exists
   */
  public static String getKey(IWay<?> sequence) {
    if (sequence != null && sequence.hasKey(KEY)) {
      return sequence.get(KEY);
    } else if (sequence != null && sequence.getReferrers().size() == 1 && sequence.getReferrers().get(0).hasKey(KEY)) {
      return sequence.getReferrers().get(0).get(KEY);
    }
    return "";
  }

  /**
   * Check if the sequence has a sequence key
   *
   * @param sequence The sequence to check
   * @return {@code true} if the sequence has a sequence key
   */
  public static boolean hasKey(IWay<?> sequence) {
    return !"".equals(getKey(sequence));
  }

  /**
   * Get the sequence for a key
   *
   * @param key The sequence key
   * @return The sequence (full)
   */
  public static IWay<?> getSequence(String key) {
    if (key == null || Utils.isStripEmpty(key)) {
      return null;
    }
    // There should be a method to get a sequence in v4
    IWay<?> sequence = SEQUENCE_CACHE.get(key);
    if (sequence == null) {
      sequence = downloadSequence(key);
      // Ensure that we don't cache a null sequence -- this will throw an InvalidArgumentException if the sequence is
      // null
      // which is why we cannot use {@link CacheAccess#get(Object, Supplier)}
      if (sequence != null) {
        SEQUENCE_CACHE.put(key, sequence);
      }
    }
    return sequence;
  }

  /**
   * Download a specific sequence
   *
   * @implNote This is synchronized to avoid a CME (JOSM #20948).
   * @param key The key to download
   * @return The downloaded sequence
   */
  private static synchronized IWay<?> downloadSequence(final String key) {
    final String sequenceUrl = MapillaryURL.APIv4.getImagesBySequences(key);
    final String data = Caches.META_DATA_CACHE.get(sequenceUrl, () -> {
      try {
        return OAuthUtils.getWithHeader(new URL(sequenceUrl)).toString();
      } catch (IOException e) {
        Logging.error(e);
        return null;
      }
    });
    if (data == null) {
      return null;
    }

    try (JsonReader jsonReader = Json.createReader(new ByteArrayInputStream(data.getBytes(StandardCharsets.UTF_8)))) {
      JsonObject json = jsonReader.readObject();
      Collection<VectorWay> seq = JsonDecoder.decodeData(json, JsonSequencesDecoder::decodeSequence);
      VectorWay sequence = seq.stream().findFirst().orElse(null);
      if (sequence != null && MapillaryLayer.hasInstance()) {
        MapillaryLayer.getInstance().getData().addPrimitive(sequence);
        if (sequence.getDataSet() != null) {
          sequence.getNodes().stream().filter(node -> node.getDataSet() == null)
            .forEach(node -> sequence.getDataSet().addPrimitive(node));
        }
      }
      return sequence;
    }
  }

  /**
   * Get the time the sequence was created at
   *
   * @param sequence The sequence
   * @return The instant the sequence was created, or {@link Instant#EPOCH} if unknown.
   */
  public static Instant getCreatedAt(IWay<?> sequence) {
    if (sequence.hasKey(CREATED_AT)) {
      return Instant.parse(sequence.get(CREATED_AT));
    }
    return Instant.EPOCH;
  }

  private MapillarySequenceUtils() {
    // No-op
  }
}
