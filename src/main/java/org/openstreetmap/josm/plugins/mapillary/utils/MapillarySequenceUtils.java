package org.openstreetmap.josm.plugins.mapillary.utils;

import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.data.osm.IWay;

import java.time.Instant;
import java.util.ArrayList;
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

  private MapillarySequenceUtils() {
    // No-op
  }

  public static final String KEY = "key";
  public static final String ID = "id";
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
  public static INode getNextOrPrevious(INode node, NextOrPrevious next) {
    IWay<?> way = node.getReferrers().stream().filter(IWay.class::isInstance).map(IWay.class::cast).findFirst()
      .orElse(null);
    if (way == null) {
      return null;
    }
    List<? extends INode> nodes = way.getNodes();
    final INode nodeOnCurrentSequence;
    int index = nodes.indexOf(node);
    if (next == NextOrPrevious.NEXT && index + 1 < nodes.size()) {
      nodeOnCurrentSequence = nodes.get(index + 1);
    } else if (next == NextOrPrevious.PREVIOUS && index > 0) {
      nodeOnCurrentSequence = nodes.get(index - 1);
    } else {
      nodeOnCurrentSequence = null;
    }
    if (nodeOnCurrentSequence != null && !MapillaryImageUtils.IS_IMAGE.test(nodeOnCurrentSequence)
      && way.isFirstLastNode(nodeOnCurrentSequence)) {
      // We are probably on a tile boundary.
      List<IWay<?>> ways = new ArrayList<>(node.getDataSet().searchWays(nodeOnCurrentSequence.getBBox()));
      String sequenceKey = node.hasKey(MapillaryImageUtils.SEQUENCE_KEY) ? node.get(MapillaryImageUtils.SEQUENCE_KEY)
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
    }
    return "";
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
}
