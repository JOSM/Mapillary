// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import org.apache.commons.jcs3.access.CacheAccess;
import org.openstreetmap.josm.data.cache.JCSCacheManager;
import org.openstreetmap.josm.data.osm.BBox;
import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.data.osm.IPrimitive;
import org.openstreetmap.josm.data.osm.IWay;

/**
 * Utils for Mapillary sequences
 */
public final class MapillarySequenceUtils {
    /**
     * An enum for next/previous, largely to help ensure that booleans aren't accidentally reversed
     */
    public enum NextOrPrevious {
        /** Get the "next" image */
        NEXT,
        /** Get the "previous" image */
        PREVIOUS
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
        List<IWay<?>> connectedWays = node.getReferrers().stream().filter(IWay.class::isInstance).map(w -> (IWay<?>) w)
            .collect(Collectors.toList());
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
            INode expectedNext = nodes.get(++index);
            while (!MapillaryImageUtils.isImage(expectedNext) && index < nodes.size() - 1) {
                expectedNext = nodes.get(++index);
            }
            nodeOnCurrentSequence = expectedNext;
        } else if (next == NextOrPrevious.PREVIOUS && index > 0) {
            INode expectedPrevious = nodes.get(--index);
            while (!MapillaryImageUtils.isImage(expectedPrevious) && index > 0) {
                expectedPrevious = nodes.get(--index);
            }
            nodeOnCurrentSequence = expectedPrevious;
        } else {
            nodeOnCurrentSequence = null;
        }
        final IWay<?> way;
        if (connectedWays.size() == 1) {
            way = connectedWays.iterator().next();
        } else if (next == NextOrPrevious.NEXT) {
            way = connectedWays.stream().filter(way2 -> node.equals(way2.firstNode())).findFirst().orElse(null);
        } else if (next == NextOrPrevious.PREVIOUS) {
            way = connectedWays.stream().filter(way2 -> node.equals(way2.lastNode())).findFirst().orElse(null);
        } else {
            way = null;
        }
        if (nodeOnCurrentSequence != null && !MapillaryImageUtils.isImage(nodeOnCurrentSequence) && way != null
            && way.isFirstLastNode(nodeOnCurrentSequence)) {
            // We are probably on a tile boundary.
            final BBox searchBBox = new BBox();
            searchBBox.addLatLon(nodeOnCurrentSequence.getCoor(), 0.001);
            List<IWay<?>> ways = new ArrayList<>(node.getDataSet().searchWays(searchBBox));
            final String sequenceKey = MapillaryImageUtils.getSequenceKey(node) != null
                ? MapillaryImageUtils.getSequenceKey(node)
                : "";
            ways.removeIf(tWay -> !tWay.hasKeys());
            ways.removeIf(tWay -> sequenceKey != null && !sequenceKey.equals(tWay.get(KEY)));
            // Deliberate reference equality
            ways.removeIf(tWay -> way == tWay);
            ways = ways.stream().distinct().collect(Collectors.toList());
            if (ways.size() == 1) {
                IWay<?> newWay = ways.get(0);
                INode newNode = newWay.getNodes().stream()
                    .filter(tNode -> nodeOnCurrentSequence.getCoor().equals(tNode.getCoor())).findFirst().orElse(null);
                if (MapillaryImageUtils.isImage(newNode)) {
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
        if (sequence == null) {
            return "";
        }
        if (sequence.hasKey(KEY)) {
            return sequence.get(KEY);
        }
        final List<? extends IPrimitive> referrers = sequence.getReferrers();
        if (referrers.size() == 1 && referrers.get(0).hasKey(KEY)) {
            // This is a heavy hit once, when we are initially getting the key, but it reduces expensive calls to
            // getReferrers later.
            referrers.get(0).getKeys().forEach(sequence::put);
            return sequence.get(KEY);
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

    private MapillarySequenceUtils() {
        // No-op
    }
}
