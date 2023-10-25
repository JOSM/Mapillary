// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.WeakHashMap;

import jakarta.annotation.Nullable;
import org.openstreetmap.josm.data.coor.ILatLon;
import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.data.osm.IWay;
import org.openstreetmap.josm.data.preferences.IntegerProperty;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;

/**
 * Used when there is a bad offset in Mapillary imagery
 *
 * @author Taylor Smock
 */
public final class OffsetUtils {
    private static final IntegerProperty OFFSET_PROPERTY = new IntegerProperty("mapillary.sequence-image.offset", 0);
    private static final Map<String, Integer> SEQUENCE_OFFSET = new WeakHashMap<>();

    private OffsetUtils() {
        // Hide constructor
    }

    /**
     * Get an offset to use to show node location on the map
     *
     * @param node The node to use
     * @return The node offset
     */
    public static int getOffset(@Nullable final INode node) {
        return SEQUENCE_OFFSET.computeIfAbsent(MapillaryImageUtils.getSequenceKey(node), key -> OFFSET_PROPERTY.get());
    }

    public static void setOffset(@Nullable Number number) {
        if (MapillaryLayer.hasInstance() && MapillaryLayer.getInstance().getData().getSelectedNodes().size() == 1) {
            final INode selected = MapillaryLayer.getInstance().getData().getSelectedNodes().iterator().next();
            final String key = MapillaryImageUtils.getSequenceKey(selected);
            if (number == null) {
                SEQUENCE_OFFSET.remove(key);
            } else {
                SEQUENCE_OFFSET.put(key, number.intValue());
            }
        } else {
            if (number == null) {
                OFFSET_PROPERTY.remove();
            } else {
                OFFSET_PROPERTY.put(number.intValue());
            }
        }
    }

    /**
     * Get the offset coordinate for a specificed node
     *
     * @param node The node to get
     * @return The offset location
     */
    public static ILatLon getOffsetLocation(final INode node) {
        final int offset = getOffset(node);
        final IWay<?> sequence = MapillaryImageUtils.getSequence(node);
        if (sequence == null || offset == 0) {
            return node;
        }
        List<? extends INode> nodes = new ArrayList<>(sequence.getNodes());
        final int index = nodes.indexOf(node);
        if (index + offset > nodes.size() - 1) {
            return nodes.get(nodes.size() - 1);
        } else if (index + offset < 0) {
            return nodes.get(0);
        } else {
            return nodes.get(index + offset);
        }
    }
}
