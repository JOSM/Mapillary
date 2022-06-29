// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import java.awt.Graphics2D;
import java.awt.Shape;
import java.awt.geom.Path2D;
import java.awt.geom.Point2D;

import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.data.osm.IPrimitive;
import org.openstreetmap.josm.data.osm.IWay;
import org.openstreetmap.josm.gui.MapView;
import org.openstreetmap.josm.gui.NavigatableComponent;

/**
 * Utility class to convert entities like {@link Bounds} and {@link IWay} into {@link Shape}s that
 * can then easily be drawn on a {@link MapView}s {@link Graphics2D}-context.
 */
public final class MapViewGeometryUtil {
    private MapViewGeometryUtil() {
        // Private constructor to avoid instantiation
    }

    /**
     * Converts a {@link IWay} into a {@link Path2D} that can be drawn
     * on the specified {@link NavigatableComponent}'s {@link Graphics2D}-context.
     *
     * @param nc the {@link NavigatableComponent} for which this conversion should be performed, typically a
     *        {@link MapView}
     * @param seq the sequence to convert
     * @param x The x coordinates
     * @param y The y coordinates
     * @return the number of points in x/y
     */
    public static int getSequencePath(NavigatableComponent nc, IWay<?> seq, int[] x, int[] y) {
        final boolean anyVisible = seq.getNodes().stream().filter(MapillaryImageUtils::isImage)
            .filter(node -> node.isReferredByWays(1)).anyMatch(IPrimitive::isVisible);
        int index = 0;
        for (INode node : seq.getNodes()) {
            if (node == null || (!node.isVisible() || !MapillaryImageUtils.isImage(node))
                && !(anyVisible && seq.isFirstLastNode(node))) {
                continue;
            }
            Point2D p = nc.getPoint2D(node);
            x[index] = (int) p.getX();
            y[index] = (int) p.getY();
            index++;
        }
        return index;
    }
}
