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
     * @return the {@link Path2D} object to which the {@link IWay} has been converted
     */
    public static Path2D getSequencePath(NavigatableComponent nc, IWay<?> seq) {
        final boolean anyVisible = seq.getNodes().stream().filter(MapillaryImageUtils::isImage)
            .filter(node -> node.isReferredByWays(1)).anyMatch(IPrimitive::isVisible);
        Path2D.Double path = null;
        for (INode node : seq.getNodes()) {
            if (node == null || (!node.isVisible() || !MapillaryImageUtils.isImage(node))
                && !(anyVisible && seq.isFirstLastNode(node))) {
                continue;
            }
            Point2D p = nc.getPoint2D(node.getEastNorth());
            if (path == null) {
                path = new Path2D.Double(Path2D.WIND_NON_ZERO, seq.getNodesCount());
                path.moveTo(p.getX(), p.getY());
            } else {
                path.lineTo(p.getX(), p.getY());
            }
        }
        return path != null ? path : new Path2D.Double();
    }
}
