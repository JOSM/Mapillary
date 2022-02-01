// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Shape;
import java.awt.geom.Path2D;
import java.util.Objects;

import org.openstreetmap.josm.data.Bounds;
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
        final Path2D.Double path = new Path2D.Double();
        final boolean anyVisible = seq.getNodes().stream().filter(MapillaryImageUtils::isImage)
            .filter(node -> node.getReferrers().size() == 1).anyMatch(IPrimitive::isVisible);
        seq.getNodes().stream().filter(Objects::nonNull)
            .filter(node -> (node.isVisible() && MapillaryImageUtils.isImage(node))
                || (anyVisible && seq.isFirstLastNode(node)))
            .forEach(img -> {
                Point p = nc.getPoint(img.getCoor());
                if (path.getCurrentPoint() == null) {
                    path.moveTo(p.getX(), p.getY());
                } else {
                    path.lineTo(p.getX(), p.getY());
                }
            });
        return path;
    }
}
