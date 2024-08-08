// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui;

import java.awt.Point;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.locks.Lock;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import javax.swing.event.MouseInputAdapter;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.data.osm.AbstractPrimitive;
import org.openstreetmap.josm.data.osm.BBox;
import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.data.osm.IPrimitive;
import org.openstreetmap.josm.data.osm.IWay;
import org.openstreetmap.josm.data.osm.PrimitiveId;
import org.openstreetmap.josm.data.projection.ProjectionRegistry;
import org.openstreetmap.josm.data.vector.VectorWay;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.layer.imagery.MVTLayer;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.MapillaryNode;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.PointObjectLayer;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.tools.Destroyable;
import org.openstreetmap.josm.tools.Geometry;
import org.openstreetmap.josm.tools.HiDPISupport;
import org.openstreetmap.josm.tools.SubclassFilteredCollection;

/**
 * A basic class to listen for mouse events in the map
 */
public class DataMouseListener extends MouseInputAdapter implements Destroyable {
    /**
     * Create a new listener for the mouse location
     */
    public DataMouseListener() {
        MainApplication.getMap().mapView.addMouseListener(this);
        MainApplication.getMap().mapView.addMouseMotionListener(this);
    }

    @Override
    public void mouseClicked(MouseEvent e) {
        super.mouseClicked(e);
        if (e.getButton() != MouseEvent.BUTTON1) {
            return;
        }
        final BBox searchBBox = getSmallBBox(e.getPoint());
        // Don't bother if we don't have a usable bbox
        if (searchBBox == null) {
            return;
        }
        for (MVTLayer layer : MainApplication.getLayerManager().getLayersOfType(MVTLayer.class)) {
            if ((layer instanceof MapillaryLayer || layer instanceof PointObjectLayer) && layer.isVisible()) {
                final Lock readLock = layer.getData().getReadLock();
                if (readLock.tryLock()) {
                    try {
                        mouseClickedInner(e, layer, searchBBox);
                    } finally {
                        readLock.unlock();
                    }
                }
            }
        }
    }

    /**
     * Just a method to extract logic from {@link #mouseClicked(MouseEvent)}
     *
     * @param e The mouse event
     * @param layer The layer we found
     * @param searchBBox The bbox to search
     */
    private static void mouseClickedInner(final MouseEvent e, final MVTLayer layer, final BBox searchBBox) {
        LatLon click = MainApplication.getMap().mapView.getLatLon(e.getX(), e.getY());
        Optional<INode> nodes = searchNodes(layer, searchBBox).stream().distinct().filter(AbstractPrimitive::isDrawable)
            .filter(INode.class::isInstance).map(INode.class::cast).filter(i -> i.getUniqueId() > 0)
            .min(Comparator.comparingDouble(i -> i.distanceSq(click)));
        if (nodes.isPresent()) {
            // This is needed since Mapillary ids are only unique within a tile.
            layer.getData().setSelected(Collections.singletonList(nodes.get()));
        } else if (layer instanceof MapillaryLayer mapillaryLayer) {
            if (e.getClickCount() >= MapillaryProperties.DESELECT_CLICK_COUNT.get()) {
                layer.getData().clearSelection();
                mapillaryLayer.setCurrentImage(null);
            }
        } else {
            Collection<VectorWay> ways = layer.getData().searchWays(searchBBox);
            ways.removeIf(way -> way.getUniqueId() <= 0);
            if (!ways.isEmpty()) {
                List<IPrimitive> primitives = layer.getData().getSelectedNodes().stream()
                    .filter(n -> ways.stream().anyMatch(w -> w.getNodes().contains(n)))
                    .collect(Collectors.toCollection(ArrayList::new));
                primitives.addAll(ways);
                layer.getData().setSelected(primitives);
            } else {
                layer.getData().clearSelection();
            }
        }
    }

    @Override
    public void mouseMoved(MouseEvent e) {
        super.mouseMoved(e);
        final BBox searchBBox = getSmallBBox(e.getPoint());
        for (MVTLayer layer : MainApplication.getLayerManager().getLayersOfType(MVTLayer.class)) {
            if (layer instanceof MapillaryLayer || layer instanceof PointObjectLayer) {
                performHighlight(layer, searchBBox);
            }
        }
    }

    private static void performHighlight(MVTLayer layer, BBox searchBBox) {
        final Lock readLock = layer.getData().getReadLock();
        if (readLock.tryLock()) {
            layer.getData().getPrimitivesById(layer.getData().getHighlighted().toArray(new PrimitiveId[0]))
                .forEach(vectorPrimitive -> vectorPrimitive.setHighlighted(false));
            try {
                if (searchBBox == null) {
                    layer.getData().setHighlighted(Collections.emptyList());
                    return;
                }
                if (layer instanceof MapillaryLayer mapillaryLayer) {
                    MapillaryNode node = mapillaryLayer.getImage();
                    if (node != null && node.getSequence() != null) {
                        node.getSequence().getNodes().forEach(n -> n.setHighlighted(false));
                    }
                }
                List<? extends AbstractPrimitive> nodes = searchNodes(layer, searchBBox);
                if (!nodes.isEmpty()) {
                    layer.getData().setHighlighted(nodes.stream().filter(IPrimitive::isDrawable)
                        .map(IPrimitive::getPrimitiveId).collect(Collectors.toSet()));
                    nodes.forEach(node -> node.setHighlighted(true));
                    layer.invalidate();
                    return;
                }
                Collection<VectorWay> ways = SubclassFilteredCollection.filter(
                    layer.getData().searchWays(searchBBox),
                    way -> convertToWaySegmentBBox(way).anyMatch(searchBBox::intersects));
                if (!ways.isEmpty()) {
                    layer.getData().setHighlighted(
                        ways.stream().map(IPrimitive::getPrimitiveId).collect(Collectors.toSet()));
                    layer.invalidate();
                } else if (!layer.getData().getHighlighted().isEmpty()) {
                    layer.getData().setHighlighted(Collections.emptyList());
                }
            } finally {
                readLock.unlock();
            }
        }
    }

    private static <N extends INode, W extends IWay<N>> Stream<BBox> convertToWaySegmentBBox(W way) {
        return IntStream.iterate(0, i -> i < way.getNodesCount() - 1, i -> i + 1)
            .mapToObj(i -> {
                BBox bbox = new BBox(way.getNode(i));
                bbox.add(way.getNode(i + 1));
                return bbox;
            });
    }

    private static List<? extends AbstractPrimitive> searchNodes(MVTLayer layer, BBox searchBBox) {
        if (searchBBox == null) {
            return Collections.emptyList();
        }
        if (layer instanceof MapillaryLayer mapillaryLayer) {
            final MapillaryNode image = mapillaryLayer.getImage();
            if (image != null) {
                final List<AbstractPrimitive> nodes;
                if (image.getSequence() != null) {
                    nodes = image.getSequence().getNodes().stream().filter(searchBBox::contains)
                        .collect(Collectors.toCollection(ArrayList::new));
                } else {
                    nodes = new ArrayList<>();
                    if (searchBBox.contains(image)) {
                        nodes.add(image);
                    }
                }
                nodes.addAll(layer.getData().searchNodes(searchBBox));
                return nodes;
            }
        }
        List<? extends AbstractPrimitive> primitives = layer.getData().searchNodes(searchBBox);
        primitives.removeIf(prim -> prim.getUniqueId() <= 0);
        return primitives;
    }

    @Nullable
    private static BBox getSmallBBox(@Nonnull Point point) {
        final double scaleInEastNorthUnitsPerPixel = MainApplication.getMap().mapView.getScale();
        final double metersPerPixel = ProjectionRegistry.getProjection().getMetersPerUnit()
            * scaleInEastNorthUnitsPerPixel;
        // This is ~z5
        if (metersPerPixel > 1) {
            return null;
        }

        final LatLon latLon = MainApplication.getMap().mapView.getLatLon(point.getX(), point.getY());
        final BBox bbox = new BBox();
        bbox.add(latLon);
        final double pixelBuffer = HiDPISupport.getHiDPIScale() * 5;
        bbox.add(Geometry.getLatLonFrom(latLon, Math.PI / 4, metersPerPixel * pixelBuffer));
        bbox.add(Geometry.getLatLonFrom(latLon, 5 * Math.PI / 4, metersPerPixel * pixelBuffer));
        return bbox;
    }

    @Override
    public void destroy() {
        if (MainApplication.getMap() != null && MainApplication.getMap().mapView != null) {
            MainApplication.getMap().mapView.removeMouseListener(this);
            MainApplication.getMap().mapView.removeMouseMotionListener(this);
        }
    }
}
