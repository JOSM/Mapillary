package org.openstreetmap.josm.plugins.mapillary.gui;

import java.awt.Point;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.locks.Lock;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import javax.swing.event.MouseInputAdapter;

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
    public DataMouseListener() {
        MainApplication.getMap().mapView.addMouseListener(this);
        MainApplication.getMap().mapView.addMouseMotionListener(this);
    }

    @Override
    public void mouseClicked(MouseEvent e) {
        super.mouseClicked(e);
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
        Collection<INode> nodes = searchNodes(layer, searchBBox).stream().distinct()
            .filter(AbstractPrimitive::isVisible).filter(INode.class::isInstance).map(INode.class::cast)
            .collect(Collectors.toList());
        if (!nodes.isEmpty()) {
            // This is needed since Mapillary ids are only unique within a tile.
            layer.getData().setSelected(nodes);
            nodes.stream().filter(AbstractPrimitive.class::isInstance).map(AbstractPrimitive.class::cast)
                .forEach(IPrimitive::isSelected);
        } else if (layer instanceof MapillaryLayer) {
            if (e.getClickCount() >= MapillaryProperties.DESELECT_CLICK_COUNT.get()) {
                layer.getData().clearSelection();
                ((MapillaryLayer) layer).setCurrentImage(null);
            }
        } else {
            Collection<VectorWay> ways = layer.getData().searchWays(searchBBox);
            if (!ways.isEmpty()) {
                layer.getData().setSelected(ways);
            }
        }
    }

    @Override
    public void mouseMoved(MouseEvent e) {
        super.mouseMoved(e);
        final BBox searchBBox = getSmallBBox(e.getPoint());
        for (MVTLayer layer : MainApplication.getLayerManager().getLayersOfType(MVTLayer.class)) {
            if (layer instanceof MapillaryLayer || layer instanceof PointObjectLayer) {
                final Lock readLock = layer.getData().getReadLock();
                if (readLock.tryLock()) {
                    layer.getData().getPrimitivesById(layer.getData().getHighlighted().toArray(new PrimitiveId[0]))
                        .forEach(vectorPrimitive -> vectorPrimitive.setHighlighted(false));
                    try {
                        if (searchBBox == null) {
                            layer.getData().setHighlighted(Collections.emptyList());
                            continue;
                        }
                        if (layer instanceof MapillaryLayer) {
                            MapillaryNode node = ((MapillaryLayer) layer).getImage();
                            if (node != null && node.getSequence() != null) {
                                node.getSequence().getNodes().forEach(n -> n.setHighlighted(false));
                            }
                        }
                        List<? extends AbstractPrimitive> nodes = searchNodes(layer, searchBBox);
                        if (!nodes.isEmpty()) {
                            layer.getData().setHighlighted(
                                nodes.stream().map(IPrimitive::getPrimitiveId).collect(Collectors.toSet()));
                            nodes.forEach(node -> node.setHighlighted(true));
                            layer.invalidate();
                            continue;
                        }
                        Collection<VectorWay> ways = SubclassFilteredCollection.filter(
                            layer.getData().searchWays(searchBBox),
                            way -> convertToWaySegmentBBox(way).anyMatch(searchBBox::intersects));
                        if (!ways.isEmpty()) {
                            layer.getData().setHighlighted(
                                ways.stream().map(IPrimitive::getPrimitiveId).collect(Collectors.toSet()));
                            layer.invalidate();
                        } else {
                            layer.getData().setHighlighted(Collections.emptyList());
                        }
                    } finally {
                        readLock.unlock();
                    }
                }
            }
        }
    }

    private static <N extends INode, W extends IWay<N>> Stream<BBox> convertToWaySegmentBBox(W way) {
        // TODO: Stream.iterate would be good here (Java 9)
        Stream.Builder<BBox> builder = Stream.builder();
        for (int i = 0; i < way.getNodesCount() - 1; i++) {
            BBox bbox = new BBox(way.getNode(i));
            bbox.add(way.getNode(i + 1));
            builder.add(bbox);
        }
        return builder.build();
    }

    private static List<? extends AbstractPrimitive> searchNodes(MVTLayer layer, BBox searchBBox) {
        if (searchBBox == null) {
            return Collections.emptyList();
        }
        if (layer instanceof MapillaryLayer) {
            final MapillaryNode image = ((MapillaryLayer) layer).getImage();
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
        return layer.getData().searchNodes(searchBBox);
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
