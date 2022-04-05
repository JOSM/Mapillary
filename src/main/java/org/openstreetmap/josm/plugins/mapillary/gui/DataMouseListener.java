package org.openstreetmap.josm.plugins.mapillary.gui;

import java.awt.Point;
import java.awt.event.MouseEvent;
import java.util.Collection;
import java.util.Collections;
import java.util.concurrent.locks.Lock;
import java.util.stream.Collectors;

import javax.swing.event.MouseInputAdapter;

import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.data.osm.AbstractPrimitive;
import org.openstreetmap.josm.data.osm.BBox;
import org.openstreetmap.josm.data.osm.IPrimitive;
import org.openstreetmap.josm.data.projection.ProjectionRegistry;
import org.openstreetmap.josm.data.vector.VectorNode;
import org.openstreetmap.josm.data.vector.VectorWay;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.layer.geoimage.ImageViewerDialog;
import org.openstreetmap.josm.gui.layer.imagery.MVTLayer;
import org.openstreetmap.josm.gui.util.GuiHelper;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.PointObjectLayer;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.geoimage.MapillaryImageEntry;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.tools.Destroyable;
import org.openstreetmap.josm.tools.Geometry;
import org.openstreetmap.josm.tools.HiDPISupport;

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
        Collection<VectorNode> nodes = layer.getData().searchNodes(searchBBox).stream().distinct()
            .filter(AbstractPrimitive::isVisible).collect(Collectors.toList());
        if (!nodes.isEmpty()) {
            // This is needed since Mapillary ids are only unique within a tile.
            layer.getData().setSelected(nodes);
            if (layer instanceof MapillaryLayer && nodes.size() == 1) {
                GuiHelper.runInEDT(() -> ImageViewerDialog.getInstance()
                    .displayImage(MapillaryImageEntry.getCachedEntry(nodes.iterator().next())));
            }
        } else if (layer instanceof MapillaryLayer) {
            if (e.getClickCount() >= MapillaryProperties.DESELECT_CLICK_COUNT.get()) {
                layer.getData().clearSelection();
                layer.getData().clearSelection();
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
                    try {
                        Collection<VectorNode> nodes = layer.getData().searchNodes(searchBBox);
                        if (!nodes.isEmpty()) {
                            layer.getData().setHighlighted(
                                nodes.stream().map(IPrimitive::getPrimitiveId).collect(Collectors.toSet()));
                            layer.invalidate();
                            continue;
                        }
                        Collection<VectorWay> ways = layer.getData().searchWays(searchBBox);
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

    private static BBox getSmallBBox(Point point) {
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
