package org.openstreetmap.josm.plugins.mapillary.gui;

import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.data.osm.BBox;
import org.openstreetmap.josm.data.osm.IPrimitive;
import org.openstreetmap.josm.data.vector.VectorNode;
import org.openstreetmap.josm.data.vector.VectorWay;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.layer.imagery.MVTLayer;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.PointObjectLayer;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.tools.Destroyable;

import javax.swing.SwingUtilities;
import javax.swing.event.MouseInputAdapter;
import java.awt.Point;
import java.awt.event.MouseEvent;
import java.util.Collection;
import java.util.Collections;
import java.util.stream.Collectors;

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
      if (!(layer instanceof MapillaryLayer || layer instanceof PointObjectLayer) || !layer.isVisible()) {
        continue;
      }
      Collection<VectorNode> nodes = layer.getData().searchNodes(searchBBox).stream().distinct()
        .collect(Collectors.toList());
      if (!nodes.isEmpty()) {
        // This is needed since Mapillary ids are only unique within a tile.
        if (layer instanceof MapillaryLayer) {
          ((MapillaryLayer) layer).setSelected(nodes);
        }
        layer.getData().setSelected(nodes);
        if (nodes.size() == 1 && MapillaryMainDialog.hasInstance()) {
          SwingUtilities.invokeLater(() -> MapillaryMainDialog.getInstance().setImage(nodes.iterator().next()));
        }
        continue;
      } else if (layer instanceof MapillaryLayer) {
        if (e.getClickCount() >= MapillaryProperties.DESELECT_CLICK_COUNT.get()) {
          ((MapillaryLayer) layer).setSelected(Collections.emptyList());
          layer.getData().clearSelection();
        }
        continue;
      }
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
      if (!(layer instanceof MapillaryLayer || layer instanceof PointObjectLayer)) {
        continue;
      }
      Collection<VectorNode> nodes = layer.getData().searchNodes(searchBBox);
      if (!nodes.isEmpty()) {
        layer.getData().setHighlighted(nodes.stream().map(IPrimitive::getPrimitiveId).collect(Collectors.toSet()));
        layer.invalidate();
        continue;
      }
      Collection<VectorWay> ways = layer.getData().searchWays(searchBBox);
      if (!ways.isEmpty()) {
        layer.getData().setHighlighted(ways.stream().map(IPrimitive::getPrimitiveId).collect(Collectors.toSet()));
        layer.invalidate();
      }
    }
  }

  private static BBox getSmallBBox(Point point) {
    double scale = MainApplication.getMap().mapView.getScale();
    // This is ~z5
    if (scale > 10) {
      return null;
    }

    final LatLon latLon = MainApplication.getMap().mapView.getLatLon(point.getX(), point.getY());
    final BBox bbox = new BBox();
    // TODO fiddle with the scale
    // ~.07 at z12
    bbox.addLatLon(latLon, 0.0001 * scale);
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
