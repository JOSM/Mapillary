package org.openstreetmap.josm.plugins.mapillary.gui;

import static org.openstreetmap.josm.data.projection.Ellipsoid.WGS84;

import org.openstreetmap.josm.data.Version;
import org.openstreetmap.josm.data.coor.ILatLon;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.data.osm.BBox;
import org.openstreetmap.josm.data.osm.IPrimitive;
import org.openstreetmap.josm.data.projection.ProjectionRegistry;
import org.openstreetmap.josm.data.vector.VectorNode;
import org.openstreetmap.josm.data.vector.VectorWay;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.layer.imagery.MVTLayer;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.PointObjectLayer;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.tools.Destroyable;
import org.openstreetmap.josm.tools.Geometry;
import org.openstreetmap.josm.tools.HiDPISupport;

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
        layer.getData().setSelected(nodes);
        if (layer instanceof MapillaryLayer && nodes.size() == 1 && MapillaryMainDialog.hasInstance()) {
          SwingUtilities.invokeLater(() -> MapillaryMainDialog.getInstance().setImage(nodes.iterator().next()));
        }
        continue;
      } else if (layer instanceof MapillaryLayer) {
        if (e.getClickCount() >= MapillaryProperties.DESELECT_CLICK_COUNT.get()) {
          layer.getData().clearSelection();
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
        continue;
      }
      layer.getData().setHighlighted(Collections.emptyList());
    }
  }

  private static BBox getSmallBBox(Point point) {
    final double scaleInEastNorthUnitsPerPixel = MainApplication.getMap().mapView.getScale();
    final double metersPerPixel = ProjectionRegistry.getProjection().getMetersPerUnit() * scaleInEastNorthUnitsPerPixel;
    // This is ~z5
    if (metersPerPixel > 1) {
      return null;
    }

    final LatLon latLon = MainApplication.getMap().mapView.getLatLon(point.getX(), point.getY());
    final BBox bbox = new BBox();
    // TODO fiddle with the scale
    // ~.07 at z12
    bbox.add(latLon);
    final double pixelBuffer = HiDPISupport.getHiDPIScale() * 5;
    bbox.add(getLatLon(latLon, Math.PI / 4, metersPerPixel * pixelBuffer));
    bbox.add(getLatLon(latLon, 5 * Math.PI / 4, metersPerPixel * pixelBuffer));
    return bbox;
  }

  @Override
  public void destroy() {
    if (MainApplication.getMap() != null && MainApplication.getMap().mapView != null) {
      MainApplication.getMap().mapView.removeMouseListener(this);
      MainApplication.getMap().mapView.removeMouseMotionListener(this);
    }
  }

  /**
   * Create a new LatLon at a specified distance. Uses WGS84.
   * NOTE: Copied from another plugin (unreleased). TODO: Add to Geometry?
   *
   * @param original The originating point
   * @param angle The angle (from true north) in radians
   * @param offset The distance to the new point (meters)
   * @return The new latlon
   */
  private static ILatLon getLatLon(final ILatLon original, final double angle, final double offset) {
    if (Version.getInstance().getVersion() < 18109) {
      final double deltaLongitudeDegree = (Math.PI * WGS84.a * Math.cos(Math.toRadians(original.lat())))
        / (180 * Math.sqrt(1 - WGS84.e2 * Math.pow(Math.sin(Math.toRadians(original.lat())), 2)));
      final double deltaLatitudeDegree = (Math.PI * WGS84.a * (1 - WGS84.e2))
        / (180 * Math.pow(1 - WGS84.e2 * Math.pow(Math.sin(original.lat()), 2), 1.5));
      final double dx = offset * Math.sin(angle);
      final double dy = offset * Math.cos(angle);
      final double dLon = dx / deltaLongitudeDegree;
      final double dLat = dy / deltaLatitudeDegree;
      return new LatLon(original.lat() + dLat, original.lon() + dLon);
    }
    return Geometry.getLatLonFrom(original, angle, offset);
  }
}
