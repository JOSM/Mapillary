package org.openstreetmap.josm.plugins.mapillary.mode;

import java.awt.Point;
import java.awt.event.MouseAdapter;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryData;

public abstract class AbstractMode extends MouseAdapter {

  protected MapillaryData data;

  public AbstractMode() {
    super();
  }

  protected MapillaryAbstractImage getClosest(Point clickPoint) {
    double snapDistance = 10;
    double minDistance = Double.MAX_VALUE;
    MapillaryAbstractImage closest = null;
    for (MapillaryAbstractImage image : data.getImages()) {
      Point imagePoint = Main.map.mapView.getPoint(image.getLatLon());
      imagePoint.setLocation(imagePoint.getX(), imagePoint.getY());
      double dist = clickPoint.distanceSq(imagePoint);
      if (minDistance > dist && clickPoint.distance(imagePoint) < snapDistance && image.isVisible()) {
        minDistance = dist;
        closest = image;
      }
    }
    return closest;
  }

}