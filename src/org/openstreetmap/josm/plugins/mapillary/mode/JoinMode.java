package org.openstreetmap.josm.plugins.mapillary.mode;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.gui.layer.OsmDataLayer;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;

public class JoinMode extends AbstractMode {
  
  @Override
  public void mousePressed(MouseEvent e) {
    
  }
  
  @Override
  public void mouseMoved(MouseEvent e) {
    MapillaryAbstractImage closestTemp = getClosest(e.getPoint());
    if (!(Main.map.mapView.getActiveLayer() instanceof MapillaryLayer))
      return;
    data.setHighlightedImage(closestTemp);
  }
}
