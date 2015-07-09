package org.openstreetmap.josm.plugins.mapillary.mode;

import java.awt.Color;
import java.awt.Cursor;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.event.MouseEvent;
import java.util.ArrayList;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.gui.MapView;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImportedImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.MapillarySequence;

public class JoinMode extends AbstractMode {

  public MapillaryImportedImage lastClick;
  public MouseEvent lastPos;
  
  public JoinMode() {
    cursor = Cursor.CROSSHAIR_CURSOR;
  }

  @Override
  public void mousePressed(MouseEvent e) {
    if (data.getHighlighted() == null)
      return;
    if (lastClick == null && data.getHighlighted() instanceof MapillaryImportedImage) {
      lastClick = (MapillaryImportedImage) data.getHighlighted();
    } else if (lastClick != null && data.getHighlighted() instanceof MapillaryImportedImage) {
      if (((data.getHighlighted().previous() == null && lastClick.next() == null) || (data.getHighlighted().next() == null && lastClick
          .previous() == null))
          && (data.getHighlighted().getSequence() != lastClick.getSequence() || lastClick.getSequence() == null)) {
        join(lastClick, (MapillaryImportedImage) data.getHighlighted());
      } else if (lastClick.next() == data.getHighlighted() || lastClick.previous() == data.getHighlighted())
        unjoin(lastClick, (MapillaryImportedImage) data.getHighlighted());
      lastClick = null;
    }
    data.dataUpdated();
  }

  @Override
  public void mouseMoved(MouseEvent e) {
    lastPos = e;
    if (!(Main.map.mapView.getActiveLayer() instanceof MapillaryLayer))
      return;
    MapillaryAbstractImage closestTemp = getClosest(e.getPoint());
    data.setHighlightedImage(closestTemp);
    data.dataUpdated();
  }

  @Override
  public void paint(Graphics2D g, MapView mv, Bounds box) {
    if (lastClick != null) {
      g.setColor(Color.WHITE);
      Point p0 = mv.getPoint(lastClick.getLatLon());
      Point p1 = lastPos.getPoint();
      g.drawLine(p0.x, p0.y, p1.x, p1.y);
    }
  }

  private void join(MapillaryImportedImage img1, MapillaryImportedImage img2) {
    if (img1.next() != null) {
      MapillaryImportedImage temp = img1;
      img1 = img2;
      img2 = temp;
    }
    if (img1.getSequence() == null) {
      MapillarySequence seq = new MapillarySequence();
      seq.add(img1);
      img1.setSequence(seq);
    }
    if (img2.getSequence() == null) {
      MapillarySequence seq = new MapillarySequence();
      seq.add(img2);
      img2.setSequence(seq);
    }

    for (MapillaryAbstractImage img : img2.getSequence().getImages()) {
      img1.getSequence().add(img);
      img.setSequence(img1.getSequence());
    }
  }

  private void unjoin(MapillaryImportedImage img1, MapillaryImportedImage img2) {
    if (img1.next() != img2) {
      MapillaryImportedImage temp = img1;
      img1 = img2;
      img2 = temp;
    }

    ArrayList<MapillaryAbstractImage> firstHalf = new ArrayList<>(img1.getSequence().getImages()
        .subList(0, img1.getSequence().getImages().indexOf(img2)));
    ArrayList<MapillaryAbstractImage> secondHalf = new ArrayList<>(img1.getSequence().getImages()
        .subList(img1.getSequence().getImages().indexOf(img2), img1.getSequence().getImages().size()));

    MapillarySequence seq1 = new MapillarySequence();
    MapillarySequence seq2 = new MapillarySequence();

    for (MapillaryAbstractImage img : firstHalf) {
      img.setSequence(seq1);
      seq1.add(img);
    }
    for (MapillaryAbstractImage img : secondHalf) {
      img.setSequence(seq2);
      seq2.add(img);
    }
  }
  
  @Override
  public String toString() {
    return "Join mode";
  }
}
