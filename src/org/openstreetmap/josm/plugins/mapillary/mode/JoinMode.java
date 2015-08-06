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
import org.openstreetmap.josm.plugins.mapillary.MapillaryData;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImportedImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.MapillarySequence;

/**
 * In this mode the user can join pictures to make sequences or unjoin them.
 *
 * @author nokutu
 *
 */
public class JoinMode extends AbstractMode {

  private MapillaryImportedImage lastClick;
  private MouseEvent lastPos;

  /**
   * Main constructor.
   */
  public JoinMode() {
    this.cursor = Cursor.CROSSHAIR_CURSOR;
  }

  @Override
  public void mousePressed(MouseEvent e) {
    if (this.data.getHighlightedImage() == null)
      return;
    if (this.lastClick == null
        && this.data.getHighlightedImage() instanceof MapillaryImportedImage) {
      this.lastClick = (MapillaryImportedImage) this.data.getHighlightedImage();
    } else if (this.lastClick != null
        && this.data.getHighlightedImage() instanceof MapillaryImportedImage) {
      if (((this.data.getHighlightedImage().previous() == null && this.lastClick.next() == null) || (this.data
          .getHighlightedImage().next() == null && this.lastClick.previous() == null))
          && (this.data.getHighlightedImage().getSequence() != this.lastClick.getSequence() || this.lastClick
              .getSequence() == null)) {
        join(this.lastClick, (MapillaryImportedImage) this.data.getHighlightedImage());
      } else if (this.lastClick.next() == this.data.getHighlightedImage()
          || this.lastClick.previous() == this.data.getHighlightedImage())
        unjoin(this.lastClick, (MapillaryImportedImage) this.data.getHighlightedImage());
      this.lastClick = null;
    }
    MapillaryData.dataUpdated();
  }

  @Override
  public void mouseMoved(MouseEvent e) {
    this.lastPos = e;
    if (!(Main.map.mapView.getActiveLayer() instanceof MapillaryLayer))
      return;
    MapillaryAbstractImage closestTemp = getClosest(e.getPoint());
    this.data.setHighlightedImage(closestTemp);
    MapillaryData.dataUpdated();
  }

  @Override
  public void paint(Graphics2D g, MapView mv, Bounds box) {
    if (this.lastClick != null) {
      g.setColor(Color.WHITE);
      Point p0 = mv.getPoint(this.lastClick.getLatLon());
      Point p1 = this.lastPos.getPoint();
      g.drawLine(p0.x, p0.y, p1.x, p1.y);
    }
  }

  private static void join(MapillaryImportedImage img1, MapillaryImportedImage img2) {
    MapillaryImportedImage firstImage = img1;
    MapillaryImportedImage secondImage = img2;

    if (img1.next() != null) {
      firstImage = img2;
      secondImage = img1;
    }
    if (firstImage.getSequence() == null) {
      MapillarySequence seq = new MapillarySequence();
      seq.add(firstImage);
      firstImage.setSequence(seq);
    }
    if (secondImage.getSequence() == null) {
      MapillarySequence seq = new MapillarySequence();
      seq.add(secondImage);
      img2.setSequence(seq);
    }

    for (MapillaryAbstractImage img : secondImage.getSequence().getImages()) {
      firstImage.getSequence().add(img);
      img.setSequence(firstImage.getSequence());
    }
  }

  private static void unjoin(MapillaryImportedImage img1, MapillaryImportedImage img2) {
    MapillaryImportedImage firstImage = img1;
    MapillaryImportedImage secondImage = img2;

    if (img1.next() != img2) {
      firstImage = img2;
      secondImage = img1;
    }

    ArrayList<MapillaryAbstractImage> firstHalf = new ArrayList<>(firstImage
        .getSequence().getImages()
        .subList(0, firstImage.getSequence().getImages().indexOf(secondImage)));
    ArrayList<MapillaryAbstractImage> secondHalf = new ArrayList<>(firstImage
        .getSequence()
        .getImages()
        .subList(firstImage.getSequence().getImages().indexOf(secondImage),
            firstImage.getSequence().getImages().size()));

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
