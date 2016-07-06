// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.mode;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.event.InputEvent;
import java.awt.event.MouseEvent;
import java.util.concurrent.ConcurrentSkipListSet;

import javax.swing.SwingUtilities;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.data.osm.OsmPrimitive;
import org.openstreetmap.josm.gui.MapView;
import org.openstreetmap.josm.gui.layer.OsmDataLayer;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryData;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryMainDialog;
import org.openstreetmap.josm.plugins.mapillary.history.MapillaryRecord;
import org.openstreetmap.josm.plugins.mapillary.history.commands.CommandMove;
import org.openstreetmap.josm.plugins.mapillary.history.commands.CommandTurn;

/**
 * Handles the input event related with the layer. Mainly clicks.
 *
 * @author nokutu
 */
public class SelectMode extends AbstractMode {
  private Point start;
  private MapillaryAbstractImage closest;
  private MapillaryAbstractImage lastClicked;
  private final MapillaryRecord record;
  private boolean nothingHighlighted;
  private boolean imageHighlighted;

  /**
   * Main constructor.
   */
  public SelectMode() {
    this.record = MapillaryRecord.getInstance();
  }

  @Override
  public void mousePressed(MouseEvent e) {
    if (e.getButton() != MouseEvent.BUTTON1) {
      return;
    }
    MapillaryAbstractImage closest = getClosest(e.getPoint());
    if (!(Main.getLayerManager().getActiveLayer() instanceof MapillaryLayer)
            && closest != null && Main.map.mapMode == Main.map.mapModeSelect) {
      this.lastClicked = this.closest;
      this.data.setSelectedImage(closest);
      return;
    } else if (Main.getLayerManager().getActiveLayer() != MapillaryLayer.getInstance()) {
      return;
    }
    // Double click
    if (e.getClickCount() == 2 && this.data.getSelectedImage() != null && closest != null) {
      for (MapillaryAbstractImage img : closest.getSequence().getImages()) {
        this.data.addMultiSelectedImage(img);
      }
    }
    this.start = e.getPoint();
    this.lastClicked = this.closest;
    this.closest = closest;
    if (closest != null && this.data.getMultiSelectedImages().contains(closest)) {
      return;
    }
    // ctrl+click
    if (e.getModifiers() == (InputEvent.BUTTON1_MASK | InputEvent.CTRL_MASK) && closest != null) {
      this.data.addMultiSelectedImage(closest);
      // shift + click
    } else if (
        e.getModifiers() == (InputEvent.BUTTON1_MASK | InputEvent.SHIFT_MASK)
        && this.lastClicked instanceof MapillaryImage
    ) {
      if (this.closest != null && this.lastClicked != null
              && this.closest.getSequence() == (this.lastClicked).getSequence()) {
        int i = this.closest.getSequence().getImages().indexOf(this.closest);
        int j = this.lastClicked.getSequence().getImages().indexOf(this.lastClicked);
        this.data.addMultiSelectedImage(
            i < j
            ? new ConcurrentSkipListSet<>(this.closest.getSequence().getImages().subList(i, j + 1))
            : new ConcurrentSkipListSet<>(this.closest.getSequence().getImages().subList(j, i + 1))
        );
      }
      // click
    } else {
      this.data.setSelectedImage(closest);
    }
  }

  @Override
  public void mouseDragged(MouseEvent e) {
    MapillaryAbstractImage highlightImg = data.getHighlightedImage();
    if (
      Main.getLayerManager().getActiveLayer() == MapillaryLayer.getInstance()
      && SwingUtilities.isLeftMouseButton(e)
      && highlightImg != null && highlightImg.getLatLon() != null
    ) {
      Point highlightImgPoint = Main.map.mapView.getPoint(highlightImg.getTempLatLon());
      if (e.isShiftDown()) { // turn
        for (MapillaryAbstractImage img : data.getMultiSelectedImages()) {
          if (Main.pref.getBoolean("mapillary.developer") || !(img instanceof MapillaryImage)) {
            img.turn(Math.toDegrees(Math.atan2((e.getX() - highlightImgPoint.getX()), -(e.getY() - highlightImgPoint.getY()))) - highlightImg.getTempCa());
          }
        }
      } else { // move
        for (MapillaryAbstractImage img : this.data.getMultiSelectedImages()) {
          if (Main.pref.getBoolean("mapillary.developer") || !(img instanceof MapillaryImage)) {
            LatLon eventLatLon = Main.map.mapView.getLatLon(e.getX(), e.getY());
            LatLon imgLatLon = Main.map.mapView.getLatLon(highlightImgPoint.getX(), highlightImgPoint.getY());
            img.move(eventLatLon.getX() - imgLatLon.getX(), eventLatLon.getY() - imgLatLon.getY());
          }
        }
      }
      Main.map.repaint();
    }
  }

  @Override
  public void mouseReleased(MouseEvent e) {
    if (this.data.getSelectedImage() == null)
      return;
    if (this.data.getSelectedImage().getTempCa() != this.data.getSelectedImage().getMovingCa()) {
      double from = this.data.getSelectedImage().getTempCa();
      double to = this.data.getSelectedImage().getMovingCa();
      this.record.addCommand(new CommandTurn(this.data.getMultiSelectedImages(), to
              - from));
    } else if (this.data.getSelectedImage().getTempLatLon() != this.data
            .getSelectedImage().getMovingLatLon()) {
      LatLon from = this.data.getSelectedImage().getTempLatLon();
      LatLon to = this.data.getSelectedImage().getMovingLatLon();
      this.record.addCommand(new CommandMove(this.data.getMultiSelectedImages(), to
              .getX() - from.getX(), to.getY() - from.getY()));
    }
    for (MapillaryAbstractImage img : this.data.getMultiSelectedImages()) {
      if (img != null)
        img.stopMoving();
    }
  }

  /**
   * Checks if the mouse is over pictures.
   */
  @Override
  public void mouseMoved(MouseEvent e) {
    if (Main.getLayerManager().getActiveLayer() instanceof OsmDataLayer
            && Main.map.mapMode != Main.map.mapModeSelect) {
      return;
    }
    if (!Main.pref.getBoolean("mapillary.hover-enabled", true)) {
      return;
    }

    MapillaryAbstractImage closestTemp = getClosest(e.getPoint());

    if (closestTemp != null
            && Main.getLayerManager().getActiveLayer() instanceof OsmDataLayer
            && !this.imageHighlighted) {
      Main.map.mapMode.putValue("active", Boolean.FALSE);
      this.imageHighlighted = true;

    } else if (closestTemp == null
            && Main.getLayerManager().getActiveLayer() instanceof OsmDataLayer
            && this.imageHighlighted && this.nothingHighlighted) {
      this.nothingHighlighted = false;
      Main.map.mapMode.putValue("active", Boolean.TRUE);

    } else if (this.imageHighlighted && !this.nothingHighlighted
            && Main.getLayerManager().getEditLayer().data != null
            && Main.getLayerManager().getActiveLayer() instanceof OsmDataLayer) {

      for (OsmPrimitive primivitive : Main.getLayerManager().getEditLayer().data
              .allPrimitives()) {
        primivitive.setHighlighted(false);
      }
      this.imageHighlighted = false;
      this.nothingHighlighted = true;
    }

    if (this.data.getHighlightedImage() != closestTemp && closestTemp != null) {
      this.data.setHighlightedImage(closestTemp);
      MapillaryMainDialog.getInstance().setImage(closestTemp);
      MapillaryMainDialog.getInstance().updateImage(false);
    } else if (this.data.getHighlightedImage() != closestTemp && closestTemp == null) {
      this.data.setHighlightedImage(null);
      MapillaryMainDialog.getInstance().setImage(this.data.getSelectedImage());
      MapillaryMainDialog.getInstance().updateImage();
    }
    MapillaryData.dataUpdated();
  }

  @Override
  public void paint(Graphics2D g, MapView mv, Bounds box) {
  }

  @Override
  public String toString() {
    return tr("Select mode");
  }
}
