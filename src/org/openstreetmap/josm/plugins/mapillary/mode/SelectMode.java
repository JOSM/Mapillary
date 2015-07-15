package org.openstreetmap.josm.plugins.mapillary.mode;

import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.event.MouseEvent;
import java.util.ArrayList;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.data.osm.OsmPrimitive;
import org.openstreetmap.josm.gui.MapView;
import org.openstreetmap.josm.gui.layer.OsmDataLayer;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryData;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImportedImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.commands.CommandMoveImage;
import org.openstreetmap.josm.plugins.mapillary.commands.CommandTurnImage;
import org.openstreetmap.josm.plugins.mapillary.commands.MapillaryRecord;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryMainDialog;

/**
 * Handles the input event related with the layer. Mainly clicks.
 *
 * @author nokutu
 *
 */
public class SelectMode extends AbstractMode {
  private Point start;
  private int lastButton;
  private MapillaryAbstractImage closest;
  private MapillaryAbstractImage lastClicked;
  private MapillaryRecord record;

  private boolean nothingHighlighted;
  private boolean imageHighlighted = false;

  public SelectMode() {
    data = MapillaryData.getInstance();
    record = MapillaryRecord.getInstance();
  }

  @Override
  public void mousePressed(MouseEvent e) {
    lastButton = e.getButton();
    if (e.getButton() != MouseEvent.BUTTON1)
      return;
    MapillaryAbstractImage closest = getClosest(e.getPoint());
    if (Main.map.mapView.getActiveLayer() instanceof OsmDataLayer
        && closest != null && Main.map.mapMode == Main.map.mapModeSelect) {
      this.lastClicked = this.closest;
      MapillaryData.getInstance().setSelectedImage(closest);
      return;
    } else if (Main.map.mapView.getActiveLayer() != MapillaryLayer
        .getInstance())
      return;
    // Double click
    if (e.getClickCount() == 2 && data.getSelectedImage() != null
        && closest != null) {
      for (MapillaryAbstractImage img : closest.getSequence().getImages()) {
        data.addMultiSelectedImage(img);
      }
    }
    this.start = e.getPoint();
    this.lastClicked = this.closest;
    this.closest = closest;
    if (data.getMultiSelectedImages().contains(closest))
      return;
    // ctrl+click
    if (e.getModifiers() == (MouseEvent.BUTTON1_MASK | MouseEvent.CTRL_MASK)
        && closest != null)
      data.addMultiSelectedImage(closest);
    // shift + click
    else if (e.getModifiers() == (MouseEvent.BUTTON1_MASK | MouseEvent.SHIFT_MASK)
        && this.lastClicked instanceof MapillaryImage) {
      if (this.closest != null && this.lastClicked != null
          && this.closest.getSequence() == (this.lastClicked).getSequence()) {
        int i = this.closest.getSequence().getImages().indexOf(this.closest);
        int j = this.lastClicked.getSequence().getImages()
            .indexOf(this.lastClicked);
        if (i < j)
          data.addMultiSelectedImage(new ArrayList<>(this.closest.getSequence()
              .getImages().subList(i, j + 1)));
        else
          data.addMultiSelectedImage(new ArrayList<>(this.closest.getSequence()
              .getImages().subList(j, i + 1)));
      }
      // click
    } else
      data.setSelectedImage(closest);
  }

  @Override
  public void mouseDragged(MouseEvent e) {
    if (Main.map.mapView.getActiveLayer() != MapillaryLayer.getInstance())
      return;

    if (!Main.pref.getBoolean("mapillary.developer"))
      for (MapillaryAbstractImage img : MapillaryData.getInstance()
          .getMultiSelectedImages()) {
        if (img instanceof MapillaryImage)
          return;
      }
    if (MapillaryData.getInstance().getSelectedImage() != null) {
      if (lastButton == MouseEvent.BUTTON1 && !e.isShiftDown()) {
        LatLon to = Main.map.mapView.getLatLon(e.getX(), e.getY());
        LatLon from = Main.map.mapView.getLatLon(start.getX(), start.getY());
        for (MapillaryAbstractImage img : MapillaryData.getInstance()
            .getMultiSelectedImages()) {

          img.move(to.getX() - from.getX(), to.getY() - from.getY());
        }
        Main.map.repaint();
      } else if (lastButton == MouseEvent.BUTTON1 && e.isShiftDown()) {
        this.closest.turn(Math.toDegrees(Math.atan2((e.getX() - start.x),
            -(e.getY() - start.y)))
            - closest.getTempCa());
        for (MapillaryAbstractImage img : MapillaryData.getInstance()
            .getMultiSelectedImages()) {
          img.turn(Math.toDegrees(Math.atan2((e.getX() - start.x),
              -(e.getY() - start.y))) - closest.getTempCa());
        }
        Main.map.repaint();
      }
    }
  }

  @Override
  public void mouseReleased(MouseEvent e) {
    if (data.getSelectedImage() == null)
      return;
    if (data.getSelectedImage().getTempCa() != data.getSelectedImage().getCa()) {
      double from = data.getSelectedImage().getTempCa();
      double to = data.getSelectedImage().getCa();
      record.addCommand(new CommandTurnImage(data.getMultiSelectedImages(), to
          - from));
    } else if (data.getSelectedImage().getTempLatLon() != data
        .getSelectedImage().getLatLon()) {
      LatLon from = data.getSelectedImage().getTempLatLon();
      LatLon to = data.getSelectedImage().getLatLon();
      record.addCommand(new CommandMoveImage(data.getMultiSelectedImages(), to
          .getX() - from.getX(), to.getY() - from.getY()));
    }
    for (MapillaryAbstractImage img : data.getMultiSelectedImages()) {
      if (img != null)
        img.stopMoving();
    }
  }

  /**
   * Checks if the mouse is over pictures.
   */
  @Override
  public void mouseMoved(MouseEvent e) {
    MapillaryAbstractImage closestTemp = getClosest(e.getPoint());
    if (Main.map.mapView.getActiveLayer() instanceof OsmDataLayer
        && Main.map.mapMode != Main.map.mapModeSelect)
      return;
    if (closestTemp != null
        && Main.map.mapView.getActiveLayer() instanceof OsmDataLayer
        && !imageHighlighted) {
      Main.map.mapMode.putValue("active", Boolean.FALSE);
      imageHighlighted = true;

    } else if (closestTemp == null
        && Main.map.mapView.getActiveLayer() instanceof OsmDataLayer
        && imageHighlighted && nothingHighlighted) {
      nothingHighlighted = false;
      Main.map.mapMode.putValue("active", Boolean.TRUE);

    } else if (imageHighlighted && !nothingHighlighted
        && Main.map.mapView != null
        && Main.map.mapView.getEditLayer().data != null
        && Main.map.mapView.getActiveLayer() instanceof OsmDataLayer) {

      for (OsmPrimitive primivitive : Main.map.mapView.getEditLayer().data
          .allPrimitives()) {
        primivitive.setHighlighted(false);
      }
      imageHighlighted = false;
      nothingHighlighted = true;
    }

    if (MapillaryData.getInstance().getHighlighted() != closestTemp
        && closestTemp != null) {
      MapillaryData.getInstance().setHighlightedImage(closestTemp);
      MapillaryMainDialog.getInstance().setImage(closestTemp);
      MapillaryMainDialog.getInstance().updateImage();
    } else if (MapillaryData.getInstance().getHighlighted() != closestTemp
        && closestTemp == null) {
      MapillaryData.getInstance().setHighlightedImage(null);
      MapillaryMainDialog.getInstance().setImage(
          MapillaryData.getInstance().getSelectedImage());
      MapillaryMainDialog.getInstance().updateImage();
    }
    MapillaryData.getInstance().dataUpdated();
  }

  @Override
  public void paint(Graphics2D g, MapView mv, Bounds box) {
  }

  @Override
  public String toString() {
    return "Select mode";
  }
}
