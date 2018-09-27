// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.mode;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.event.InputEvent;
import java.awt.event.MouseEvent;
import java.util.Objects;
import java.util.concurrent.ConcurrentSkipListSet;

import javax.swing.SwingUtilities;

import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.data.osm.OsmPrimitive;
import org.openstreetmap.josm.gui.MainApplication;
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
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;

/**
 * Handles the input event related with the layer. Mainly clicks.
 *
 * @author nokutu
 */
public class SelectMode extends AbstractMode {
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
    final MapillaryAbstractImage closest = getClosest(e.getPoint());
    if (closest == null) {
      return;
    }

    if (MainApplication.getLayerManager().getActiveLayer() instanceof MapillaryLayer) {
      if (e.getClickCount() == 2) { // Double click
        if (e.getButton() == MouseEvent.BUTTON1 && MapillaryLayer.getInstance().getData().getSelectedImage() != null) {
          MapillaryLayer.getInstance().getData().addMultiSelectedImage(closest.getSequence().getImages());
        }
      } else if (e.getModifiersEx() == (InputEvent.BUTTON1_DOWN_MASK | InputEvent.CTRL_DOWN_MASK)) { // ctrl + click
        MapillaryLayer.getInstance().getData().addMultiSelectedImage(closest);
      } else if (e.getModifiersEx() == (InputEvent.BUTTON1_DOWN_MASK | InputEvent.SHIFT_DOWN_MASK)) { // shift + click
        if (lastClicked != null && closest.getSequence() == lastClicked.getSequence()) {
          final int i = closest.getSequence().getImages().indexOf(closest);
          final int j = lastClicked.getSequence().getImages().indexOf(lastClicked);
          MapillaryLayer.getInstance().getData().addMultiSelectedImage(
            new ConcurrentSkipListSet<>(closest.getSequence().getImages().subList(
              i < j ? i : j,
              i < j ? j + 1 : i + 1
            ))
          );
        }
      } else { // click
        MapillaryLayer.getInstance().getData().setSelectedImage(closest);
      }
    } else { // If the MapillaryLayer is NOT selected
      if (MainApplication.getMap().mapMode == MainApplication.getMap().mapModeSelect) {
        MapillaryLayer.getInstance().getData().setSelectedImage(closest);
      }
    }
    this.lastClicked = this.closest;
    this.closest = closest;
  }

  @Override
  public void mouseDragged(MouseEvent e) {
    MapillaryAbstractImage highlightImg = MapillaryLayer.getInstance().getData().getHighlightedImage();
    if (
            MainApplication.getLayerManager().getActiveLayer() == MapillaryLayer.getInstance()
                && SwingUtilities.isLeftMouseButton(e)
                && highlightImg != null && highlightImg.getLatLon() != null
            ) {
      Point highlightImgPoint = MainApplication.getMap().mapView.getPoint(highlightImg.getTempLatLon());
      if (e.isShiftDown()) { // turn
        MapillaryLayer.getInstance().getData().getMultiSelectedImages().parallelStream().filter(img -> !(img instanceof MapillaryImage) || MapillaryProperties.DEVELOPER.get())
                .forEach(img -> img.turn(Math.toDegrees(Math.atan2(e.getX() - highlightImgPoint.getX(), -e.getY() + highlightImgPoint.getY())) - highlightImg.getTempCa()));
      } else { // move
        LatLon eventLatLon = MainApplication.getMap().mapView.getLatLon(e.getX(), e.getY());
        LatLon imgLatLon = MainApplication.getMap().mapView.getLatLon(highlightImgPoint.getX(), highlightImgPoint.getY());
        MapillaryLayer.getInstance().getData().getMultiSelectedImages().parallelStream().filter(img -> !(img instanceof MapillaryImage) || MapillaryProperties.DEVELOPER.get())
                .forEach(img -> img.move(eventLatLon.getX() - imgLatLon.getX(), eventLatLon.getY() - imgLatLon.getY()));
      }
      MapillaryLayer.invalidateInstance();
    }
  }

  @Override
  public void mouseReleased(MouseEvent e) {
    final MapillaryData data = MapillaryLayer.getInstance().getData();
    if (data.getSelectedImage() == null) {
      return;
    }
    if (!Objects.equals(data.getSelectedImage().getTempCa(), data.getSelectedImage().getMovingCa())) {
      double from = data.getSelectedImage().getTempCa();
      double to = data.getSelectedImage().getMovingCa();
      record.addCommand(new CommandTurn(data.getMultiSelectedImages(), to - from));
    } else if (!Objects.equals(data.getSelectedImage().getTempLatLon(), data.getSelectedImage().getMovingLatLon())) {
      LatLon from = data.getSelectedImage().getTempLatLon();
      LatLon to = data.getSelectedImage().getMovingLatLon();
      record.addCommand(new CommandMove(data.getMultiSelectedImages(), to.getX() - from.getX(), to.getY() - from.getY()));
    }
    data.getMultiSelectedImages().parallelStream().filter(Objects::nonNull).forEach(MapillaryAbstractImage::stopMoving);
    MapillaryLayer.invalidateInstance();
  }

  /**
   * Checks if the mouse is over pictures.
   */
  @Override
  public void mouseMoved(MouseEvent e) {
    if (MainApplication.getLayerManager().getActiveLayer() instanceof OsmDataLayer
            && MainApplication.getMap().mapMode != MainApplication.getMap().mapModeSelect) {
      return;
    }
    if (!MapillaryProperties.HOVER_ENABLED.get()) {
      return;
    }

    MapillaryAbstractImage closestTemp = getClosest(e.getPoint());

    final OsmDataLayer editLayer = MainApplication.getLayerManager().getEditLayer();
    if (editLayer != null) {
      if (closestTemp != null && !this.imageHighlighted) {
        if (MainApplication.getMap().mapMode != null) {
          MainApplication.getMap().mapMode.putValue("active", Boolean.FALSE);
        }
        imageHighlighted = true;
      } else if (closestTemp == null && imageHighlighted && nothingHighlighted) {
        if (MainApplication.getMap().mapMode != null) {
          MainApplication.getMap().mapMode.putValue("active", Boolean.TRUE);
        }
        nothingHighlighted = false;
      } else if (imageHighlighted && !nothingHighlighted && editLayer.data != null) {
        for (OsmPrimitive primivitive : MainApplication.getLayerManager().getEditLayer().data.allPrimitives()) {
          primivitive.setHighlighted(false);
        }
        imageHighlighted = false;
        nothingHighlighted = true;
      }
    }

    if (MapillaryLayer.getInstance().getData().getHighlightedImage() != closestTemp && closestTemp != null) {
      MapillaryLayer.getInstance().getData().setHighlightedImage(closestTemp);
      MapillaryMainDialog.getInstance().setImage(closestTemp);
      MapillaryMainDialog.getInstance().updateImage(false);
    } else if (MapillaryLayer.getInstance().getData().getHighlightedImage() != closestTemp && closestTemp == null) {
      MapillaryLayer.getInstance().getData().setHighlightedImage(null);
      MapillaryMainDialog.getInstance().setImage(MapillaryLayer.getInstance().getData().getSelectedImage());
      MapillaryMainDialog.getInstance().updateImage();
    }
    MapillaryLayer.invalidateInstance();
  }

  @Override
  public void paint(Graphics2D g, MapView mv, Bounds box) {
  }

  @Override
  public String toString() {
    return tr("Select mode");
  }
}
