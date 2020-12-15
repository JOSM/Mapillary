// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.mode;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.event.MouseEvent;
import java.util.Objects;

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
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryMainDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.dialog.MapillaryChangesetDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.history.MapillaryRecord;
import org.openstreetmap.josm.plugins.mapillary.history.commands.CommandMove;
import org.openstreetmap.josm.plugins.mapillary.history.commands.CommandTurn;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.spi.preferences.Config;
import org.openstreetmap.josm.tools.ImageProvider;

/**
 * In this mode user can edit images, changeset of which can be submitted to Mapillary.
 *
 * @author Kishan
 */
public class EditMode extends AbstractMode {

  private final MapillaryRecord record;
  private boolean imageHighlighted;
  private boolean nothingHighlighted;

  /**
   * Main Constructor
   */
  public EditMode() {
    super(tr("Mapillary Edit Mode"), "mapillary-edit", tr("Edit images in Mapillary Layer"),
      ImageProvider.getCursor("normal", null));
    this.record = MapillaryRecord.getInstance();
  }

  @Override
  public void enterMode() {
    super.enterMode();
    if (MapillaryLayer.hasInstance()) {
      MapillaryLayer.getInstance().getData().setSelectedImage(null);
    }
  }

  @Override
  public void exitMode() {
    super.exitMode();
    if (MapillaryLayer.hasInstance()) {
      MapillaryLayer.getInstance().getData().setSelectedImage(null);
    }
  }

  @Override
  protected MapillaryAbstractImage getClosest(Point clickPoint) {
    double snapDistance = 10;
    double minDistance = Double.MAX_VALUE;
    MapillaryAbstractImage closest = null;
    for (MapillaryAbstractImage img : MapillaryLayer.getInstance().getData().getImages()) {
      Point imagePoint = MainApplication.getMap().mapView.getPoint(
        (img instanceof MapillaryImage && ((MapillaryImage) img).toDelete()) ? img.getLatLon() : img.getMovingLatLon());
      imagePoint.setLocation(imagePoint.getX(), imagePoint.getY());
      double dist = clickPoint.distanceSq(imagePoint);
      if (minDistance > dist && clickPoint.distance(imagePoint) < snapDistance && img.isVisible()) {
        minDistance = dist;
        closest = img;
      }
    }
    return closest;
  }

  @Override
  public void mousePressed(MouseEvent e) {
    if (e.getButton() != MouseEvent.BUTTON1) {
      return;
    }
    final MapillaryAbstractImage closest = getClosest(e.getPoint());
    if (closest == null) {
      if (e.getClickCount() == Config.getPref().getInt("mapillary.image.deselect.click.count", 3)
        && MapillaryLayer.hasInstance()) { // Triple click
        MapillaryLayer.getInstance().getData().setSelectedImage(null);
      }
      return;
    }

    if (MainApplication.getLayerManager().getActiveLayer() instanceof MapillaryLayer) {
      if (e.getClickCount() == 2) { // Double click
        if (MapillaryLayer.getInstance().getData().getSelectedImage() != null
          && (closest instanceof MapillaryImage ? !((MapillaryImage) closest).toDelete() : true)
          && closest.getSequence() != null) {
          MapillaryLayer.getInstance().getData().addMultiSelectedImage(closest.getSequence().getImages());
        }
      } else { // click
        MapillaryLayer.getInstance().getData().setSelectedImage(closest);
      }
    } else { // If the MapillaryLayer is NOT selected
      if (MainApplication.getMap().mapMode == MainApplication.getMap().mapModeSelect) {
        MapillaryLayer.getInstance().getData().setSelectedImage(closest);
      }
    }
  }

  @Override
  public void mouseDragged(MouseEvent e) {
    MapillaryAbstractImage highlightImg = MapillaryLayer.getInstance().getData().getHighlightedImage();
    if (MainApplication.getLayerManager().getActiveLayer() == MapillaryLayer.getInstance()
      && SwingUtilities.isLeftMouseButton(e) && highlightImg != null && highlightImg.getLatLon() != null) {
      Point highlightImgPoint = MainApplication.getMap().mapView.getPoint(highlightImg.getTempLatLon());
      if (e.isShiftDown()) { // turn
        MapillaryLayer.getInstance().getData().getMultiSelectedImages().parallelStream()
          .forEach(img -> img
            .turn(Math.toDegrees(Math.atan2(e.getX() - highlightImgPoint.getX(), -e.getY() + highlightImgPoint.getY()))
              - highlightImg.getTempCa()));
      } else { // move
        LatLon eventLatLon = MainApplication.getMap().mapView.getLatLon(e.getX(), e.getY());
        LatLon imgLatLon = MainApplication.getMap().mapView.getLatLon(highlightImgPoint.getX(),
          highlightImgPoint.getY());
        MapillaryLayer.getInstance().getData().getMultiSelectedImages().parallelStream()
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
      record
        .addCommand(new CommandMove(data.getMultiSelectedImages(), to.getX() - from.getX(), to.getY() - from.getY()));
    }
    data.getMultiSelectedImages().parallelStream().filter(Objects::nonNull).forEach(MapillaryAbstractImage::stopMoving);
    MapillaryChangesetDialog.getInstance().selectedImageChanged(data.getSelectedImage(), data.getSelectedImage());
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

    MapillaryAbstractImage highlightedImage = MapillaryLayer.getInstance().getData().getHighlightedImage();
    if (closestTemp != null && !closestTemp.equals(highlightedImage)) {
      MapillaryLayer.getInstance().getData().setHighlightedImage(closestTemp);
      MapillaryMainDialog.getInstance().setImage(closestTemp);
      MapillaryMainDialog.getInstance().updateImage(false);
    } else if (closestTemp == null && highlightedImage != null) {
      MapillaryLayer.getInstance().getData().setHighlightedImage(null);
      MapillaryMainDialog.getInstance().setImage(MapillaryLayer.getInstance().getData().getSelectedImage());
      MapillaryMainDialog.getInstance().updateImage();
    }
    MapillaryLayer.invalidateInstance();
  }

  @Override
  public void paint(Graphics2D g, MapView mv, Bounds box) {
    // No extra graphics for this mode.
  }

  @Override
  public String toString() {
    return tr("Edit mode");
  }
}
