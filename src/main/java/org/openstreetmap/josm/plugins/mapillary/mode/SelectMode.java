// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.mode;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.Graphics2D;
import java.awt.event.InputEvent;
import java.awt.event.MouseEvent;
import java.util.concurrent.ConcurrentSkipListSet;

import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.data.osm.OsmPrimitive;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.MapView;
import org.openstreetmap.josm.gui.layer.OsmDataLayer;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryMainDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.tools.ImageProvider;

/**
 * Handles the input event related with the layer. Mainly clicks.
 *
 * @author nokutu
 */
public class SelectMode extends AbstractMode {
  private MapillaryAbstractImage closest;
  private MapillaryAbstractImage lastClicked;
  private boolean nothingHighlighted;
  private boolean imageHighlighted;

  /**
   * Main constructor.
   */
  public SelectMode() {
    super(tr("Mapillary Select Mode"), "mapillary-select", tr("Select images in the Mapillary Layer"),
      ImageProvider.getCursor("normal", null));
  }

  @Override
  public void mousePressed(MouseEvent e) {
    if (e.getButton() != MouseEvent.BUTTON1 || e.isConsumed()) {
      return;
    }
    final MapillaryAbstractImage closest = getClosest(e.getPoint());
    if (closest == null) {
      if (e.getClickCount() == MapillaryProperties.DESELECT_CLICK_COUNT.get() && MapillaryLayer.hasInstance()) {
        MapillaryLayer.getInstance().getData().setSelectedImage(null);
      }
      return;
    }

    if (MainApplication.getLayerManager().getActiveLayer() instanceof MapillaryLayer) {
      if (e.getClickCount() == 2) { // Double click
        if (MapillaryLayer.getInstance().getData().getSelectedImage() != null && closest.getSequence() != null) {
          MapillaryLayer.getInstance().getData().addMultiSelectedImage(closest.getSequence().getImages());
        }
      } else if (e.getModifiersEx() == (InputEvent.BUTTON1_DOWN_MASK | InputEvent.CTRL_DOWN_MASK)) { // ctrl + click
        MapillaryLayer.getInstance().getData().addMultiSelectedImage(closest);
      } else if (e.getModifiersEx() == (InputEvent.BUTTON1_DOWN_MASK | InputEvent.SHIFT_DOWN_MASK)) { // shift + click
        if (lastClicked != null && closest.getSequence() != null
          && closest.getSequence() == lastClicked.getSequence()) {
          final int i = closest.getSequence().getImages().indexOf(closest);
          final int j = lastClicked.getSequence().getImages().indexOf(lastClicked);
          MapillaryLayer.getInstance().getData().addMultiSelectedImage(
            new ConcurrentSkipListSet<>(closest.getSequence().getImages().subList(Math.min(i, j), Math.max(i, j) + 1)));
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
  }

  @Override
  public String toString() {
    return tr("Select mode");
  }
}
