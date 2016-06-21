// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.mode;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.Color;
import java.awt.Cursor;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.event.MouseEvent;
import java.util.Arrays;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.gui.MapView;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryData;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImportedImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.history.MapillaryRecord;
import org.openstreetmap.josm.plugins.mapillary.history.commands.CommandJoin;
import org.openstreetmap.josm.plugins.mapillary.history.commands.CommandUnjoin;

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
    if (this.lastClick == null && this.data.getHighlightedImage() instanceof MapillaryImportedImage) {
      this.lastClick = (MapillaryImportedImage) this.data.getHighlightedImage();
    } else if (this.lastClick != null
        && this.data.getHighlightedImage() instanceof MapillaryImportedImage) {
      if (
        (this.data.getHighlightedImage().previous() == null && this.lastClick.next() == null
          || this.data.getHighlightedImage().next() == null && this.lastClick.previous() == null)
        && (this.data.getHighlightedImage().getSequence() != this.lastClick.getSequence() || this.lastClick.getSequence() == null)
      ) {

        MapillaryRecord.getInstance().addCommand(
            new CommandJoin(Arrays.asList(new MapillaryAbstractImage[] {
                this.lastClick, this.data.getHighlightedImage() })));
      } else if (this.lastClick.next() == this.data.getHighlightedImage()
          || this.lastClick.previous() == this.data.getHighlightedImage()) {
        MapillaryRecord.getInstance().addCommand(
            new CommandUnjoin(Arrays.asList(new MapillaryAbstractImage[] {
                this.lastClick, this.data.getHighlightedImage() })));
      }
      this.lastClick = null;
    }
    MapillaryData.dataUpdated();
  }

  @Override
  public void mouseMoved(MouseEvent e) {
    this.lastPos = e;
    if (!(Main.getLayerManager().getActiveLayer() instanceof MapillaryLayer))
      return;
    MapillaryAbstractImage closestTemp = getClosest(e.getPoint());
    this.data.setHighlightedImage(closestTemp);
    MapillaryData.dataUpdated();
  }

  @Override
  public void paint(Graphics2D g, MapView mv, Bounds box) {
    if (this.lastClick != null) {
      g.setColor(Color.WHITE);
      Point p0 = mv.getPoint(this.lastClick.getMovingLatLon());
      Point p1 = this.lastPos.getPoint();
      g.drawLine(p0.x, p0.y, p1.x, p1.y);
    }
  }

  @Override
  public String toString() {
    return tr("Join mode");
  }
}
