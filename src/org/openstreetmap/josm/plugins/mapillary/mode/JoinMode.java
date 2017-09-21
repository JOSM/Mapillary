// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.mode;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.Color;
import java.awt.Cursor;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.event.MouseEvent;
import java.util.Arrays;

import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.MapView;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
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
    final MapillaryAbstractImage highlighted = MapillaryLayer.getInstance().getData().getHighlightedImage();
    if (highlighted == null) {
      return;
    }
    if (this.lastClick == null && highlighted instanceof MapillaryImportedImage) {
      this.lastClick = (MapillaryImportedImage) highlighted;
    } else if (this.lastClick != null
        && highlighted instanceof MapillaryImportedImage) {
      if (
        (
          (highlighted.previous() == null && this.lastClick.next() == null) ||
          (highlighted.next() == null && this.lastClick.previous() == null)
        )
        && highlighted.getSequence() != this.lastClick.getSequence()
      ) {
        MapillaryRecord.getInstance().addCommand(new CommandJoin(this.lastClick, highlighted));
      } else if (this.lastClick.next() == highlighted || this.lastClick.previous() == highlighted) {
        MapillaryRecord.getInstance().addCommand(
          new CommandUnjoin(Arrays.asList(this.lastClick, highlighted))
        );
      }
      this.lastClick = null;
    }
    MapillaryLayer.invalidateInstance();
  }

  @Override
  public void mouseMoved(MouseEvent e) {
    this.lastPos = e;
    if (!(MainApplication.getLayerManager().getActiveLayer() instanceof MapillaryLayer))
      return;
    MapillaryAbstractImage closestTemp = getClosest(e.getPoint());
    MapillaryLayer.getInstance().getData().setHighlightedImage(closestTemp);
    MapillaryLayer.invalidateInstance();
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
