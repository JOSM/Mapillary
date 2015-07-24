package org.openstreetmap.josm.plugins.mapillary.mode;

import java.awt.Cursor;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.event.MouseAdapter;
import java.util.Calendar;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.gui.MapView;
import org.openstreetmap.josm.gui.NavigatableComponent.ZoomChangeListener;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryData;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.downloads.MapillaryDownloader;

/**
 * Superclass for all the mode of the {@link MapillaryLayer}.
 *
 * @author nokutu
 * @see MapillaryLayer
 *
 */
public abstract class AbstractMode extends MouseAdapter implements
    ZoomChangeListener {

  protected MapillaryData data = MapillaryData.getInstance();
  private static final SemiautomaticThread semiautomaticThread = new SemiautomaticThread();

  /**
   * Cursor that should become active when this mode is activated.
   */
  public int cursor = Cursor.DEFAULT_CURSOR;

  protected MapillaryAbstractImage getClosest(Point clickPoint) {
    double snapDistance = 10;
    double minDistance = Double.MAX_VALUE;
    MapillaryAbstractImage closest = null;
    for (MapillaryAbstractImage image : data.getImages()) {
      Point imagePoint = Main.map.mapView.getPoint(image.getLatLon());
      imagePoint.setLocation(imagePoint.getX(), imagePoint.getY());
      double dist = clickPoint.distanceSq(imagePoint);
      if (minDistance > dist && clickPoint.distance(imagePoint) < snapDistance
          && image.isVisible()) {
        minDistance = dist;
        closest = image;
      }
    }
    return closest;
  }

  /**
   * Paints whatever the mode needs.
   *
   * @param g
   * @param mv
   * @param box
   */
  public abstract void paint(Graphics2D g, MapView mv, Bounds box);

  @Override
  public void zoomChanged() {
    if (Main.pref.get("mapillary.download-mode").equals(
        MapillaryDownloader.MODES[1])
        || MapillaryLayer.getInstance().TEMP_SEMIAUTOMATIC) {
      if (!semiautomaticThread.isAlive())
        semiautomaticThread.start();
      semiautomaticThread.moved();
    }
  }

  private static class SemiautomaticThread extends Thread {

    /** If in semiautomatic mode, the last Epoch time when there was a download */
    private long lastDownload;

    private boolean moved = false;

    @Override
    public void run() {
      while (true) {
        if (moved
            && Calendar.getInstance().getTimeInMillis() - lastDownload >= 2000) {
          lastDownload = Calendar.getInstance().getTimeInMillis();
          MapillaryDownloader.completeView();
          moved = false;
          MapillaryData.getInstance().dataUpdated();
        }
        synchronized (this) {
          try {
            wait(100);
          } catch (InterruptedException e) {
            Main.error(e);
          }
        }
      }
    }

    public void moved() {
      moved = true;
    }
  }
}