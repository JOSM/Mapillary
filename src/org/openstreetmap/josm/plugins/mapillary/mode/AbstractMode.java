// License: GPL. For details, see LICENSE file.
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
import org.openstreetmap.josm.plugins.mapillary.io.download.MapillaryDownloader;

/**
 * Superclass for all the mode of the {@link MapillaryLayer}.
 *
 * @author nokutu
 * @see MapillaryLayer
 *
 */
public abstract class AbstractMode extends MouseAdapter implements
    ZoomChangeListener {

  private static final int DOWNLOAD_COOLDOWN = 2000;

  protected MapillaryData data = MapillaryLayer.getInstance().getData();
  private static SemiautomaticThread semiautomaticThread = new SemiautomaticThread();

  /**
   * Cursor that should become active when this mode is activated.
   */
  public int cursor = Cursor.DEFAULT_CURSOR;

  protected MapillaryAbstractImage getClosest(Point clickPoint) {
    double snapDistance = 10;
    double minDistance = Double.MAX_VALUE;
    MapillaryAbstractImage closest = null;
    for (MapillaryAbstractImage image : this.data.getImages()) {
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
   * Paint the dataset using the engine set.
   *
   * @param g
   * @param mv
   *          The object that can translate GeoPoints to screen coordinates.
   * @param box
   */
  public abstract void paint(Graphics2D g, MapView mv, Bounds box);

  @Override
  public void zoomChanged() {
    if (MapillaryDownloader.getMode() == MapillaryDownloader.MODES.Semiautomatic) {
      if (!semiautomaticThread.isAlive())
        semiautomaticThread.start();
      semiautomaticThread.moved();
    }
  }

  /**
   * Resets the semiautomatic mode thread.
   */
  public static void resetThread() {
    semiautomaticThread.interrupt();
    semiautomaticThread = new SemiautomaticThread();
  }

  private static class SemiautomaticThread extends Thread {

    /** If in semiautomatic mode, the last Epoch time when there was a download */
    private long lastDownload;

    private boolean moved = false;

    @Override
    public void run() {
      while (true) {
        if (this.moved
            && Calendar.getInstance().getTimeInMillis() - this.lastDownload >= DOWNLOAD_COOLDOWN) {
          this.lastDownload = Calendar.getInstance().getTimeInMillis();
          MapillaryDownloader.completeView();
          this.moved = false;
          MapillaryData.dataUpdated();
        }
        synchronized (this) {
          try {
            wait(100);
          } catch (InterruptedException e) {
          }
        }
      }
    }

    public void moved() {
      this.moved = true;
    }
  }
}