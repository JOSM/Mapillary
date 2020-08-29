// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.imageviewer;

import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import javax.swing.SwingUtilities;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;

/**
 * MouseListener for zooming and dragging images.
 * @author Kishan
 */
public class ZoomPanMouseListener implements MouseListener, MouseWheelListener, MouseMotionListener {

  protected boolean isZooming = false;
  protected boolean isPanning = false;
  private final AbstractImageViewer imageViewer;
  protected boolean inWindow = false;

  public ZoomPanMouseListener(AbstractImageViewer imageViewer) {
    this.imageViewer = imageViewer;
  }

  public void reset() {
    isZooming = false;
    isPanning = false;
  }

  public void stopPanning() {
    isPanning = false;
  }

  @Override
  public void mouseClicked(MouseEvent e) {
    if (e.getButton() != MapillaryProperties.PICTURE_OPTION_BUTTON.get()) {
      return;
    }
    if (e.getClickCount() == 2) {
      imageViewer.zoomBestFitOrOne();
    }
  }

  @Override
  public void mousePressed(MouseEvent e) {
    if (!isPanning &&
      (e.getButton() == MapillaryProperties.PICTURE_DRAG_BUTTON.get())) {
      isPanning = true;
      imageViewer.startPanning(e.getPoint());
    }
  }

  @Override
  public void mouseReleased(MouseEvent e) {
    if (isPanning &&
      (e.getButton() == MapillaryProperties.PICTURE_DRAG_BUTTON.get())) {
      stopPanning();
    }
  }

  @Override
  public void mouseEntered(MouseEvent e) {
    inWindow = true;
  }

  @Override
  public void mouseExited(MouseEvent e) {
    inWindow = false;
  }

  @Override
  public void mouseWheelMoved(MouseWheelEvent e) {
    if (isPanning) {
      return;
    }
    if (e.getWheelRotation() < 0) {
      imageViewer.zoom(e.getX(), e.getY(), true);
    } else {
      imageViewer.zoom(e.getX(), e.getY(), false);
    }
  }

  @Override
  public void mouseDragged(MouseEvent e) {
    if (SwingUtilities.isRightMouseButton(e)) {
      if (isPanning) {
        imageViewer.pan(e.getPoint());
      }
    }
  }

  @Override
  public void mouseMoved(MouseEvent e) {
    //Do nothing.
  }
}
