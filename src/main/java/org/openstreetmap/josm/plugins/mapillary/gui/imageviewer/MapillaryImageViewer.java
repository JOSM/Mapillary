// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.imageviewer;

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.image.BufferedImage;
import org.openstreetmap.josm.plugins.mapillary.utils.ImageViewUtil;

public class MapillaryImageViewer extends AbstractImageViewer {

  private Point mousePointInImg;

  public MapillaryImageViewer() {
    super();
    ZoomPanMouseListener zoomPanMouseListener = new ZoomPanMouseListener(this);
    this.addMouseListener(zoomPanMouseListener);
    this.addMouseWheelListener(zoomPanMouseListener);
    this.addMouseMotionListener(zoomPanMouseListener);
  }

  @Override
  public Rectangle getDefaultVisibleRect() {
    if (image != null) {
      Rectangle visibleRectangle = new Rectangle(0, 0, image.getWidth(), image.getHeight());
      return visibleRectangle;
    }
    return visibleRect;
  }

  @Override
  void paintImage(Graphics g, BufferedImage image, Rectangle visibleRect) {
    Rectangle target = ImageViewUtil.calculateDrawImageRectangle(visibleRect,
      new Rectangle(0, 0, getSize().width, getSize().height));
    g.drawImage(image, target.x, target.y, target.x + target.width,
      target.y + target.height, visibleRect.x, visibleRect.y,
      visibleRect.x + visibleRect.width, visibleRect.y + visibleRect.height, this);
  }

  @Override
  public void zoomOut(int zoomCenterX, int zoomCenterY) {
    Image mouseImage;
    Rectangle mouseVisibleRect;
    synchronized (this) {
      mouseImage = getImage();
      mouseVisibleRect = this.visibleRect;
    }
    if (mouseImage == null) {
      return;
    }
    mousePointInImg = comp2imgCoord(mouseVisibleRect, zoomCenterX, zoomCenterY);
    mouseVisibleRect.width = mouseVisibleRect.width * 3 / 2;
    mouseVisibleRect.height = mouseVisibleRect.height * 3 / 2;
    checkZoom(mouseVisibleRect);
    checkAspectRatio(mouseVisibleRect);
    checkVisibleRectSize(mouseImage, mouseVisibleRect);
    Rectangle drawRect = calculateDrawImageRectangle(mouseVisibleRect);
    mouseVisibleRect.x = mousePointInImg.x
      + ((drawRect.x - zoomCenterX) * mouseVisibleRect.width) / drawRect.width;
    mouseVisibleRect.y = mousePointInImg.y
      + ((drawRect.y - zoomCenterY) * mouseVisibleRect.height) / drawRect.height;
    ImageViewUtil.checkVisibleRectPos(mouseImage, mouseVisibleRect);
    synchronized (this) {
      visibleRect = mouseVisibleRect;
    }
    repaint();
  }

  @Override
  public void zoomIn(int zoomCenterX, int zoomCenterY) {
    Image mouseImage;
    Rectangle mouseVisibleRect;
    synchronized (this) {
      mouseImage = getImage();
      mouseVisibleRect = this.visibleRect;
    }
    if (mouseImage == null) {
      return;
    }
    mousePointInImg = comp2imgCoord(mouseVisibleRect, zoomCenterX, zoomCenterY);
    mouseVisibleRect.width = mouseVisibleRect.width * 2 / 3;
    mouseVisibleRect.height = mouseVisibleRect.height * 2 / 3;
    checkZoom(mouseVisibleRect);
    checkAspectRatio(mouseVisibleRect);
    checkVisibleRectSize(mouseImage, mouseVisibleRect);
    Rectangle drawRect = calculateDrawImageRectangle(mouseVisibleRect);
    mouseVisibleRect.x = mousePointInImg.x
      + ((drawRect.x - zoomCenterX) * mouseVisibleRect.width) / drawRect.width;
    mouseVisibleRect.y = mousePointInImg.y
      + ((drawRect.y - zoomCenterY) * mouseVisibleRect.height) / drawRect.height;
    ImageViewUtil.checkVisibleRectPos(mouseImage, mouseVisibleRect);
    synchronized (this) {
      visibleRect = mouseVisibleRect;
    }
    repaint();
  }

  @Override
  public void startPanning(Point p) {
    if (getImage() == null) {
      return;
    }
    Rectangle mouseVisibleRect;
    synchronized (this) {
      mouseVisibleRect = visibleRect;
    }
    mousePointInImg = comp2imgCoord(mouseVisibleRect, p.x, p.y);
  }

  @Override
  public void pan(Point p) {
    Image mouseImage;
    Rectangle mouseVisibleRect;
    synchronized (this) {
      mouseImage = getImage();
      mouseVisibleRect = visibleRect;
    }
    if (mouseImage == null) {
      return;
    }
    Point newImagePoint = comp2imgCoord(mouseVisibleRect, p.x, p.y);
    mouseVisibleRect.x += this.mousePointInImg.x - newImagePoint.x;
    mouseVisibleRect.y += this.mousePointInImg.y - newImagePoint.y;
    ImageViewUtil.checkVisibleRectPos(mouseImage, mouseVisibleRect);
    synchronized (this) {
      visibleRect = mouseVisibleRect;
    }
    repaint();
  }

  @Override
  public void stopPanning() {
    //Do Nothing.
  }

  /**
   * Check that the Zoom is not greater than 3:1
   *
   * @param zoomedRectangle
   */
  protected void checkZoom(Rectangle zoomedRectangle) {
    if (zoomedRectangle.width < getSize().width / 3) {
      zoomedRectangle.width = getSize().width / 3;
    }
    if (zoomedRectangle.height < getSize().height / 3) {
      zoomedRectangle.height = getSize().height / 3;
    }
  }
}
