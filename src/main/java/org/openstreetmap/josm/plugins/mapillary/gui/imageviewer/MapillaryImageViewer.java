// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.imageviewer;

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.geom.AffineTransform;
import java.awt.image.BufferedImage;
import java.util.List;
import javax.swing.ImageIcon;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.PointObjectLayer;
import org.openstreetmap.josm.plugins.mapillary.model.ImageDetection;
import org.openstreetmap.josm.plugins.mapillary.model.MapObject;
import org.openstreetmap.josm.plugins.mapillary.utils.ImageViewUtil;
import static org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils.checkIfDetectionIsFiltered;

public class MapillaryImageViewer extends AbstractImageViewer {

  private Point mousePointInImg;
  public ZoomPanMouseListener zoomPanMouseListener;

  public MapillaryImageViewer() {
    super();
    zoomPanMouseListener = new ZoomPanMouseListener(this);
    addMouseListener(zoomPanMouseListener);
    addMouseWheelListener(zoomPanMouseListener);
    addMouseMotionListener(zoomPanMouseListener);
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
  protected void paintImage(Graphics g, BufferedImage image, Rectangle visibleRect) {
    Rectangle target = ImageViewUtil.calculateDrawImageRectangle(visibleRect,
      new Rectangle(0, 0, getSize().width, getSize().height));
    g.drawImage(image, target.x, target.y, target.x + target.width,
      target.y + target.height, visibleRect.x, visibleRect.y,
      visibleRect.x + visibleRect.width, visibleRect.y + visibleRect.height, this);
  }

  @Override
  public void zoom(int zoomCenterX, int zoomCenterY, boolean zoomedIn) {
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
    if (zoomedIn) {
      mouseVisibleRect.width = mouseVisibleRect.width * 2 / 3;
      mouseVisibleRect.height = mouseVisibleRect.height * 2 / 3;
    } else {
      mouseVisibleRect.width = mouseVisibleRect.width * 3 / 2;
      mouseVisibleRect.height = mouseVisibleRect.height * 3 / 2;
    }
    checkZoom(mouseVisibleRect);
    checkAspectRatio(mouseVisibleRect);
    ImageViewUtil.checkVisibleRectSize(mouseImage, mouseVisibleRect);
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
  protected void paintDetections(Graphics2D g2d, Rectangle visibleRect, List<PointObjectLayer> detectionLayers) {
    final Point upperLeft = img2compCoord(visibleRect, 0, 0);
    final Point lowerRight = img2compCoord(visibleRect, getImage().getWidth(), getImage().getHeight());
    final AffineTransform unit2CompTransform = AffineTransform.getTranslateInstance(upperLeft.getX(), upperLeft.getY());
    unit2CompTransform.concatenate(
      AffineTransform.getScaleInstance(lowerRight.getX() - upperLeft.getX(), lowerRight.getY() - upperLeft.getY()));

    for (final ImageDetection d : detections) {
      if (checkIfDetectionIsFiltered(detectionLayers, d)) {
        continue;
      }
      final Shape shape = d.getShape().createTransformedShape(unit2CompTransform);
      g2d.setColor(d.getColor());
      g2d.draw(shape);
      ImageIcon icon = MapObject.getIcon(d.getValue());
      if (d.isTrafficSign() && icon != null && !icon.equals(MapObject.ICON_NULL_TYPE)) {
        final Rectangle bounds = shape.getBounds();
        g2d.drawImage(MapObject.getIcon(d.getValue()).getImage(), bounds.x, bounds.y, bounds.width, bounds.height,
          null);
      }
    }
  }

  protected static class ComponentSizeListener extends ComponentAdapter {
    @Override
    public void componentResized(ComponentEvent e) {
      final Component component = e.getComponent();
      if (component instanceof MapillaryImageViewer) {
        final MapillaryImageViewer imgDisplay = (MapillaryImageViewer) component;
        if (imgDisplay.getImage() != null) {
          imgDisplay.checkAspectRatio(imgDisplay.visibleRect);
          ImageViewUtil.checkVisibleRectSize(imgDisplay.image, imgDisplay.visibleRect);
        }
      }
    }
  }
}
