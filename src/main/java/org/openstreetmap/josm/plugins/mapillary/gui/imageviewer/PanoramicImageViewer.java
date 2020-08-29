// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.imageviewer;

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.geom.PathIterator;
import java.awt.image.BufferedImage;
import java.util.List;
import java.util.stream.Collectors;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.PointObjectLayer;
import org.openstreetmap.josm.plugins.mapillary.gui.panorama.CameraPlane;
import org.openstreetmap.josm.plugins.mapillary.gui.panorama.UVMapping;
import org.openstreetmap.josm.plugins.mapillary.model.ImageDetection;
import org.openstreetmap.josm.plugins.mapillary.utils.ImageViewUtil;
import static org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils.checkIfDetectionIsFiltered;

/**
 * Image viewer for Panoramic Images.
 * @author Kishan
 */
public class PanoramicImageViewer extends AbstractImageViewer {

  private BufferedImage offscreenImage;
  /**
   * 360-degree panorama photo projection class.
   */
  private CameraPlane cameraPlane;
  private Point mousePointInImg;
  static final double PANORAMA_FOV = Math.toRadians(110);
  /**
   * Use INT_RGB for faster image mapping
   * See {@link CameraPlane#mapping}.
   */
  static final int imageType = BufferedImage.TYPE_INT_RGB;
  public ZoomPanMouseListener zoomPanMouseListener;

  public PanoramicImageViewer() {
    super();
    zoomPanMouseListener = new ZoomPanMouseListener(this);
    addMouseListener(zoomPanMouseListener);
    addMouseWheelListener(zoomPanMouseListener);
    addMouseMotionListener(zoomPanMouseListener);
    addComponentListener(new ComponentSizeListener());
  }

  @Override
  public Rectangle getDefaultVisibleRect() {
    if (image != null) {
      Rectangle visibleRectangle = new Rectangle(0, 0, getWidth(), getHeight());
      return visibleRectangle;
    }
    return visibleRect;
  }

  /**
   * Change image to type {@link BufferedImage#TYPE_INT_RGB} for faster image mapping.
   * See {@link CameraPlane#mapping}.
   */
  @Override
  protected void setImage(BufferedImage image) {
    if (image != null) {
      BufferedImage newImage = new BufferedImage(image.getWidth(),
        image.getHeight(), imageType);
      Graphics2D g = newImage.createGraphics();
      g.drawImage(image, 0, 0, null);
      g.dispose();
      super.setImage(newImage);
    } else {
      super.setImage(image);
    }
  }

  @Override
  protected void paintImage(Graphics g, BufferedImage image, Rectangle visibleRect) {
    final Rectangle target;
    if (cameraPlane == null) {
      double cameraPlaneDistance = (getWidth() / 2d) / Math.tan(PANORAMA_FOV / 2);
      cameraPlane = new CameraPlane(this.getWidth(), getHeight(), cameraPlaneDistance);
    }
    if (offscreenImage == null) {
      offscreenImage = new BufferedImage(getWidth(), getHeight(), imageType);
    }
    cameraPlane.mapping(image, offscreenImage);
    target = new Rectangle(0, 0, getWidth(), getHeight());
    g.drawImage(offscreenImage, target.x, target.y, target.x + target.width, target.y
      + target.height, visibleRect.x, visibleRect.y, visibleRect.x
      + visibleRect.width, visibleRect.y + visibleRect.height, null);
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
    ImageViewUtil.checkVisibleRectSize(offscreenImage, mouseVisibleRect);
    Rectangle drawRect = calculateDrawImageRectangle(mouseVisibleRect);
    mouseVisibleRect.x = mousePointInImg.x
      + ((drawRect.x - zoomCenterX) * mouseVisibleRect.width) / drawRect.width;
    mouseVisibleRect.y = mousePointInImg.y
      + ((drawRect.y - zoomCenterY) * mouseVisibleRect.height) / drawRect.height;
    ImageViewUtil.checkVisibleRectPos(offscreenImage, mouseVisibleRect);
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
    ImageViewUtil.checkPointInRect(newImagePoint, mouseVisibleRect);
    cameraPlane.setRotationFromDelta(mousePointInImg, newImagePoint);
    mousePointInImg = newImagePoint;
    repaint();
  }

  @Override
  protected void paintDetections(Graphics2D g2d, Rectangle visibleRect, List<PointObjectLayer> detectionLayers) {
    List<ImageDetection> paintDetections = detections.parallelStream()
      .filter(d -> !checkIfDetectionIsFiltered(detectionLayers, d)).collect(Collectors.toList());
    for (final ImageDetection d : paintDetections) {
      g2d.setColor(d.getColor());
      final PathIterator pathIt = d.getShape().getPathIterator(null);
      Point prevPoint = null;
      while (!pathIt.isDone()) {
        final double[] buffer = new double[6];
        final int segmentType = pathIt.currentSegment(buffer);

        if (segmentType == PathIterator.SEG_LINETO || segmentType == PathIterator.SEG_QUADTO
          || segmentType == PathIterator.SEG_CUBICTO) {
          // Takes advantage of the fact that SEG_LINETO=1, SEG_QUADTO=2, SEG_CUBICTO=3 and currentSegment() returns 1,
          // 2 and 3 points for each of these segment types
          final Point curPoint = cameraPlane
            .getPoint(UVMapping.getVector(buffer[2 * (segmentType - 1)], buffer[2 * (segmentType - 1) + 1]));
          if (prevPoint != null && curPoint != null) {
            g2d.drawLine(prevPoint.x, prevPoint.y, curPoint.x, curPoint.y);
          }
          prevPoint = curPoint;
        } else if (segmentType == PathIterator.SEG_MOVETO) {
          prevPoint = cameraPlane.getPoint(UVMapping.getVector(buffer[0], buffer[1]));
        } else {
          prevPoint = null;
        }
        pathIt.next();
      }
    }
  }

  private boolean validMousePoint(Point p) {
    if (0 <= p.x && p.x < offscreenImage.getWidth()) {
      if (0 <= p.y && p.y < offscreenImage.getHeight()) {
        return true;
      }
    }
    return false;
  }

  protected static class ComponentSizeListener extends ComponentAdapter {

    @Override
    public void componentResized(ComponentEvent e) {
      final Component component = e.getComponent();
      if (component instanceof PanoramicImageViewer) {
        final PanoramicImageViewer imgDisplay = (PanoramicImageViewer) component;
        int width = imgDisplay.getWidth();
        int height = imgDisplay.getHeight();
        /*
           for performance issues
           if (width * height > Math.pow(10, 6)) {
           double scaleFactor = Math.sqrt(Math.pow(10, 6) / (width * height));
           width = (int) Math.round(width * scaleFactor);
           height = (int) Math.round(height * scaleFactor);
           }
         */
        imgDisplay.offscreenImage = new BufferedImage(width, height,
          imageType);
        double cameraPlaneDistance = (width / 2d) / Math.tan(PANORAMA_FOV / 2);
        imgDisplay.cameraPlane = new CameraPlane(width, height,
          cameraPlaneDistance);
        if (imgDisplay.getImage() != null) {
          imgDisplay.checkZoom(imgDisplay.visibleRect);
          imgDisplay.checkAspectRatio(imgDisplay.visibleRect);
          ImageViewUtil.checkVisibleRectSize(imgDisplay.offscreenImage, imgDisplay.visibleRect);
        }
      }
    }
  }
}
