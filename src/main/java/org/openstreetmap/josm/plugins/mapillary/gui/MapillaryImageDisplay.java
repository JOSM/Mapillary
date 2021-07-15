// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionListener;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.awt.geom.AffineTransform;
import java.awt.geom.PathIterator;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import javax.swing.ImageIcon;
import javax.swing.JPanel;

import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.layer.LayerManager;
import org.openstreetmap.josm.plugins.mapillary.actions.MapillaryDownloadAction;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.ObjectDetections;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.PointObjectLayer;
import org.openstreetmap.josm.plugins.mapillary.gui.panorama.CameraPlane;
import org.openstreetmap.josm.plugins.mapillary.gui.panorama.UVMapping;
import org.openstreetmap.josm.plugins.mapillary.model.ImageDetection;
import org.openstreetmap.josm.plugins.mapillary.utils.ImageViewUtil;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;

import org.apache.commons.math3.geometry.euclidean.threed.RotationConvention;
import org.apache.commons.math3.geometry.euclidean.threed.RotationOrder;

/**
 * This object is a responsible JComponent which lets you zoom and drag. It is
 * included in a {@link MapillaryMainDialog} object.
 *
 * @author nokutu
 * @see MapillaryImageDisplay
 * @see MapillaryMainDialog
 */
public final class MapillaryImageDisplay extends JPanel {

  private static final long serialVersionUID = 3369727203329307716L;
  static final double PANORAMA_FOV = Math.toRadians(110);

  private final transient Collection<ImageDetection<?>> detections = Collections.synchronizedList(new ArrayList<>());
  private final transient Collection<Painter<Graphics, BufferedImage, Rectangle>> additionalPainters = Collections
    .synchronizedCollection(new ArrayList<>());

  /** The image currently displayed */
  transient BufferedImage image;

  /** {@code true} if the currently visible image is panoramic */
  boolean pano;
  /** {@code true} if zoom + pan is enabled */
  private boolean zoomPanEnabled = true;

  /**
   * The rectangle (in image coordinates) of the image that is visible. This
   * rectangle is calculated each time the zoom is modified
   */
  Rectangle visibleRect;

  /**
   * When a selection is done, the rectangle of the selection (in image
   * coordinates)
   */
  Rectangle selectedRect;

  /**
   * When panorama 360-degree photo is downloaded, use offscreen buffer for display.
   */
  transient BufferedImage offscreenImage;

  /**
   * 360-degree panorama photo projection class.
   */
  transient CameraPlane cameraPlane;

  protected class ImgDisplayMouseListener extends MouseAdapter implements MouseWheelListener, MouseMotionListener {
    private boolean mouseIsDragging;
    private long lastTimeForMousePoint;
    private Point mousePointInImg;

    /**
     * Zoom in and out, trying to preserve the point of the image that was under
     * the mouse cursor at the same place
     */
    @Override
    public void mouseWheelMoved(MouseWheelEvent e) {
      Image mouseImage;
      Rectangle mouseVisibleRect;
      synchronized (MapillaryImageDisplay.this) {
        mouseImage = getImage();
        mouseVisibleRect = MapillaryImageDisplay.this.visibleRect;
      }
      this.mouseIsDragging = false;
      MapillaryImageDisplay.this.selectedRect = null;
      if (mouseImage != null && Math.min(getSize().getWidth(), getSize().getHeight()) > 0) {
        // Calculate the mouse cursor position in image coordinates, so that
        // we can center the zoom
        // on that mouse position.
        // To avoid issues when the user tries to zoom in on the image
        // borders, this point is not calculated
        // again if there was less than 1.5seconds since the last event.
        if (e.getWhen() - this.lastTimeForMousePoint > 1500 || this.mousePointInImg == null) {
          this.lastTimeForMousePoint = e.getWhen();
          this.mousePointInImg = comp2imgCoord(mouseVisibleRect, e.getX(), e.getY());
        }
        // Set the zoom to the visible rectangle in image coordinates
        if (e.getWheelRotation() > 0) {
          mouseVisibleRect.width = mouseVisibleRect.width * 3 / 2;
          mouseVisibleRect.height = mouseVisibleRect.height * 3 / 2;
        } else {
          mouseVisibleRect.width = mouseVisibleRect.width * 2 / 3;
          mouseVisibleRect.height = mouseVisibleRect.height * 2 / 3;
        }
        // Check that the zoom doesn't exceed 2:1
        if (mouseVisibleRect.width < getSize().width / 2) {
          mouseVisibleRect.width = getSize().width / 2;
        }
        if (mouseVisibleRect.height < getSize().height / 2) {
          mouseVisibleRect.height = getSize().height / 2;
        }
        // Set the same ratio for the visible rectangle and the display area
        int hFact = mouseVisibleRect.height * getSize().width;
        int wFact = mouseVisibleRect.width * getSize().height;
        if (hFact > wFact) {
          mouseVisibleRect.width = hFact / getSize().height;
        } else {
          mouseVisibleRect.height = wFact / getSize().width;
        }
        final Image maxImageSize;
        if (MapillaryImageDisplay.this.pano) {
          // The size of the visible rectangle is limited by the offscreenImage size.
          maxImageSize = offscreenImage;
        } else {
          // The size of the visible rectangle is limited by the image size.
          maxImageSize = mouseImage;
        }
        checkVisibleRectSize(maxImageSize, mouseVisibleRect);
        // Set the position of the visible rectangle, so that the mouse
        // cursor doesn't move on the image.
        Rectangle drawRect = calculateDrawImageRectangle(mouseVisibleRect);
        mouseVisibleRect.x = this.mousePointInImg.x
          + ((drawRect.x - e.getX()) * mouseVisibleRect.width) / drawRect.width;
        mouseVisibleRect.y = this.mousePointInImg.y
          + ((drawRect.y - e.getY()) * mouseVisibleRect.height) / drawRect.height;
        // The position is also limited by the image size
        ImageViewUtil.checkVisibleRectPos(maxImageSize, mouseVisibleRect);

        synchronized (MapillaryImageDisplay.this) {
          MapillaryImageDisplay.this.visibleRect = mouseVisibleRect;
        }
        MapillaryImageDisplay.this.repaint();
      }
    }

    /** Center the display on the point that has been clicked */
    @Override
    public void mouseClicked(MouseEvent e) {
      // Move the center to the clicked point.
      Image mouseImage;
      Rectangle mouseVisibleRect;
      synchronized (MapillaryImageDisplay.this) {
        mouseImage = getImage();
        mouseVisibleRect = MapillaryImageDisplay.this.visibleRect;
      }
      if (mouseImage != null && Math.min(getSize().getWidth(), getSize().getHeight()) > 0) {
        if (MapillaryImageDisplay.this.pano) {
          if (e.getButton() == MapillaryProperties.PICTURE_OPTION_BUTTON.get()) {
            if (!MapillaryImageDisplay.this.visibleRect
              .equals(new Rectangle(0, 0, offscreenImage.getWidth(null), offscreenImage.getHeight(null)))) {
              // Zoom to 1:1
              MapillaryImageDisplay.this.visibleRect = new Rectangle(0, 0, offscreenImage.getWidth(null),
                offscreenImage.getHeight(null));
              MapillaryImageDisplay.this.repaint();
            }
          } else if (e.getButton() == MapillaryProperties.PICTURE_DRAG_BUTTON.get()
            && MapillaryImageDisplay.this.cameraPlane != null) {
            cameraPlane.setRotation(comp2imgCoord(mouseVisibleRect, e.getX(), e.getY()));
            MapillaryImageDisplay.this.repaint();
          }
          return;
        }
        if (e.getButton() == MapillaryProperties.PICTURE_OPTION_BUTTON.get()) {
          if (!MapillaryImageDisplay.this.visibleRect
            .equals(new Rectangle(0, 0, mouseImage.getWidth(null), mouseImage.getHeight(null)))) {
            // Zooms to 1:1
            MapillaryImageDisplay.this.visibleRect = new Rectangle(0, 0, mouseImage.getWidth(null),
              mouseImage.getHeight(null));
          } else {
            // Zooms to best fit.
            MapillaryImageDisplay.this.visibleRect = new Rectangle(0,
              (mouseImage.getHeight(null) - (mouseImage.getWidth(null) * getHeight()) / getWidth()) / 2,
              mouseImage.getWidth(null), (mouseImage.getWidth(null) * getHeight()) / getWidth());
          }
          MapillaryImageDisplay.this.repaint();
          return;
        } else if (e.getButton() != MapillaryProperties.PICTURE_DRAG_BUTTON.get()) {
          return;
        }
        // Calculate the translation to set the clicked point the center of
        // the view.
        Point click = comp2imgCoord(mouseVisibleRect, e.getX(), e.getY());
        Point center = getCenterImgCoord(mouseVisibleRect);
        mouseVisibleRect.x += click.x - center.x;
        mouseVisibleRect.y += click.y - center.y;
        ImageViewUtil.checkVisibleRectPos(mouseImage, mouseVisibleRect);
        synchronized (MapillaryImageDisplay.this) {
          MapillaryImageDisplay.this.visibleRect = mouseVisibleRect;
        }
        MapillaryImageDisplay.this.repaint();
      }
    }

    /**
     * Initialize the dragging, either with button 1 (simple dragging) or button
     * 3 (selection of a picture part)
     */
    @Override
    public void mousePressed(MouseEvent e) {
      if (getImage() == null) {
        this.mouseIsDragging = false;
        MapillaryImageDisplay.this.selectedRect = null;
        return;
      }
      Image mousePressedImage;
      Rectangle mouseVisibleRect;
      synchronized (MapillaryImageDisplay.this) {
        mousePressedImage = MapillaryImageDisplay.this.image;
        mouseVisibleRect = MapillaryImageDisplay.this.visibleRect;
      }
      if (mousePressedImage == null)
        return;
      if (e.getButton() == MapillaryProperties.PICTURE_DRAG_BUTTON.get()) {
        this.mousePointInImg = comp2imgCoord(mouseVisibleRect, e.getX(), e.getY());
        this.mouseIsDragging = true;
        MapillaryImageDisplay.this.selectedRect = null;
      } else if (e.getButton() == MapillaryProperties.PICTURE_ZOOM_BUTTON.get()) {
        this.mousePointInImg = comp2imgCoord(mouseVisibleRect, e.getX(), e.getY());
        checkPointInVisibleRect(this.mousePointInImg, mouseVisibleRect);
        this.mouseIsDragging = false;
        MapillaryImageDisplay.this.selectedRect = new Rectangle(this.mousePointInImg.x, this.mousePointInImg.y, 0, 0);
        MapillaryImageDisplay.this.repaint();
      } else {
        this.mouseIsDragging = false;
        MapillaryImageDisplay.this.selectedRect = null;
      }
    }

    @Override
    public void mouseDragged(MouseEvent e) {
      if (!this.mouseIsDragging && MapillaryImageDisplay.this.selectedRect == null
        || !MapillaryImageDisplay.this.zoomPanEnabled)
        return;
      Image mouseImage;
      Rectangle mouseVisibleRect;
      synchronized (MapillaryImageDisplay.this) {
        mouseImage = getImage();
        mouseVisibleRect = MapillaryImageDisplay.this.visibleRect;
      }
      if (mouseImage == null) {
        this.mouseIsDragging = false;
        MapillaryImageDisplay.this.selectedRect = null;
        return;
      }
      if (this.mouseIsDragging) {
        Point p = comp2imgCoord(mouseVisibleRect, e.getX(), e.getY());
        if (!pano) {
          mouseVisibleRect.x += this.mousePointInImg.x - p.x;
          mouseVisibleRect.y += this.mousePointInImg.y - p.y;
          ImageViewUtil.checkVisibleRectPos(mouseImage, mouseVisibleRect);
          synchronized (MapillaryImageDisplay.this) {
            MapillaryImageDisplay.this.visibleRect = mouseVisibleRect;
          }
        } else if (MapillaryImageDisplay.this.cameraPlane != null) {
          cameraPlane.setRotationFromDelta(mousePointInImg, p);
          // Set the current mouse point to where the pointer is now.
          mousePointInImg = p;
        }
        MapillaryImageDisplay.this.repaint();
      } else if (MapillaryImageDisplay.this.selectedRect != null) {
        final Point p = comp2imgCoord(mouseVisibleRect, e.getX(), e.getY());
        checkPointInVisibleRect(p, mouseVisibleRect);
        Rectangle rect = new Rectangle(Math.min(p.x, this.mousePointInImg.x), Math.min(p.y, this.mousePointInImg.y),
          Math.abs(p.x - this.mousePointInImg.x), Math.abs(p.y - this.mousePointInImg.y));
        checkVisibleRectSize(mouseImage, rect);
        ImageViewUtil.checkVisibleRectPos(mouseImage, rect);
        MapillaryImageDisplay.this.selectedRect = rect;
        MapillaryImageDisplay.this.repaint();
      }
    }

    @Override
    public void mouseReleased(MouseEvent e) {
      if (!this.mouseIsDragging && MapillaryImageDisplay.this.selectedRect == null
        || !MapillaryImageDisplay.this.zoomPanEnabled)
        return;
      Image mouseImage;
      synchronized (MapillaryImageDisplay.this) {
        mouseImage = getImage();
      }
      if (mouseImage == null) {
        this.mouseIsDragging = false;
        MapillaryImageDisplay.this.selectedRect = null;
        return;
      }
      /*
       * When dragging panorama photo, re-calculate rotation when release.
       * For normal photo, just stop dragging flag because redraw during dragging.
       */
      if (MapillaryImageDisplay.this.selectedRect != null) {
        int oldWidth = MapillaryImageDisplay.this.selectedRect.width;
        int oldHeight = MapillaryImageDisplay.this.selectedRect.height;
        // Check that the zoom doesn't exceed 2:1
        if (MapillaryImageDisplay.this.selectedRect.width < getSize().width / 2) {
          MapillaryImageDisplay.this.selectedRect.width = getSize().width / 2;
        }
        if (MapillaryImageDisplay.this.selectedRect.height < getSize().height / 2) {
          MapillaryImageDisplay.this.selectedRect.height = getSize().height / 2;
        }
        // Set the same ratio for the visible rectangle and the display
        // area
        int hFact = MapillaryImageDisplay.this.selectedRect.height * getSize().width;
        int wFact = MapillaryImageDisplay.this.selectedRect.width * getSize().height;
        if (hFact > wFact) {
          MapillaryImageDisplay.this.selectedRect.width = hFact / getSize().height;
        } else {
          MapillaryImageDisplay.this.selectedRect.height = wFact / getSize().width;
        }
        // Keep the center of the selection
        if (MapillaryImageDisplay.this.selectedRect.width != oldWidth) {
          MapillaryImageDisplay.this.selectedRect.x -= (MapillaryImageDisplay.this.selectedRect.width - oldWidth) / 2;
        }
        if (MapillaryImageDisplay.this.selectedRect.height != oldHeight) {
          MapillaryImageDisplay.this.selectedRect.y -= (MapillaryImageDisplay.this.selectedRect.height - oldHeight) / 2;
        }
        checkVisibleRectSize(mouseImage, MapillaryImageDisplay.this.selectedRect);
        ImageViewUtil.checkVisibleRectPos(mouseImage, MapillaryImageDisplay.this.selectedRect);
        synchronized (MapillaryImageDisplay.this) {
          MapillaryImageDisplay.this.visibleRect = MapillaryImageDisplay.this.selectedRect;
        }
        MapillaryImageDisplay.this.selectedRect = null;
        MapillaryImageDisplay.this.repaint();
      }
    }

    private void checkPointInVisibleRect(Point p, Rectangle visibleRect) {
      if (p.x < visibleRect.x) {
        p.x = visibleRect.x;
      }
      if (p.x > visibleRect.x + visibleRect.width) {
        p.x = visibleRect.x + visibleRect.width;
      }
      if (p.y < visibleRect.y) {
        p.y = visibleRect.y;
      }
      if (p.y > visibleRect.y + visibleRect.height) {
        p.y = visibleRect.y + visibleRect.height;
      }
    }
  }

  /**
   * Main constructor.
   */
  MapillaryImageDisplay() {
    ImgDisplayMouseListener mouseListener = new ImgDisplayMouseListener();
    addMouseListener(mouseListener);
    setOpaque(true);
    addMouseWheelListener(mouseListener);
    addMouseMotionListener(mouseListener);
    addComponentListener(new ComponentSizeListener());

    MainApplication.getLayerManager().addLayerChangeListener(new LayerManager.LayerChangeListener() {
      @Override
      public void layerAdded(LayerManager.LayerAddEvent e) {
        // We don't care about this
      }

      @Override
      public void layerRemoving(LayerManager.LayerRemoveEvent e) {
        if (e.getRemovedLayer() instanceof MapillaryLayer) {
          setImage(null, Collections.emptyList(), false);
        }
      }

      @Override
      public void layerOrderChanged(LayerManager.LayerOrderChangeEvent e) {
        // We don't care about this
      }
    });

    MapillaryProperties.SHOW_DETECTION_OUTLINES.addListener(it -> repaint());
    MapillaryProperties.SHOW_DETECTED_SIGNS.addListener(it -> repaint());
  }

  /**
   * Sets a new picture to be displayed.
   *
   * @param image The picture to be displayed.
   * @param detections image detections
   * @param pano The property to indicate whether image is panorama or not.
   */
  void setImage(BufferedImage image, Collection<ImageDetection<?>> detections, boolean pano) {
    synchronized (this) {
      this.image = image;
      this.pano = pano;
      this.detections.clear();
      if (detections != null) {
        this.detections.addAll(detections);
      }
      this.selectedRect = null;
      this.visibleRect = getDefaultVisibleRect();
    }
    repaint();
  }

  public Rectangle getDefaultVisibleRect() {
    if (image != null) {
      Rectangle visibleRectangle;
      if (this.pano) {
        visibleRectangle = new Rectangle(0, 0, getSize().width, getSize().height);
      } else {
        visibleRectangle = new Rectangle(0, 0, image.getWidth(null), image.getHeight(null));
      }
      return visibleRectangle;
    }
    return visibleRect;
  }

  /**
   * Returns the picture that is being displayed
   *
   * @return The picture that is being displayed.
   */
  public BufferedImage getImage() {
    return this.image;
  }

  /**
   * Paints the visible part of the picture.
   */
  @Override
  public void paintComponent(Graphics g) {
    super.paintComponent(g);
    final BufferedImage bufferedImage;
    final Rectangle paintVisibleRect;
    synchronized (this) {
      bufferedImage = this.image;
      paintVisibleRect = this.visibleRect;
    }
    if (bufferedImage == null) {
      paintNoImage(g);
    } else {
      paintImage(g, bufferedImage, paintVisibleRect);
      paintDetections(g, paintVisibleRect);
      this.additionalPainters.forEach(consumer -> consumer.accept(g, bufferedImage, paintVisibleRect));
    }
  }

  private void paintNoImage(Graphics g) {
    final String noImageStr = MapillaryLayer.hasInstance() ? tr("no image selected")
      : tr("Press \"{0}\" to download images", MapillaryDownloadAction.SHORTCUT.getKeyText());
    if (noImageStr != null) {
      Rectangle2D noImageSize = g.getFontMetrics(g.getFont()).getStringBounds(noImageStr, g);
      Dimension size = getSize();
      g.setColor(getForeground());
      g.drawString(noImageStr, (int) ((size.width - noImageSize.getWidth()) / 2),
        (int) ((size.height - noImageSize.getHeight()) / 2));
    }
  }

  public void paintLoadingImage() {
    if (getGraphics() != null)
      paintLoadingImage(getGraphics());
  }

  private void paintLoadingImage(Graphics g) {
    final String noImageStr = tr("loading image");
    Rectangle2D noImageSize = g.getFontMetrics().getStringBounds(noImageStr, g);
    Dimension size = getSize();
    int llx = (int) ((size.width - noImageSize.getWidth()) / 2);
    int lly = (int) ((size.height - noImageSize.getHeight()) / 2);
    if (g instanceof Graphics2D) {
      Graphics2D g2d = (Graphics2D) g;
      int height = g2d.getFontMetrics().getHeight();
      int descender = g2d.getFontMetrics().getDescent();
      g2d.setColor(getBackground());
      int width = (int) (noImageSize.getWidth() * 1);
      int tlx = (int) ((size.getWidth() - noImageSize.getWidth()) / 2);
      int tly = (int) ((size.getHeight() - 3 * noImageSize.getHeight()) / 2 + descender);
      g2d.fillRect(tlx, tly, width, height);
      g2d.setColor(getForeground());
      g2d.drawString(noImageStr, llx, lly);
    } else {
      g.setColor(getForeground());
      g.drawString(noImageStr, llx, lly);
    }
  }

  private void paintImage(Graphics g, BufferedImage image, Rectangle visibleRect) {
    final Rectangle target;
    if (this.pano && this.cameraPlane != null) {
      this.cameraPlane.mapping(image, offscreenImage);
      target = new Rectangle(0, 0, offscreenImage.getWidth(null), offscreenImage.getHeight(null));
      g.drawImage(offscreenImage, target.x, target.y, target.x + target.width, target.y + target.height, visibleRect.x,
        visibleRect.y, visibleRect.x + visibleRect.width, visibleRect.y + visibleRect.height, null);
    } else {
      target = calculateDrawImageRectangle(visibleRect);
      g.drawImage(image, target.x, target.y, target.x + target.width, target.y + target.height, visibleRect.x,
        visibleRect.y, visibleRect.x + visibleRect.width, visibleRect.y + visibleRect.height, null);
      if (this.selectedRect != null && this.zoomPanEnabled) {
        Point topLeft = img2compCoord(visibleRect, this.selectedRect.x, this.selectedRect.y);
        Point bottomRight = img2compCoord(visibleRect, this.selectedRect.x + this.selectedRect.width,
          this.selectedRect.y + this.selectedRect.height);
        g.setColor(new Color(128, 128, 128, 180));
        g.fillRect(target.x, target.y, target.width, topLeft.y - target.y);
        g.fillRect(target.x, target.y, topLeft.x - target.x, target.height);
        g.fillRect(bottomRight.x, target.y, target.x + target.width - bottomRight.x, target.height);
        g.fillRect(target.x, bottomRight.y, target.width, target.y + target.height - bottomRight.y);
        g.setColor(Color.black);
        g.drawRect(topLeft.x, topLeft.y, bottomRight.x - topLeft.x, bottomRight.y - topLeft.y);
      }
    }
  }

  private void paintDetections(Graphics g, Rectangle visibleRect) {
    if (g instanceof Graphics2D) {
      final Graphics2D g2d = (Graphics2D) g;
      g2d.setStroke(new BasicStroke(2));
      List<PointObjectLayer> detectionLayers = MainApplication.getLayerManager()
        .getLayersOfType(PointObjectLayer.class);
      synchronized (detections) {
        if (pano) {
          paintPano(g2d, detectionLayers);
        } else {
          paintNonPano(g2d, visibleRect, detectionLayers);
        }
      }
    }
  }

  private void paintPano(Graphics2D g2d, List<PointObjectLayer> detectionLayers) {
    if (this.cameraPlane == null) {
      return;
    }
    List<ImageDetection<?>> paintDetections = detections.parallelStream()
      .filter(d -> !MapillaryUtils.checkIfDetectionIsFilteredBasic(detectionLayers, d)).collect(Collectors.toList());
    for (final ImageDetection<?> d : paintDetections) {
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

  private void paintNonPano(Graphics2D g2d, Rectangle visibleRect, List<PointObjectLayer> detectionLayers) {
    final Point upperLeft = img2compCoord(visibleRect, 0, 0);
    final Point lowerRight = img2compCoord(visibleRect, this.getImage().getWidth(), this.getImage().getHeight());
    final AffineTransform unit2CompTransform = AffineTransform.getTranslateInstance(upperLeft.getX(), upperLeft.getY());
    unit2CompTransform.concatenate(
      AffineTransform.getScaleInstance(lowerRight.getX() - upperLeft.getX(), lowerRight.getY() - upperLeft.getY()));

    for (final ImageDetection<?> d : this.detections) {
      if (MapillaryUtils.checkIfDetectionIsFilteredBasic(detectionLayers, d))
        continue;
      final Shape shape = unit2CompTransform.createTransformedShape(d.getShape());
      g2d.setColor(d.getColor());
      g2d.draw(shape);
      ImageIcon icon = d.getValue().getIcon();
      if (d.isTrafficSign() && icon != null && !icon.equals(ObjectDetections.NO_ICON)) {
        final Rectangle bounds = shape.getBounds();
        g2d.drawImage(icon.getImage(), bounds.x, bounds.y, bounds.width, bounds.height, null);
      }
    }
  }

  private Point img2compCoord(Rectangle visibleRect, int xImg, int yImg) {
    Rectangle drawRect = calculateDrawImageRectangle(visibleRect);
    return new Point(drawRect.x + ((xImg - visibleRect.x) * drawRect.width) / visibleRect.width,
      drawRect.y + ((yImg - visibleRect.y) * drawRect.height) / visibleRect.height);
  }

  Point comp2imgCoord(Rectangle visibleRect, int xComp, int yComp) {
    Rectangle drawRect = calculateDrawImageRectangle(visibleRect);
    return new Point(visibleRect.x + ((xComp - drawRect.x) * visibleRect.width) / drawRect.width,
      visibleRect.y + ((yComp - drawRect.y) * visibleRect.height) / drawRect.height);
  }

  static Point getCenterImgCoord(Rectangle visibleRect) {
    return new Point(visibleRect.x + visibleRect.width / 2, visibleRect.y + visibleRect.height / 2);
  }

  Rectangle calculateDrawImageRectangle(Rectangle visibleRect) {
    return ImageViewUtil.calculateDrawImageRectangle(visibleRect,
      new Rectangle(0, 0, getSize().width, getSize().height));
  }

  protected static void checkVisibleRectSize(Image image, Rectangle visibleRect) {
    if (visibleRect.width > image.getWidth(null)) {
      visibleRect.width = image.getWidth(null);
    }
    if (visibleRect.height > image.getHeight(null)) {
      visibleRect.height = image.getHeight(null);
    }
  }

  protected static class ComponentSizeListener extends ComponentAdapter {
    @Override
    public void componentResized(ComponentEvent e) {
      final Component component = e.getComponent();
      if (component instanceof MapillaryImageDisplay && e.getComponent().getWidth() > 0
        && e.getComponent().getHeight() > 0) {
        final MapillaryImageDisplay imgDisplay = (MapillaryImageDisplay) component;
        imgDisplay.offscreenImage = new BufferedImage(e.getComponent().getWidth(), e.getComponent().getHeight(),
          BufferedImage.TYPE_3BYTE_BGR);
        final double cameraPlaneDistance = (e.getComponent().getWidth() / 2d) / Math.tan(PANORAMA_FOV / 2);
        imgDisplay.cameraPlane = new CameraPlane(e.getComponent().getWidth(), e.getComponent().getHeight(),
          cameraPlaneDistance);
      }
    }
  }

  /**
   * Set the detections to be shown
   *
   * @param detections The detections to set. Triggers a repaint.
   */
  public void setAllDetections(List<ImageDetection<?>> detections) {
    this.detections.clear();
    if (detections != null) {
      this.detections.addAll(detections);
    }
    repaint();
  }

  /**
   * Get the detections to be shown
   *
   * @return The detections to be shown.
   */
  public Collection<ImageDetection<?>> getShownDetections() {
    List<PointObjectLayer> layers = MainApplication.getLayerManager().getLayersOfType(PointObjectLayer.class);
    return this.detections.stream().filter(d -> !MapillaryUtils.checkIfDetectionIsFiltered(layers, d))
      .collect(Collectors.toList());
  }

  /**
   * Enable or disable zoom+pan functionality
   *
   * @param enabled {@code true} to enable zoom+pan functionality
   */
  public void setZoomPanEnabled(boolean enabled) {
    this.zoomPanEnabled = enabled;
  }

  /**
   * Add additional drawing functions for the image viewer
   *
   * @param painter Additional drawing function
   * @return If successfully added to the additional painters
   */
  public boolean addAdditionalFunction(Painter<Graphics, BufferedImage, Rectangle> painter) {
    return this.additionalPainters.add(painter);
  }

  /**
   * Remove additional drawing functions for the image viewer
   *
   * @param painter Additional drawing function
   * @return If successfully removed from the additional painters
   */
  public boolean removeAdditionalFunction(Painter<Graphics, BufferedImage, Rectangle> painter) {
    return this.additionalPainters.remove(painter);
  }

  /**
   * Get the rotation of the current image (mostly used by 360 images)
   *
   * @return The current rotation for the image view
   */
  public double getRotation() {
    if (this.pano && this.cameraPlane != null) {
      double[] rotation = this.cameraPlane.getRotation().getAngles(RotationOrder.XYZ,
        RotationConvention.VECTOR_OPERATOR);
      return rotation[2];
    }
    // Default to 0 for standard images
    return 0;
  }

  /**
   * Interface for additional painters (really just a generic TriConsumer)
   *
   * @param <T> Typically a Graphics object
   * @param <U> Typically a buffered image
   * @param <V> Typically a rectangle
   */
  public interface Painter<T, U, V> {
    void accept(T t, U u, V v);
  }

}
