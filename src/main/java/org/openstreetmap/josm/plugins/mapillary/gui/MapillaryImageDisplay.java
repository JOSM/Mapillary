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
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
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

import org.openstreetmap.josm.data.osm.OsmPrimitive;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.layer.LayerManager;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.actions.MapillaryDownloadAction;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.PointObjectLayer;
import org.openstreetmap.josm.plugins.mapillary.gui.panorama.CameraPlane;
import org.openstreetmap.josm.plugins.mapillary.gui.panorama.UVMapping;
import org.openstreetmap.josm.plugins.mapillary.model.ImageDetection;
import org.openstreetmap.josm.plugins.mapillary.model.MapObject;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryColorScheme;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;

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

  private final transient Collection<ImageDetection> detections = Collections.synchronizedList(new ArrayList<>());

  /** The image currently displayed */
  transient volatile BufferedImage image;

  boolean pano;

  /**
   * The rectangle (in image coordinates) of the image that is visible. This
   * rectangle is calculated each time the zoom is modified
   */
  volatile Rectangle visibleRect;

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

  protected class ImgDisplayMouseListener implements MouseListener, MouseWheelListener, MouseMotionListener {
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
        if (MapillaryImageDisplay.this.pano) {
           // The size of the visible rectangle is limited by the offscreenImage size.
          checkVisibleRectSize(offscreenImage, mouseVisibleRect);
          // Set the position of the visible rectangle, so that the mouse
          // cursor doesn't move on the image.
          Rectangle drawRect = calculateDrawImageRectangle(mouseVisibleRect);
          mouseVisibleRect.x = this.mousePointInImg.x
              + ((drawRect.x - e.getX()) * mouseVisibleRect.width) / drawRect.width;
          mouseVisibleRect.y = this.mousePointInImg.y
              + ((drawRect.y - e.getY()) * mouseVisibleRect.height) / drawRect.height;
          // The position is also limited by the image size
          checkVisibleRectPos(offscreenImage, mouseVisibleRect);
        } else {
          // The size of the visible rectangle is limited by the image size.
          checkVisibleRectSize(mouseImage, mouseVisibleRect);
          // Set the position of the visible rectangle, so that the mouse
          // cursor doesn't move on the image.
          Rectangle drawRect = calculateDrawImageRectangle(mouseVisibleRect);
          mouseVisibleRect.x = this.mousePointInImg.x
              + ((drawRect.x - e.getX()) * mouseVisibleRect.width) / drawRect.width;
          mouseVisibleRect.y = this.mousePointInImg.y
              + ((drawRect.y - e.getY()) * mouseVisibleRect.height) / drawRect.height;
          // The position is also limited by the image size
          checkVisibleRectPos(mouseImage, mouseVisibleRect);
        }
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
            if (!MapillaryImageDisplay.this.visibleRect.equals(new Rectangle(0, 0,
                    offscreenImage.getWidth(null), offscreenImage.getHeight(null)))) {
              // Zoom to 1:1
              MapillaryImageDisplay.this.visibleRect = new Rectangle(0, 0,
                  offscreenImage.getWidth(null), offscreenImage.getHeight(null));
              MapillaryImageDisplay.this.repaint();
            }
          } else if (e.getButton() == MapillaryProperties.PICTURE_DRAG_BUTTON.get()) {
            cameraPlane.setRotation(comp2imgCoord(mouseVisibleRect, e.getX(), e.getY()));
            MapillaryImageDisplay.this.repaint();
          }
          return;
        } else {
          if (e.getButton() == MapillaryProperties.PICTURE_OPTION_BUTTON.get()) {
            if (!MapillaryImageDisplay.this.visibleRect.equals(new Rectangle(0, 0, mouseImage.getWidth(null), mouseImage.getHeight(null)))) {
              // Zooms to 1:1
              MapillaryImageDisplay.this.visibleRect = new Rectangle(0, 0,
                  mouseImage.getWidth(null), mouseImage.getHeight(null));
            } else {
              // Zooms to best fit.
              MapillaryImageDisplay.this.visibleRect = new Rectangle(
                  0,
                  (mouseImage.getHeight(null) - (mouseImage.getWidth(null) * getHeight()) / getWidth()) / 2,
                  mouseImage.getWidth(null),
                  (mouseImage.getWidth(null) * getHeight()) / getWidth()
              );
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
          checkVisibleRectPos(mouseImage, mouseVisibleRect);
          synchronized (MapillaryImageDisplay.this) {
            MapillaryImageDisplay.this.visibleRect = mouseVisibleRect;
          }
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
      if (!this.mouseIsDragging && MapillaryImageDisplay.this.selectedRect == null)
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
          checkVisibleRectPos(mouseImage, mouseVisibleRect);
          synchronized (MapillaryImageDisplay.this) {
            MapillaryImageDisplay.this.visibleRect = mouseVisibleRect;
          }
        } else {
          cameraPlane.setRotationFromDelta(mousePointInImg, p);
          // Set the current mouse point to where the pointer is now.
          mousePointInImg = p;
        }
        MapillaryImageDisplay.this.repaint();
      } else if (MapillaryImageDisplay.this.selectedRect != null) {
        final Point p = comp2imgCoord(mouseVisibleRect, e.getX(), e.getY());
        checkPointInVisibleRect(p, mouseVisibleRect);
        Rectangle rect = new Rectangle(
          Math.min(p.x, this.mousePointInImg.x),
          Math.min(p.y, this.mousePointInImg.y),
          Math.abs(p.x - this.mousePointInImg.x),
          Math.abs(p.y - this.mousePointInImg.y)
        );
        checkVisibleRectSize(mouseImage, rect);
        checkVisibleRectPos(mouseImage, rect);
        MapillaryImageDisplay.this.selectedRect = rect;
        MapillaryImageDisplay.this.repaint();
      }
    }

    @Override
    public void mouseReleased(MouseEvent e) {
      if (!this.mouseIsDragging && MapillaryImageDisplay.this.selectedRect == null)
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
       * When dragging panorama photo, re-calcurate rotation when release.
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
        checkVisibleRectPos(mouseImage, MapillaryImageDisplay.this.selectedRect);
        synchronized (MapillaryImageDisplay.this) {
          MapillaryImageDisplay.this.visibleRect = MapillaryImageDisplay.this.selectedRect;
        }
        MapillaryImageDisplay.this.selectedRect = null;
        MapillaryImageDisplay.this.repaint();
      }
    }

    @Override
    public void mouseEntered(MouseEvent e) {
      // Do nothing, method is enforced by MouseListener
    }

    @Override
    public void mouseExited(MouseEvent e) {
      // Do nothing, method is enforced by MouseListener
    }

    @Override
    public void mouseMoved(MouseEvent e) {
      // Do nothing, method is enforced by MouseListener
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
    setDarkMode(MapillaryProperties.DARK_MODE.get());
    addMouseWheelListener(mouseListener);
    addMouseMotionListener(mouseListener);
    addComponentListener(new ComponentSizeListener());

    MainApplication.getLayerManager().addLayerChangeListener(new LayerManager.LayerChangeListener() {
      @Override
      public void layerAdded(LayerManager.LayerAddEvent e) { }

      @Override
      public void layerRemoving(LayerManager.LayerRemoveEvent e) {
        if (e.getRemovedLayer() instanceof MapillaryLayer) {
          setImage(null, Collections.emptyList(), false);
        }
      }

      @Override
      public void layerOrderChanged(LayerManager.LayerOrderChangeEvent e) { }
    });

    MapillaryProperties.SHOW_DETECTION_OUTLINES.addListener(it -> repaint());
    MapillaryProperties.SHOW_DETECTED_SIGNS.addListener(it -> repaint());
    MapillaryProperties.DARK_MODE.addListener(it -> setDarkMode(it.getProperty().get()));
  }

  private void setDarkMode(final boolean darkMode) {
    setBackground(darkMode ? MapillaryColorScheme.TOOLBAR_DARK_GREY : Color.WHITE);
    setForeground(darkMode ? Color.LIGHT_GRAY : Color.DARK_GRAY);
  }

  /**
   * Sets a new picture to be displayed.
   *
   * @param image      The picture to be displayed.
   * @param detections image detections
   * @param pano       The property to indicate whether image is panorama or not.
   */
  void setImage(BufferedImage image, Collection<ImageDetection> detections, boolean pano) {
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
      Rectangle visibleRectangle = null;
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
    }
  }

  private void paintNoImage(Graphics g) {
    final String noImageStr = MapillaryLayer.hasInstance() ? tr("no image selected") : tr("Press \"{0}\" to download images", MapillaryDownloadAction.SHORTCUT.getKeyText());
    if (noImageStr != null) {
      Rectangle2D noImageSize = g.getFontMetrics(g.getFont()).getStringBounds(noImageStr, g);
      Dimension size = getSize();
      g.setColor(getForeground());
      g.drawString(noImageStr,
        (int) ((size.width - noImageSize.getWidth()) / 2),
        (int) ((size.height - noImageSize.getHeight()) / 2));
    }
  }

  private void paintImage(Graphics g, BufferedImage image, Rectangle visibleRect) {
    final Rectangle target;
    if (this.pano) {
      cameraPlane.mapping(image, offscreenImage);
      target = new Rectangle(0, 0, offscreenImage.getWidth(null), offscreenImage.getHeight(null));
      g.drawImage(offscreenImage, target.x, target.y, target.x + target.width, target.y
          + target.height, visibleRect.x, visibleRect.y, visibleRect.x
          + visibleRect.width, visibleRect.y + visibleRect.height, null);
    } else {
      target = calculateDrawImageRectangle(visibleRect);
      g.drawImage(image, target.x, target.y, target.x + target.width, target.y
          + target.height, visibleRect.x, visibleRect.y, visibleRect.x
          + visibleRect.width, visibleRect.y + visibleRect.height, null);
      if (this.selectedRect != null) {
        Point topLeft = img2compCoord(visibleRect, this.selectedRect.x,
            this.selectedRect.y);
        Point bottomRight = img2compCoord(visibleRect, this.selectedRect.x
            + this.selectedRect.width, this.selectedRect.y + this.selectedRect.height);
        g.setColor(new Color(128, 128, 128, 180));
        g.fillRect(target.x, target.y, target.width, topLeft.y - target.y);
        g.fillRect(target.x, target.y, topLeft.x - target.x, target.height);
        g.fillRect(bottomRight.x, target.y, target.x + target.width
            - bottomRight.x, target.height);
        g.fillRect(target.x, bottomRight.y, target.width, target.y
            + target.height - bottomRight.y);
        g.setColor(Color.black);
        g.drawRect(topLeft.x, topLeft.y, bottomRight.x - topLeft.x,
            bottomRight.y - topLeft.y);
      }
    }
  }

  private void paintDetections(Graphics g, Rectangle visibleRect) {
    if (g instanceof Graphics2D) {
      final Graphics2D g2d = (Graphics2D) g;
      g2d.setStroke(new BasicStroke(2));
      List<PointObjectLayer> detectionLayers = MainApplication.getLayerManager().getLayersOfType(PointObjectLayer.class);
      synchronized (detections) {
        if (pano) {
          paintPano(g2d, visibleRect, detectionLayers);
        } else {
          paintNonPano(g2d, visibleRect, detectionLayers);
        }
      }
    }
  }

  private void paintPano(Graphics2D g2d, Rectangle visibleRect, List<PointObjectLayer> detectionLayers) {
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

  private void paintNonPano(Graphics2D g2d, Rectangle visibleRect, List<PointObjectLayer> detectionLayers) {
    final Point upperLeft = img2compCoord(visibleRect, 0, 0);
    final Point lowerRight = img2compCoord(visibleRect, getImage().getWidth(), getImage().getHeight());
    final AffineTransform unit2CompTransform = AffineTransform.getTranslateInstance(upperLeft.getX(), upperLeft.getY());
    unit2CompTransform.concatenate(
        AffineTransform.getScaleInstance(lowerRight.getX() - upperLeft.getX(), lowerRight.getY() - upperLeft.getY()));

    for (final ImageDetection d : detections) {
      if (checkIfDetectionIsFiltered(detectionLayers, d))
        continue;
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

  private static boolean checkIfDetectionIsFiltered(List<PointObjectLayer> detectionLayers, ImageDetection d) {
    if ((Boolean.FALSE.equals(MapillaryProperties.SHOW_DETECTION_OUTLINES.get())
        && !"trafficsigns".contains(d.getPackage()))
        || (Boolean.FALSE.equals(MapillaryProperties.SHOW_DETECTED_SIGNS.get())
            && "trafficsigns".contains(d.getPackage())))
      return true;
    OsmPrimitive prim = detectionLayers.parallelStream().map(PointObjectLayer::getDataSet).flatMap(data -> data.allPrimitives().parallelStream()).filter(p -> p.hasKey("detections") && p.get("detections").contains(d.getKey())).findAny().orElse(null);
    return prim != null && prim.isDisabled();
  }

  private Point img2compCoord(Rectangle visibleRect, int xImg, int yImg) {
    Rectangle drawRect = calculateDrawImageRectangle(visibleRect);
    return new Point(drawRect.x + ((xImg - visibleRect.x) * drawRect.width)
        / visibleRect.width, drawRect.y
        + ((yImg - visibleRect.y) * drawRect.height) / visibleRect.height);
  }

  protected Point comp2imgCoord(Rectangle visibleRect, int xComp, int yComp) {
    Rectangle drawRect = calculateDrawImageRectangle(visibleRect);
    return new Point(
            visibleRect.x + ((xComp - drawRect.x) * visibleRect.width) / drawRect.width,
            visibleRect.y + ((yComp - drawRect.y) * visibleRect.height) / drawRect.height
    );
  }

  protected static Point getCenterImgCoord(Rectangle visibleRect) {
    return new Point(visibleRect.x + visibleRect.width / 2, visibleRect.y + visibleRect.height / 2);
  }

  protected Rectangle calculateDrawImageRectangle(Rectangle visibleRect) {
    return calculateDrawImageRectangle(visibleRect, new Rectangle(0, 0, getSize().width, getSize().height));
  }

  /**
   * calculateDrawImageRectangle
   *
   * @param imgRect
   *          the part of the image that should be drawn (in image coordinates)
   * @param compRect
   *          the part of the component where the image should be drawn (in
   *          component coordinates)
   * @return the part of compRect with the same width/height ratio as the image
   */
  private static Rectangle calculateDrawImageRectangle(Rectangle imgRect, Rectangle compRect) {
    int x = 0;
    int y = 0;
    int w = compRect.width;
    int h = compRect.height;
    int wFact = w * imgRect.height;
    int hFact = h * imgRect.width;
    if (wFact != hFact) {
      if (wFact > hFact) {
        w = hFact / imgRect.height;
        x = (compRect.width - w) / 2;
      } else {
        h = wFact / imgRect.width;
        y = (compRect.height - h) / 2;
      }
    }
    return new Rectangle(x + compRect.x, y + compRect.y, w, h);
  }

  /**
   * Zooms to 1:1 and, if it is already in 1:1, to best fit.
   */
  public void zoomBestFitOrOne() {
    Image zoomImage;
    Rectangle zoomVisibleRect;
    synchronized (this) {
      zoomImage = this.image;
      zoomVisibleRect = this.visibleRect;
    }
    if (zoomImage == null)
      return;
    if (zoomVisibleRect.width != zoomImage.getWidth(null)
        || zoomVisibleRect.height != zoomImage.getHeight(null)) {
      // The display is not at best fit. => Zoom to best fit
      zoomVisibleRect = new Rectangle(0, 0, zoomImage.getWidth(null),
          zoomImage.getHeight(null));
    } else {
      // The display is at best fit => zoom to 1:1
      Point center = getCenterImgCoord(zoomVisibleRect);
      zoomVisibleRect = new Rectangle(center.x - getWidth() / 2, center.y
          - getHeight() / 2, getWidth(), getHeight());
      checkVisibleRectPos(zoomImage, zoomVisibleRect);
    }
    synchronized (this) {
      this.visibleRect = zoomVisibleRect;
    }
    repaint();
  }

  protected static void checkVisibleRectPos(Image image, Rectangle visibleRect) {
    if (visibleRect.x < 0) {
      visibleRect.x = 0;
    }
    if (visibleRect.y < 0) {
      visibleRect.y = 0;
    }
    if (visibleRect.x + visibleRect.width > image.getWidth(null)) {
      visibleRect.x = image.getWidth(null) - visibleRect.width;
    }
    if (visibleRect.y + visibleRect.height > image.getHeight(null)) {
      visibleRect.y = image.getHeight(null) - visibleRect.height;
    }
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
      if (component instanceof MapillaryImageDisplay) {
        final MapillaryImageDisplay imgDisplay = (MapillaryImageDisplay) component;
        imgDisplay.offscreenImage = new BufferedImage(
            e.getComponent().getWidth(),
            e.getComponent().getHeight(),
            BufferedImage.TYPE_3BYTE_BGR
        );
        final double cameraPlaneDistance = (e.getComponent().getWidth() / 2d) / Math.tan(PANORAMA_FOV / 2);
        imgDisplay.cameraPlane = new CameraPlane(e.getComponent().getWidth(), e.getComponent().getHeight(), cameraPlaneDistance);
      }
    }
  }

  /**
   * @param detections The detections to set. Triggers a repaint.
   */
  public void setAllDetections(List<ImageDetection> detections) {
    this.detections.clear();
    if (detections != null) {
      this.detections.addAll(detections);
    }
    repaint();
  }
}
