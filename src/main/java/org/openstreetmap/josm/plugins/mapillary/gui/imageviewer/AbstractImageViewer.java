// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.imageviewer;

import java.awt.BasicStroke;
import java.awt.Color;

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import javax.swing.JPanel;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.layer.LayerManager;

import org.openstreetmap.josm.plugins.mapillary.actions.MapillaryDownloadAction;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.PointObjectLayer;
import org.openstreetmap.josm.plugins.mapillary.model.ImageDetection;
import org.openstreetmap.josm.plugins.mapillary.utils.ImageViewUtil;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryColorScheme;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import static org.openstreetmap.josm.tools.I18n.tr;

/**
 *
 * @author Kishan
 */
public abstract class AbstractImageViewer extends JPanel {

  protected BufferedImage image;
  protected BufferedImage displayImage;
  /**
   * The rectangle (in image coordinates) of the image that is visible.
   * This rectangle is calculated each time the zoom is modified.
   */
  volatile Rectangle visibleRect;
  protected final Collection<ImageDetection> detections = Collections.synchronizedList(new ArrayList<>());
  private final ZoomPanMouseListener zoomPanMouseListener;
  private boolean zoomPanEnabled;

  public AbstractImageViewer() {
    zoomPanMouseListener = new ZoomPanMouseListener(this);
    addMouseListener(zoomPanMouseListener);
    addMouseWheelListener(zoomPanMouseListener);
    addMouseMotionListener(zoomPanMouseListener);
    zoomPanEnabled = true;

    setOpaque(true);
    setDarkMode(MapillaryProperties.DARK_MODE.get());
    MainApplication.getLayerManager().addLayerChangeListener(new LayerManager.LayerChangeListener() {
      @Override
      public void layerAdded(LayerManager.LayerAddEvent e) { }

      @Override
      public void layerRemoving(LayerManager.LayerRemoveEvent e) {
        if (e.getRemovedLayer() instanceof MapillaryLayer) {
          setImage(null, Collections.emptyList());
        }
      }

      @Override
      public void layerOrderChanged(LayerManager.LayerOrderChangeEvent e) { }
    });
    MapillaryProperties.SHOW_DETECTION_OUTLINES.addListener(it -> repaint());
    MapillaryProperties.SHOW_DETECTED_SIGNS.addListener(it -> repaint());
    MapillaryProperties.DARK_MODE.addListener(it -> setDarkMode(it.getProperty().get()));
  }

  protected void setImage(BufferedImage image) {
      this.image = image;
  }

  public void setImage(BufferedImage image, Collection<ImageDetection> detections) {
    synchronized (this) {
      setImage(image);
      this.detections.clear();
      if (detections != null) {
        this.detections.addAll(detections);
      }
      this.visibleRect = getDefaultVisibleRect();
    }
    repaint();
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

  public abstract Rectangle getDefaultVisibleRect();

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
    final Collection<ImageDetection> imageDetections;
    synchronized (this) {
      bufferedImage = this.image;
      paintVisibleRect = this.visibleRect;
      imageDetections = this.detections;
    }
    if (bufferedImage == null) {
      paintNoImage(g);
    } else {
      paintImage(g, bufferedImage, paintVisibleRect);
      if (!imageDetections.isEmpty()) {
        paintDetections(g, paintVisibleRect);
      }
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

  public void paintLoadingImage() {
    if (getGraphics() != null) {
      paintLoadingImage(getGraphics());
    }
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

  private void paintDetections(Graphics g, Rectangle visibleRect) {
    if (g instanceof Graphics2D) {
      final Graphics2D g2d = (Graphics2D) g;
      g2d.setStroke(new BasicStroke(2));
      List<PointObjectLayer> detectionLayers = MainApplication.getLayerManager().getLayersOfType(PointObjectLayer.class);
      synchronized (detections) {
        paintDetections(g2d, visibleRect, detectionLayers);
      }
    }
  }

  protected abstract void paintDetections(Graphics2D g2d, Rectangle visibleRect, List<PointObjectLayer> detectionLayers);

  protected void checkAspectRatio(Rectangle zoomedRectangle) {
    int hFact = zoomedRectangle.height * getSize().width;
    int wFact = zoomedRectangle.width * getSize().height;
    if (hFact > wFact) {
      zoomedRectangle.width = hFact / getSize().height;
    } else {
      zoomedRectangle.height = wFact / getSize().width;
    }
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
    if (zoomImage == null) {
      return;
    }
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
      ImageViewUtil.checkVisibleRectPos(zoomImage, zoomVisibleRect);
    }
    synchronized (this) {
      this.visibleRect = zoomVisibleRect;
    }
    repaint();
  }

  protected static Point getCenterImgCoord(Rectangle visibleRect) {
    return new Point(visibleRect.x + visibleRect.width / 2, visibleRect.y + visibleRect.height / 2);
  }

  protected Point comp2imgCoord(Rectangle visibleRect, int xComp, int yComp) {
    Rectangle drawRect = calculateDrawImageRectangle(visibleRect);
    return new Point(
      visibleRect.x + ((xComp - drawRect.x) * visibleRect.width) / drawRect.width,
      visibleRect.y + ((yComp - drawRect.y) * visibleRect.height) / drawRect.height
    );
  }

  protected Point img2compCoord(Rectangle visibleRect, int xImg, int yImg) {
    Rectangle drawRect = calculateDrawImageRectangle(visibleRect);
    return new Point(drawRect.x + ((xImg - visibleRect.x) * drawRect.width)
      / visibleRect.width, drawRect.y
      + ((yImg - visibleRect.y) * drawRect.height) / visibleRect.height);
  }

  protected Rectangle calculateDrawImageRectangle(Rectangle visibleRect) {
    return ImageViewUtil.calculateDrawImageRectangle(visibleRect, new Rectangle(0, 0, getSize().width, getSize().height));
  }

  private void setDarkMode(final boolean darkMode) {
    setBackground(darkMode ? MapillaryColorScheme.TOOLBAR_DARK_GREY : getBackground());
    setForeground(darkMode ? Color.LIGHT_GRAY : getForeground());
  }

  /**
   * Check that the Zoom is not greater than 2:1
   *
   * @param zoomedRectangle
   */
  protected void checkZoom(Rectangle zoomedRectangle) {
    if (zoomedRectangle.width < getSize().width / 2) {
      zoomedRectangle.width = getSize().width / 2;
    }
    if (zoomedRectangle.height < getSize().height / 2) {
      zoomedRectangle.height = getSize().height / 2;
    }
  }

  protected abstract void paintImage(Graphics g, BufferedImage image, Rectangle visibleRect);

  /**
   * Method for zooming.
   * @param zoomCenterX The x coordinate in component where zoomed
   * @param zoomCenterY The y coordinate in component where zoomed
   * @param zoomedIn true if zoomed inwards else false
   */
  public abstract void zoom(int zoomCenterX, int zoomCenterY, boolean zoomedIn);

  /**
   * Start dragging Image.
   * @param p The point in component.
   */
  public abstract void startPanning(Point p);

  /**
   * Drag Image to this point
   * @param p The point in component.
   */
  public abstract void pan(Point p);

  public abstract void viewSizeChanged();

}
