// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.imageviewer;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import javax.swing.JPanel;

import org.openstreetmap.josm.plugins.mapillary.actions.MapillaryDownloadAction;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.utils.ImageUtil;
import org.openstreetmap.josm.plugins.mapillary.utils.ImageViewUtil;
import static org.openstreetmap.josm.tools.I18n.tr;

/**
 *
 * @author Kishan
 */
public abstract class AbstractImageViewer extends JPanel {

  protected BufferedImage image;
  protected BufferedImage displayImage;
  /**
   * The rectangle (in image coordinates) of the image that is visible. This rectangle is calculated each time the zoom
   * is modified
   */
  volatile Rectangle visibleRect;

  public void setImage(BufferedImage image) {
    synchronized (this) {
      this.image = image;
      this.visibleRect = getDefaultVisibleRect();
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
    synchronized (this) {
      bufferedImage = this.image;
      paintVisibleRect = this.visibleRect;
    }
    if (bufferedImage == null) {
      paintNoImage(g);
    } else {
      paintImage(g, bufferedImage, paintVisibleRect);
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

  protected void checkAspectRatio(Rectangle zoomedRectangle) {
    int hFact = zoomedRectangle.height * getSize().width;
    int wFact = zoomedRectangle.width * getSize().height;
    if (hFact > wFact) {
      zoomedRectangle.width = hFact / getSize().height;
    } else {
      zoomedRectangle.height = wFact / getSize().width;
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

  protected Rectangle calculateDrawImageRectangle(Rectangle visibleRect) {
    return ImageViewUtil.calculateDrawImageRectangle(visibleRect, new Rectangle(0, 0, getSize().width, getSize().height));
  }

  abstract void paintImage(Graphics g, BufferedImage image, Rectangle visibleRect);

  public abstract void zoomOut(int zoomCenterX, int zoomCenterY);

  public abstract void zoomIn(int zoomCenterX, int zoomCenterY);

  public abstract void startPanning(Point p);

  public abstract void pan(Point p);

  public abstract void stopPanning();
}
