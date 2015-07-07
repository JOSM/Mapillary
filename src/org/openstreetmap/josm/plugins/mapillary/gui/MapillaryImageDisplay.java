package org.openstreetmap.josm.plugins.mapillary.gui;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;

import javax.swing.JComponent;
import javax.swing.JPanel;

import org.openstreetmap.josm.Main;

/**
 * This object is a responsible JComponent which lets you zoom and drag. It is
 * included in a {@link MapillaryMainDialog} object.
 * 
 * @author Jorge
 * @see MapillaryImageDisplay
 * @see MapillaryMainDialog
 */
public class MapillaryImageDisplay extends JComponent {

  private static final int DRAG_BUTTON = Main.pref.getInteger(
      "mapillary.picture-drag-button", 3);
  private static final int OPTION_BUTTON = Main.pref.getInteger(
      "mapillary.picture-option-button", 2);
  private static final int ZOOM_BUTTON = Main.pref.getInteger(
      "mapillary.picture-zoom-button", 1);

  /** The image currently displayed */
  private transient BufferedImage image = null;

  /**
   * The rectangle (in image coordinates) of the image that is visible. This
   * rectangle is calculated each time the zoom is modified
   */
  private Rectangle visibleRect = null;

  /**
   * When a selection is done, the rectangle of the selection (in image
   * coordinates)
   */
  private Rectangle selectedRect = null;

  public HyperlinkLabel hyperlink;

  private class ImgDisplayMouseListener implements MouseListener,
      MouseWheelListener, MouseMotionListener {
    private boolean mouseIsDragging = false;
    private long lastTimeForMousePoint = 0L;
    private Point mousePointInImg = null;

    /**
     * Zoom in and out, trying to preserve the point of the image that was under
     * the mouse cursor at the same place
     */
    @Override
    public void mouseWheelMoved(MouseWheelEvent e) {
      Image image;
      Rectangle visibleRect;
      synchronized (MapillaryImageDisplay.this) {
        image = MapillaryImageDisplay.this.image;
        visibleRect = MapillaryImageDisplay.this.visibleRect;
      }
      mouseIsDragging = false;
      selectedRect = null;
      if (image == null)
        return;
      // Calculate the mouse cursor position in image coordinates, so that
      // we can center the zoom
      // on that mouse position.
      // To avoid issues when the user tries to zoom in on the image
      // borders, this point is not calculated
      // again if there was less than 1.5seconds since the last event.
      if (e.getWhen() - lastTimeForMousePoint > 1500 || mousePointInImg == null) {
        lastTimeForMousePoint = e.getWhen();
        mousePointInImg = comp2imgCoord(visibleRect, e.getX(), e.getY());
      }
      // Applicate the zoom to the visible rectangle in image coordinates
      if (e.getWheelRotation() > 0) {
        visibleRect.width = visibleRect.width * 3 / 2;
        visibleRect.height = visibleRect.height * 3 / 2;
      } else {
        visibleRect.width = visibleRect.width * 2 / 3;
        visibleRect.height = visibleRect.height * 2 / 3;
      }
      // Check that the zoom doesn't exceed 2:1
      if (visibleRect.width < getSize().width / 2) {
        visibleRect.width = getSize().width / 2;
      }
      if (visibleRect.height < getSize().height / 2) {
        visibleRect.height = getSize().height / 2;
      }
      // Set the same ratio for the visible rectangle and the display area
      int hFact = visibleRect.height * getSize().width;
      int wFact = visibleRect.width * getSize().height;
      if (hFact > wFact) {
        visibleRect.width = hFact / getSize().height;
      } else {
        visibleRect.height = wFact / getSize().width;
      }
      // The size of the visible rectangle is limited by the image size.
      checkVisibleRectSize(image, visibleRect);
      // Set the position of the visible rectangle, so that the mouse
      // cursor doesn't move on the image.
      Rectangle drawRect = calculateDrawImageRectangle(visibleRect);
      visibleRect.x = mousePointInImg.x
          + ((drawRect.x - e.getX()) * visibleRect.width) / drawRect.width;
      visibleRect.y = mousePointInImg.y
          + ((drawRect.y - e.getY()) * visibleRect.height) / drawRect.height;
      // The position is also limited by the image size
      checkVisibleRectPos(image, visibleRect);
      synchronized (MapillaryImageDisplay.this) {
        MapillaryImageDisplay.this.visibleRect = visibleRect;
      }
      MapillaryImageDisplay.this.repaint();
    }

    /** Center the display on the point that has been clicked */
    @Override
    public void mouseClicked(MouseEvent e) {
      // Move the center to the clicked point.
      Image image;
      Rectangle visibleRect;
      synchronized (MapillaryImageDisplay.this) {
        image = MapillaryImageDisplay.this.image;
        visibleRect = MapillaryImageDisplay.this.visibleRect;
      }
      if (image == null)
        return;
      if (e.getButton() == OPTION_BUTTON) {
        if (!MapillaryImageDisplay.this.visibleRect.equals(new Rectangle(0, 0,
            image.getWidth(null), image.getHeight(null))))
          // Zooms to 1:1
          MapillaryImageDisplay.this.visibleRect = new Rectangle(0, 0,
              image.getWidth(null), image.getHeight(null));
        else
          // Zooms to best fit.
          MapillaryImageDisplay.this.visibleRect = new Rectangle(0,
              (image.getHeight(null) - (image.getWidth(null) * getHeight())
                  / getWidth()) / 2, image.getWidth(null),
              (image.getWidth(null) * getHeight()) / getWidth());
        MapillaryImageDisplay.this.repaint();
        return;
      } else if (e.getButton() != DRAG_BUTTON)
        return;
      // Calculate the translation to set the clicked point the center of
      // the view.
      Point click = comp2imgCoord(visibleRect, e.getX(), e.getY());
      Point center = getCenterImgCoord(visibleRect);
      visibleRect.x += click.x - center.x;
      visibleRect.y += click.y - center.y;
      checkVisibleRectPos(image, visibleRect);
      synchronized (MapillaryImageDisplay.this) {
        MapillaryImageDisplay.this.visibleRect = visibleRect;
      }
      MapillaryImageDisplay.this.repaint();
    }

    /**
     * Initialize the dragging, either with button 1 (simple dragging) or button
     * 3 (selection of a picture part)
     */
    @Override
    public void mousePressed(MouseEvent e) {
      if (image == null) {
        mouseIsDragging = false;
        selectedRect = null;
        return;
      }
      Image image;
      Rectangle visibleRect;
      synchronized (MapillaryImageDisplay.this) {
        image = MapillaryImageDisplay.this.image;
        visibleRect = MapillaryImageDisplay.this.visibleRect;
      }
      if (image == null)
        return;
      if (e.getButton() == DRAG_BUTTON) {
        mousePointInImg = comp2imgCoord(visibleRect, e.getX(), e.getY());
        mouseIsDragging = true;
        selectedRect = null;
      } else if (e.getButton() == ZOOM_BUTTON) {
        mousePointInImg = comp2imgCoord(visibleRect, e.getX(), e.getY());
        checkPointInVisibleRect(mousePointInImg, visibleRect);
        mouseIsDragging = false;
        selectedRect = new Rectangle(mousePointInImg.x, mousePointInImg.y, 0, 0);
        MapillaryImageDisplay.this.repaint();
      } else {
        mouseIsDragging = false;
        selectedRect = null;
      }
    }

    @Override
    public void mouseDragged(MouseEvent e) {
      if (!mouseIsDragging && selectedRect == null)
        return;
      Image image;
      Rectangle visibleRect;
      synchronized (MapillaryImageDisplay.this) {
        image = MapillaryImageDisplay.this.image;
        visibleRect = MapillaryImageDisplay.this.visibleRect;
      }
      if (image == null) {
        mouseIsDragging = false;
        selectedRect = null;
        return;
      }
      if (mouseIsDragging) {
        Point p = comp2imgCoord(visibleRect, e.getX(), e.getY());
        visibleRect.x += mousePointInImg.x - p.x;
        visibleRect.y += mousePointInImg.y - p.y;
        checkVisibleRectPos(image, visibleRect);
        synchronized (MapillaryImageDisplay.this) {
          MapillaryImageDisplay.this.visibleRect = visibleRect;
        }
        MapillaryImageDisplay.this.repaint();
      } else if (selectedRect != null) {
        Point p = comp2imgCoord(visibleRect, e.getX(), e.getY());
        checkPointInVisibleRect(p, visibleRect);
        Rectangle rect = new Rectangle(p.x < mousePointInImg.x ? p.x
            : mousePointInImg.x, p.y < mousePointInImg.y ? p.y
            : mousePointInImg.y, p.x < mousePointInImg.x ? mousePointInImg.x
            - p.x : p.x - mousePointInImg.x,
            p.y < mousePointInImg.y ? mousePointInImg.y - p.y : p.y
                - mousePointInImg.y);
        checkVisibleRectSize(image, rect);
        checkVisibleRectPos(image, rect);
        MapillaryImageDisplay.this.selectedRect = rect;
        MapillaryImageDisplay.this.repaint();
      }
    }

    @Override
    public void mouseReleased(MouseEvent e) {
      if (!mouseIsDragging && selectedRect == null)
        return;
      Image image;
      synchronized (MapillaryImageDisplay.this) {
        image = MapillaryImageDisplay.this.image;
      }
      if (image == null) {
        mouseIsDragging = false;
        selectedRect = null;
        return;
      }
      if (mouseIsDragging) {
        mouseIsDragging = false;
      } else if (selectedRect != null) {
        int oldWidth = selectedRect.width;
        int oldHeight = selectedRect.height;
        // Check that the zoom doesn't exceed 2:1
        if (selectedRect.width < getSize().width / 2) {
          selectedRect.width = getSize().width / 2;
        }
        if (selectedRect.height < getSize().height / 2) {
          selectedRect.height = getSize().height / 2;
        }
        // Set the same ratio for the visible rectangle and the display
        // area
        int hFact = selectedRect.height * getSize().width;
        int wFact = selectedRect.width * getSize().height;
        if (hFact > wFact) {
          selectedRect.width = hFact / getSize().height;
        } else {
          selectedRect.height = wFact / getSize().width;
        }
        // Keep the center of the selection
        if (selectedRect.width != oldWidth) {
          selectedRect.x -= (selectedRect.width - oldWidth) / 2;
        }
        if (selectedRect.height != oldHeight) {
          selectedRect.y -= (selectedRect.height - oldHeight) / 2;
        }
        checkVisibleRectSize(image, selectedRect);
        checkVisibleRectPos(image, selectedRect);
        synchronized (MapillaryImageDisplay.this) {
          MapillaryImageDisplay.this.visibleRect = selectedRect;
        }
        selectedRect = null;
        MapillaryImageDisplay.this.repaint();
      }
    }

    @Override
    public void mouseEntered(MouseEvent e) {
    }

    @Override
    public void mouseExited(MouseEvent e) {
    }

    @Override
    public void mouseMoved(MouseEvent e) {
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

  public MapillaryImageDisplay() {
    ImgDisplayMouseListener mouseListener = new ImgDisplayMouseListener();
    addMouseListener(mouseListener);
    addMouseWheelListener(mouseListener);
    addMouseMotionListener(mouseListener);
    this.setLayout(new BorderLayout());
    JPanel southPanel = new JPanel();
    southPanel.setLayout(new BorderLayout());
    hyperlink = new HyperlinkLabel();
    southPanel.add(hyperlink, BorderLayout.EAST);
    southPanel.setOpaque(false);

    add(southPanel, BorderLayout.SOUTH);
  }

  /**
   * Sets a new picture to be displayed.
   * 
   * @param image
   */
  public void setImage(BufferedImage image) {
    synchronized (this) {
      this.image = image;
      selectedRect = null;
      if (image != null)
        this.visibleRect = new Rectangle(0, 0, image.getWidth(null),
            image.getHeight(null));
    }
    repaint();
  }

  /**
   * Returns the picture that is being displayerd
   * 
   * @return
   */
  public BufferedImage getImage() {
    return this.image;
  }

  /**
   * Paints the visible part of the picture.
   */
  public void paintComponent(Graphics g) {
    Image image;
    Rectangle visibleRect;
    synchronized (this) {
      image = this.image;
      visibleRect = this.visibleRect;
    }
    if (image == null) {
      g.setColor(Color.black);
      String noImageStr = tr("No image");
      Rectangle2D noImageSize = g.getFontMetrics(g.getFont()).getStringBounds(
          noImageStr, g);
      Dimension size = getSize();
      g.drawString(noImageStr,
          (int) ((size.width - noImageSize.getWidth()) / 2),
          (int) ((size.height - noImageSize.getHeight()) / 2));
    } else {
      Rectangle target = calculateDrawImageRectangle(visibleRect);
      g.drawImage(image, target.x, target.y, target.x + target.width, target.y
          + target.height, visibleRect.x, visibleRect.y, visibleRect.x
          + visibleRect.width, visibleRect.y + visibleRect.height, null);
      if (selectedRect != null) {
        Point topLeft = img2compCoord(visibleRect, selectedRect.x,
            selectedRect.y);
        Point bottomRight = img2compCoord(visibleRect, selectedRect.x
            + selectedRect.width, selectedRect.y + selectedRect.height);
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

  private final Point img2compCoord(Rectangle visibleRect, int xImg, int yImg) {
    Rectangle drawRect = calculateDrawImageRectangle(visibleRect);
    return new Point(drawRect.x + ((xImg - visibleRect.x) * drawRect.width)
        / visibleRect.width, drawRect.y
        + ((yImg - visibleRect.y) * drawRect.height) / visibleRect.height);
  }

  private final Point comp2imgCoord(Rectangle visibleRect, int xComp, int yComp) {
    Rectangle drawRect = calculateDrawImageRectangle(visibleRect);
    return new Point(visibleRect.x + ((xComp - drawRect.x) * visibleRect.width)
        / drawRect.width, visibleRect.y
        + ((yComp - drawRect.y) * visibleRect.height) / drawRect.height);
  }

  private final Point getCenterImgCoord(Rectangle visibleRect) {
    return new Point(visibleRect.x + visibleRect.width / 2, visibleRect.y
        + visibleRect.height / 2);
  }

  private Rectangle calculateDrawImageRectangle(Rectangle visibleRect) {
    return calculateDrawImageRectangle(visibleRect, new Rectangle(0, 0,
        getSize().width, getSize().height));
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
  static Rectangle calculateDrawImageRectangle(Rectangle imgRect,
      Rectangle compRect) {
    int x, y, w, h;
    x = 0;
    y = 0;
    w = compRect.width;
    h = compRect.height;
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
    Image image;
    Rectangle visibleRect;
    synchronized (this) {
      image = MapillaryImageDisplay.this.image;
      visibleRect = MapillaryImageDisplay.this.visibleRect;
    }
    if (image == null)
      return;
    if (visibleRect.width != image.getWidth(null)
        || visibleRect.height != image.getHeight(null)) {
      // The display is not at best fit. => Zoom to best fit
      visibleRect = new Rectangle(0, 0, image.getWidth(null),
          image.getHeight(null));
    } else {
      // The display is at best fit => zoom to 1:1
      Point center = getCenterImgCoord(visibleRect);
      visibleRect = new Rectangle(center.x - getWidth() / 2, center.y
          - getHeight() / 2, getWidth(), getHeight());
      checkVisibleRectPos(image, visibleRect);
    }
    synchronized (this) {
      this.visibleRect = visibleRect;
    }
    repaint();
  }

  private final void checkVisibleRectPos(Image image, Rectangle visibleRect) {
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

  private void checkVisibleRectSize(Image image, Rectangle visibleRect) {
    if (visibleRect.width > image.getWidth(null)) {
      visibleRect.width = image.getWidth(null);
    }
    if (visibleRect.height > image.getHeight(null)) {
      visibleRect.height = image.getHeight(null);
    }
  }
}
