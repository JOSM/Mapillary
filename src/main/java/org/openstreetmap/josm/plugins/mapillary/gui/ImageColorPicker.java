// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui;

import static org.openstreetmap.josm.tools.ColorHelper.color2html;
import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.datatransfer.StringSelection;
import java.awt.event.ItemEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.geom.Arc2D;
import java.awt.geom.Area;
import java.awt.geom.Ellipse2D;
import java.awt.image.BufferedImage;
import java.io.Serializable;

import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JToggleButton;
import javax.swing.SwingUtilities;

import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryImageDisplay.Painter;
import org.openstreetmap.josm.plugins.mapillary.gui.boilerplate.MapillaryButton;
import org.openstreetmap.josm.plugins.mapillary.gui.imageinfo.ClipboardAction;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.tools.GBC;
import org.openstreetmap.josm.tools.ImageProvider;

/**
 * UI to get color code from image.
 *
 * @author Kishan
 */
public class ImageColorPicker extends JPanel {
  private final JPanel colorPanel;
  private final JPanel tempColorPanel;
  private Color color;
  private Color tempColor;
  private static final int COLOR_AREA = 150;
  private final ClipboardAction copyAction;
  private final JLabel colorLabel;
  private EyeDropper eyeDropper;
  private boolean mouseIsDragging;
  private Point pointInComponent;
  private JToggleButton eyeDropperButton;
  private final Painter<Graphics, BufferedImage, Rectangle> dropperConsumer = new ImageColorPickerPainter();

  private Boolean detectionOutlines;
  private Boolean detectedSigns;

  public ImageColorPicker() {
    colorLabel = new JLabel(tr("No color selected"));
    colorPanel = new JPanel();
    colorPanel.setBorder(BorderFactory.createLineBorder(Color.BLACK));
    tempColorPanel = new JPanel();
    tempColorPanel.setBorder(BorderFactory.createLineBorder(Color.BLACK));
    colorPanel.setPreferredSize(new Dimension(COLOR_AREA, COLOR_AREA));
    tempColorPanel.setPreferredSize(new Dimension(COLOR_AREA, COLOR_AREA));

    copyAction = new ClipboardAction(tr("Copy color"), tr("Copied color to clipboard"), null);
    final MapillaryButton copyButton = new MapillaryButton(copyAction, true);
    copyAction.setPopupParent(copyButton);

    color = new Color(0, 0, 0);

    setupEyeDropperButton();

    JPanel panel = new JPanel(new GridBagLayout());

    panel.add(new JLabel(tr("Color")), GBC.eol());
    panel.add(colorPanel, GBC.std().insets(5).anchor(GridBagConstraints.LINE_START));
    panel.add(tempColorPanel, GBC.eol().insets(5).anchor(GridBagConstraints.LINE_START));
    panel.add(colorLabel, GBC.std().anchor(GridBagConstraints.LINE_START));
    panel.add(copyButton, GBC.eol().anchor(GridBagConstraints.LINE_START));
    panel.add(eyeDropperButton, GBC.std().anchor(GridBagConstraints.LINE_END));

    setLayout(new GridBagLayout());
    MapillaryMainDialog.getInstance().imageViewer.setPreferredSize(new Dimension(1000, 1000));
    add(MapillaryMainDialog.getInstance().imageViewer, GBC.std().insets(5).weight(1, 1).fill());
    add(panel, GBC.eol().insets(5));
  }

  protected void setColor(Color color, Color tempColor, Point p) {
    this.color = color;
    this.tempColor = tempColor;
    colorLabel.setText(color2html(color, false));
    copyAction.setContents(new StringSelection(color2html(color, false)));
    colorPanel.setBackground(color);
    tempColorPanel.setBackground(tempColor);
    pointInComponent = p;
  }

  public void addEyeDropper() {
    MapillaryImageDisplay imageViewer = MapillaryMainDialog.hasInstance()
      ? MapillaryMainDialog.getInstance().imageViewer
      : null;
    if (imageViewer != null) {
      imageViewer.setZoomPanEnabled(false);
      eyeDropper = new EyeDropper(imageViewer);
      imageViewer.addMouseListener(eyeDropper);
      imageViewer.addMouseMotionListener(eyeDropper);
      imageViewer.addAdditionalFunction(this.dropperConsumer);
      // TODO store current in local variables
      if (MapillaryProperties.SHOW_DETECTED_SIGNS.isSet()) {
        this.detectedSigns = MapillaryProperties.SHOW_DETECTED_SIGNS.get();
      }
      MainApplication.worker.execute(() -> MapillaryProperties.SHOW_DETECTED_SIGNS.put(false));
      if (MapillaryProperties.SHOW_DETECTION_OUTLINES.isSet()) {
        this.detectionOutlines = MapillaryProperties.SHOW_DETECTION_OUTLINES.get();
      }
      MainApplication.worker.execute(() -> MapillaryProperties.SHOW_DETECTION_OUTLINES.put(false));
    }
  }

  public void removeEyeDropper() {
    MapillaryImageDisplay imageViewer = MapillaryMainDialog.hasInstance()
      ? MapillaryMainDialog.getInstance().imageViewer
      : null;
    if (imageViewer != null && eyeDropper != null) {
      imageViewer.removeMouseListener(eyeDropper);
      imageViewer.removeMouseMotionListener(eyeDropper);
      imageViewer.setZoomPanEnabled(true);
      imageViewer.removeAdditionalFunction(this.dropperConsumer);
      // TODO use store from local variables
      if (this.detectedSigns != null) {
        MainApplication.worker.execute(() -> MapillaryProperties.SHOW_DETECTED_SIGNS.put(this.detectedSigns));
      } else {
        MainApplication.worker.execute(MapillaryProperties.SHOW_DETECTED_SIGNS::remove);
      }
      if (this.detectionOutlines != null) {
        MainApplication.worker.execute(() -> MapillaryProperties.SHOW_DETECTION_OUTLINES.put(this.detectionOutlines));
      } else {
        MainApplication.worker.execute(MapillaryProperties.SHOW_DETECTION_OUTLINES::remove);
      }
      this.eyeDropper = null;
    }
  }

  private void drawColorIndicator(Graphics2D g, BufferedImage i, Rectangle visibleRectangle, Point p, Color current,
    Color temp) {
    if (p == null || g == null) {
      return;
    }
    // Use floats instead of ints to avoid casting everywhere. Same size as int.
    final float r = 100;
    final float w = r / 5;

    Shape upperOuterArc = new Arc2D.Float(p.x - r, p.y - r, 2 * r, 2 * r, 0, 180, Arc2D.CHORD);
    Shape upperInnerArc = new Arc2D.Float(p.x - (r - w), p.y - (r - w), 2 * (r - w), 2 * (r - w), 0, 180, Arc2D.CHORD);
    Area upperRing = new Area(upperOuterArc);
    upperRing.subtract(new Area(upperInnerArc));

    Shape lowerOuterArc = new Arc2D.Float(p.x - r, p.y - r, 2 * r, 2 * r, 180, 180, Arc2D.CHORD);
    Shape lowerInnerArc = new Arc2D.Float(p.x - (r - w), p.y - (r - w), 2 * (r - w), 2 * (r - w), 180, 180,
      Arc2D.CHORD);
    Area lowerRing = new Area(lowerOuterArc);
    lowerRing.subtract(new Area(lowerInnerArc));

    Shape outerCircle = new Ellipse2D.Float(p.x - (r + w), p.y - (r + w), 2 * (r + w), 2 * (r + w));
    Shape innerCircle = new Ellipse2D.Float(p.x - r, p.y - r, 2 * r, 2 * r);
    Area outerRing = new Area(outerCircle);
    outerRing.subtract(new Area(innerCircle));

    g.setColor(current);
    g.fill(upperRing);
    g.setColor(temp);
    g.fill(lowerRing);
    g.setColor(Color.GRAY);
    g.draw(outerRing);
    g.fill(outerRing);
    if (this.eyeDropper != null) {
      this.eyeDropper.updateScreenshot(i, visibleRectangle);
    }
  }

  private void setupEyeDropperButton() {
    eyeDropperButton = new JToggleButton(ImageProvider.get("mapillary-eyedropper"));
    eyeDropperButton.addItemListener(e -> {
      if (e.getStateChange() == ItemEvent.SELECTED) {
        addEyeDropper();
      } else {
        removeEyeDropper();
      }
    });
  }

  public class EyeDropper implements MouseListener, MouseMotionListener, Serializable {

    private boolean inWindow;
    private transient BufferedImage screenShot;
    private Rectangle visibleRectangle;

    public EyeDropper(MapillaryImageDisplay panel) {
      this.updateScreenshot(panel.image, panel.visibleRect);
    }

    @Override
    public void mouseClicked(MouseEvent e) {
      if (getImage() != null) {
        Color c = new Color(getImage().getRGB(e.getX(), e.getY()));
        setColor(c, c, e.getPoint());
      }
      repaint();
    }

    @Override
    public void mousePressed(MouseEvent e) {
      if (getImage() != null && SwingUtilities.isLeftMouseButton(e)) {
        mouseIsDragging = true;
        Color c = new Color(getImage().getRGB(e.getX(), e.getY()));
        setColor(c, c, e.getPoint());
      } else {
        mouseIsDragging = false;
      }
      repaint();
    }

    @Override
    public void mouseReleased(MouseEvent e) {
      if (getImage() != null && SwingUtilities.isLeftMouseButton(e)) {
        if (inWindow && mouseIsDragging) {
          setColor(tempColor, tempColor, e.getPoint());
        }
        mouseIsDragging = false;
      }
      repaint();
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
    public void mouseDragged(MouseEvent e) {
      if (getImage() != null && SwingUtilities.isLeftMouseButton(e)) {
        if (inWindow) {
          mouseIsDragging = true;
          int buffer = 1;
          Point p = MapillaryMainDialog.getInstance().imageViewer.comp2imgCoord(visibleRectangle, e.getX(), e.getY());
          buffer = Math.min(buffer, getImage().getHeight() - p.y);
          buffer = Math.min(buffer, getImage().getWidth() - p.x);
          int[] surroundingPixels = getImage().getRGB(p.x, p.y, buffer, buffer, null, 0, buffer);
          if (surroundingPixels.length == 0)
            return;
          long redValue = 0;
          long greenValue = 0;
          long blueValue = 0;
          for (int rsb : surroundingPixels) {
            Color c = new Color(rsb);
            redValue += c.getRed();
            greenValue += c.getGreen();
            blueValue += c.getBlue();
          }
          int red = (int) (redValue / surroundingPixels.length);
          int green = (int) (greenValue / surroundingPixels.length);
          int blue = (int) (blueValue / surroundingPixels.length);
          Color c = new Color(red, green, blue);
          setColor(color, c, e.getPoint());
        } else {
          mouseIsDragging = false;
          return;
        }
      }
      repaint();
    }

    @Override
    public void mouseMoved(MouseEvent e) {
      // Do Nothing.
    }

    private BufferedImage getImage() {
      return this.screenShot;
    }

    void updateScreenshot(BufferedImage i, Rectangle visibleRectangle) {
      this.screenShot = i;
      this.visibleRectangle = visibleRectangle;
    }
  }

  /**
   * A class for painting the image color picker
   */
  private final class ImageColorPickerPainter implements Painter<Graphics, BufferedImage, Rectangle> {
    @Override
    public void accept(Graphics graphics, BufferedImage bufferedImage, Rectangle rectangle) {
      drawColorIndicator((Graphics2D) graphics, bufferedImage, rectangle, pointInComponent, color, tempColor);
    }
  }
}
