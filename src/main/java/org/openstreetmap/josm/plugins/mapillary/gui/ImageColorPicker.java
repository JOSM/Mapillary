// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Point;
import java.awt.Shape;
import java.awt.datatransfer.StringSelection;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.geom.Arc2D;
import java.awt.geom.Area;
import java.awt.geom.Ellipse2D;
import java.awt.image.BufferedImage;
import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JToggleButton;
import javax.swing.SwingUtilities;
import org.openstreetmap.josm.plugins.mapillary.gui.boilerplate.MapillaryButton;
import org.openstreetmap.josm.plugins.mapillary.gui.imageinfo.ClipboardAction;
import org.openstreetmap.josm.plugins.mapillary.gui.imageviewer.AbstractImageViewer;
import org.openstreetmap.josm.plugins.mapillary.gui.imageviewer.MapillaryImageViewer;
import org.openstreetmap.josm.plugins.mapillary.gui.imageviewer.PanoramicImageViewer;
import static org.openstreetmap.josm.tools.ColorHelper.color2html;
import org.openstreetmap.josm.tools.GBC;
import org.openstreetmap.josm.tools.ImageProvider;

/**
 * UI to get color code from image.
 *
 * @author Kishan
 */
public class ImageColorPicker extends JPanel {

  private AbstractImageViewer imageViewer;
  private final JPanel colorPanel;
  private final JPanel tempColorPanel;
  private Color color;
  private Color tempColor;
  private static int colorArea = 150;
  private final ClipboardAction copyAction;
  private final JLabel colorLabel;
  private EyeDropper eyeDropper;
  private boolean mouseIsDragging = false;
  private Point pointInComponent;
  private JToggleButton eyeDropperButton;

  public ImageColorPicker() {
    setupImageViewer();

    colorLabel = new JLabel(tr("No color selected"));
    colorPanel = new JPanel();
    colorPanel.setBorder(BorderFactory.createLineBorder(Color.BLACK));
    tempColorPanel = new JPanel();
    tempColorPanel.setBorder(BorderFactory.createLineBorder(Color.BLACK));
    colorPanel.setPreferredSize(new Dimension(colorArea, colorArea));
    tempColorPanel.setPreferredSize(new Dimension(colorArea, colorArea));

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
    add(imageViewer, GBC.std().insets(5).weight(1, 1).fill());
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
    if (imageViewer != null) {
      imageViewer.disableZoomPan();
      eyeDropper = new EyeDropper(imageViewer);
      imageViewer.addMouseListener(eyeDropper);
      imageViewer.addMouseMotionListener(eyeDropper);
    }
  }

  public void removeEyeDropper() {
    if (imageViewer != null && eyeDropper != null) {
      imageViewer.removeMouseListener(eyeDropper);
      imageViewer.removeMouseMotionListener(eyeDropper);
      imageViewer.enableZoomPan();
    }
  }

  private void drawColorIndicator(Graphics2D g, Point p, Color current, Color temp) {
    int r = 100;
    int w = r / 5;

    Shape upperOuterArc = new Arc2D.Float(p.x - r, p.y - r, 2 * r, 2 * r, 0, 180, Arc2D.CHORD);
    Shape upperInnerArc = new Arc2D.Float(p.x - (r - w), p.y - (r - w), 2 * (r - w), 2 * (r - w), 0, 180, Arc2D.CHORD);
    Area upperRing = new Area(upperOuterArc);
    upperRing.subtract(new Area(upperInnerArc));

    Shape lowerOuterArc = new Arc2D.Float(p.x - r, p.y - r, 2 * r, 2 * r, 180, 180, Arc2D.CHORD);
    Shape lowerInnerArc = new Arc2D.Float(p.x - (r - w), p.y - (r - w), 2 * (r - w), 2 * (r - w), 180, 180, Arc2D.CHORD);
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
  }

  private void setupImageViewer() {
    if (MapillaryMainDialog.getInstance().imageViewer instanceof PanoramicImageViewer) {
      imageViewer = new PanoramicImageViewer(){
        @Override
        public void paintComponent(Graphics g) {
          super.paintComponent(g);
          if (mouseIsDragging)
            drawColorIndicator((Graphics2D) g, pointInComponent, color, tempColor);
        }
      };
    } else {
      imageViewer = new MapillaryImageViewer(){
        @Override
        public void paintComponent(Graphics g) {
          super.paintComponent(g);
          if (mouseIsDragging)
            drawColorIndicator((Graphics2D) g, pointInComponent, color, tempColor);
        }
      };
    }
    imageViewer.setImage(MapillaryMainDialog.getInstance().imageViewer.getImage(), null);
    imageViewer.setPreferredSize(new Dimension(1000, 1000));
  }

  private void setupEyeDropperButton() {
    eyeDropperButton = new JToggleButton(ImageProvider.get("mapillary-eyedropper"));
    ItemListener itemListener = new ItemListener() {
      @Override
      public void itemStateChanged(ItemEvent e) {
        if (e.getStateChange() == ItemEvent.SELECTED) {
          addEyeDropper();
        } else {
          removeEyeDropper();
        }
      }
    };
    eyeDropperButton.addItemListener(itemListener);
  }

  public class EyeDropper implements MouseListener, MouseMotionListener {

    private boolean inWindow = false;
    private BufferedImage screenShot;

    public EyeDropper(AbstractImageViewer panel) {
      if (panel != null && panel.getSize().getHeight() > 0 && panel.getSize().getWidth() > 0) {
        this.screenShot = new BufferedImage(panel.getWidth(), panel.getHeight(), BufferedImage.TYPE_INT_RGB);
        panel.paint(screenShot.getGraphics());
      }
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
          Color c = new Color(getImage().getRGB(e.getX(), e.getY()));
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
      //Do Nothing.
    }

    private BufferedImage getImage() {
      return screenShot;
    }
  }
}
