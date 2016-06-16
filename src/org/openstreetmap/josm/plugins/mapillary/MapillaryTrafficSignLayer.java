// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary;

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.awt.FontFormatException;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.RenderingHints;
import java.io.IOException;

import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.JLabel;

import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.data.osm.visitor.BoundingXYVisitor;
import org.openstreetmap.josm.gui.MapView;
import org.openstreetmap.josm.gui.layer.AbstractModifiableLayer;
import org.openstreetmap.josm.gui.layer.Layer;
import org.openstreetmap.josm.plugins.mapillary.traffico.TrafficoSign;
import org.openstreetmap.josm.tools.I18n;

public final class MapillaryTrafficSignLayer extends AbstractModifiableLayer {
  private static final String TRAFFICO_PATH = "/data/fonts/traffico/traffico.ttf";
  private static MapillaryTrafficSignLayer instance;
  private final Font traffico;

  private MapillaryTrafficSignLayer() throws IOException {
    super("Mapillary traffic signs");
    try {
      traffico = Font.createFont(
          Font.TRUETYPE_FONT,
          MapillaryTrafficSignLayer.class.getResourceAsStream(TRAFFICO_PATH)
      ).deriveFont(50.0f);
    } catch (FontFormatException e) {
      throw new IOException(I18n.tr("Traffic sign font at ''{0}'' has wrong format", TRAFFICO_PATH), e);
    } catch (IOException e) {
      throw new IOException(I18n.tr("Could not read font-file from ''{0}''", TRAFFICO_PATH), e);
    }
  }

  /**
   * Returns and when needed instantiates the Mapillary traffic sign layer.
   *
   * @return the only instance of the traffic sign layer
   * @throws IOException if some error occured while reading the icon-font traffico or
   *         if the traffico font has the wrong format
   */
  public static synchronized MapillaryTrafficSignLayer getInstance() throws IOException {
    if (instance == null) {
      instance = new MapillaryTrafficSignLayer();
    }
    return instance;
  }

  /*
   * (non-Javadoc)
   *
   * @see org.openstreetmap.josm.gui.layer.AbstractModifiableLayer#isModified()
   */
  @Override
  public boolean isModified() {
    return false;
  }

  /*
   * (non-Javadoc)
   *
   * @see org.openstreetmap.josm.gui.layer.Layer#paint(java.awt.Graphics2D,
   * org.openstreetmap.josm.gui.MapView, org.openstreetmap.josm.data.Bounds)
   */
  @Override
  public void paint(Graphics2D g, MapView mv, Bounds box) {
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
    g.setFont(traffico);

    Point[] points = new Point[3];
    points[0] = mv.getPoint(new LatLon(49.01058, 8.40743));
    points[1] = mv.getPoint(new LatLon(49.01116, 8.40679));
    points[2] = mv.getPoint(new LatLon(49.01038, 8.40636));

    TrafficoSign[] signs = {
        TrafficoSign.getSign("de", "bicycles-only"),
        TrafficoSign.getSign("de", "bus-stop"),
        TrafficoSign.getSign("de", "pedestrian-crossing") };

    for (int i = 0; i < signs.length && i < points.length; i++) {
      for (int j = 0; signs[i] != null && j < signs[i].getNumElements(); j++) {
        g.setColor(signs[i].getElement(j).getColor());
        g.drawString(Character.toString(signs[i].getElement(j).getGlyph()), points[i].x - 25, points[i].y + 25);
      }
    }

    // Start iterating the images
    g.setColor(Color.MAGENTA);
    for (MapillaryAbstractImage img : MapillaryLayer.getInstance().getData().getImages()) {
      if (img instanceof MapillaryImage) {
        g.fillOval(mv.getPoint(img.getMovingLatLon()).x - 3, mv.getPoint(img.getMovingLatLon()).y - 3, 6, 6);
        if (!((MapillaryImage) img).getSigns().isEmpty()) {
          Point imgLoc = mv.getPoint(img.getMovingLatLon());
          // TODO: Paint a sign from the image
        }
      }
    }
  }

  /*
   * (non-Javadoc)
   *
   * @see org.openstreetmap.josm.gui.layer.Layer#getIcon()
   */
  @Override
  public Icon getIcon() {
    return MapillaryPlugin.ICON16;
  }

  /*
   * (non-Javadoc)
   *
   * @see org.openstreetmap.josm.gui.layer.Layer#getToolTipText()
   */
  @Override
  public String getToolTipText() {
    return null;
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.openstreetmap.josm.gui.layer.Layer#mergeFrom(org.openstreetmap.josm
   * .gui.layer.Layer)
   */
  @Override
  public void mergeFrom(Layer from) {
    // Does nothing as this layer is not mergeable (see method isMergable(Layer))
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.openstreetmap.josm.gui.layer.Layer#isMergable(org.openstreetmap.josm
   * .gui.layer.Layer)
   */
  @Override
  public boolean isMergable(Layer other) {
    return false;
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.openstreetmap.josm.gui.layer.Layer#visitBoundingBox(org.openstreetmap
   * .josm.data.osm.visitor.BoundingXYVisitor)
   */
  @Override
  public void visitBoundingBox(BoundingXYVisitor v) {
    // TODO Auto-generated method stub
  }

  /*
   * (non-Javadoc)
   *
   * @see org.openstreetmap.josm.gui.layer.Layer#getInfoComponent()
   */
  @Override
  public Component getInfoComponent() {
    return new JLabel("Mapillary traffic sign layer");
  }

  /*
   * (non-Javadoc)
   *
   * @see org.openstreetmap.josm.gui.layer.Layer#getMenuEntries()
   */
  @Override
  public Action[] getMenuEntries() {
    return new Action[]{};
  }

}
