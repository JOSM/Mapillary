/**
 *
 */
package org.openstreetmap.josm.plugins.mapillary;

import java.awt.Color;
import java.awt.Font;
import java.awt.FontFormatException;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.RenderingHints;
import java.io.File;
import java.io.IOException;

import javax.swing.Action;
import javax.swing.Icon;

import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.data.osm.visitor.BoundingXYVisitor;
import org.openstreetmap.josm.gui.MapView;
import org.openstreetmap.josm.gui.layer.AbstractModifiableLayer;
import org.openstreetmap.josm.gui.layer.Layer;
import org.openstreetmap.josm.plugins.mapillary.traffico.TrafficoSign;
import org.openstreetmap.josm.plugins.mapillary.traffico.TrafficoSignElement;

/**
 *
 */
public class MapillaryTrafficSignLayer extends AbstractModifiableLayer {
  private static MapillaryTrafficSignLayer instance;

  /**
   * Returns and when needed instantiates the Mapillary traffic sign layer.
   * 
   * @return the only instance of the traffic sign layer
   */
  public static MapillaryTrafficSignLayer getInstance() {
    return instance == null ? (instance = new MapillaryTrafficSignLayer())
        : instance;
  }

  /**
   * @param name
   */
  private MapillaryTrafficSignLayer() {
    super("Mapillary traffic signs");
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
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
        RenderingHints.VALUE_ANTIALIAS_ON);
    try {
      g.setFont(Font.createFont(Font.TRUETYPE_FONT,
          new File("data/fonts/traffico/traffico.ttf")).deriveFont(50.0f));
    } catch (FontFormatException e) {
      e.printStackTrace();
    } catch (IOException e) {
      e.printStackTrace();
    }

    Point[] points = new Point[3];
    points[0] = mv.getPoint(new LatLon(49.01058, 8.40743));
    points[1] = mv.getPoint(new LatLon(49.01116, 8.40679));
    points[2] = mv.getPoint(new LatLon(49.01038, 8.40636));

    TrafficoSignElement[][] signs = {
        TrafficoSign.getSign("europe", "mandatory_cycle_track"),
        TrafficoSign.getSign("de", "information_bus_stop"),
        TrafficoSign.getSign("europe", "information_pedestrian_crossing") };

    for (int i = 0; i < signs.length && i < points.length; i++) {
      for (TrafficoSignElement layer : signs[i]) { // TODO: NPE
        g.setColor(layer.getColor());
        g.drawString("" + layer.getGlyph(), points[i].x - 25, points[i].y + 25);
      }
    }

    // Start iterating the images
    g.setColor(Color.MAGENTA);
    for (MapillaryAbstractImage img : MapillaryLayer.getInstance()
        .getData().getImages()) {
      if (img instanceof MapillaryImage) {
        g.fillOval(mv.getPoint(img.getLatLon()).x - 3,
            mv.getPoint(img.getLatLon()).y - 3, 6, 6);
        if (((MapillaryImage) img).getSigns().size() >= 1) {
          Point imgLoc = mv.getPoint(img.getLatLon());
          for (TrafficoSignElement e : TrafficoSign.getSign("de",
              ((MapillaryImage) img).getSigns().get(0))) {
            g.setColor(e.getColor());
            g.drawString("" + e.getGlyph(), imgLoc.x, imgLoc.y);
          }
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
    // Does nothing as this layer is not mergeable (see method
    // isMergable(Layer))
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
  public Object getInfoComponent() {
    // TODO Auto-generated method stub
    return null;
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.openstreetmap.josm.gui.layer.Layer#getMenuEntries()
   */
  @Override
  public Action[] getMenuEntries() {
    // TODO Auto-generated method stub
    return null;
  }

}
