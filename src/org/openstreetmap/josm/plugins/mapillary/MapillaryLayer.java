package org.openstreetmap.josm.plugins.mapillary;

import static org.openstreetmap.josm.tools.I18n.tr;
import static org.openstreetmap.josm.tools.I18n.marktr;

import org.openstreetmap.josm.plugins.mapillary.cache.CacheUtils;
import org.openstreetmap.josm.plugins.mapillary.downloads.MapillaryDownloader;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryFilterDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryMainDialog;
import org.openstreetmap.josm.plugins.mapillary.mode.AbstractMode;
import org.openstreetmap.josm.plugins.mapillary.mode.JoinMode;
import org.openstreetmap.josm.plugins.mapillary.mode.SelectMode;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;
import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.gui.layer.Layer;
import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.gui.MapView;
import org.openstreetmap.josm.gui.MapView.EditLayerChangeListener;
import org.openstreetmap.josm.gui.MapView.LayerChangeListener;
import org.openstreetmap.josm.gui.NavigatableComponent;
import org.openstreetmap.josm.gui.layer.AbstractModifiableLayer;
import org.openstreetmap.josm.gui.layer.OsmDataLayer;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.data.osm.visitor.BoundingXYVisitor;
import org.openstreetmap.josm.data.osm.visitor.paint.PaintColors;
import org.openstreetmap.josm.gui.dialogs.LayerListDialog;
import org.openstreetmap.josm.gui.dialogs.LayerListPopup;
import org.openstreetmap.josm.data.osm.event.PrimitivesAddedEvent;
import org.openstreetmap.josm.data.osm.event.PrimitivesRemovedEvent;
import org.openstreetmap.josm.data.osm.event.TagsChangedEvent;
import org.openstreetmap.josm.data.osm.event.NodeMovedEvent;
import org.openstreetmap.josm.data.osm.event.WayNodesChangedEvent;
import org.openstreetmap.josm.data.osm.event.RelationMembersChangedEvent;
import org.openstreetmap.josm.data.osm.event.AbstractDatasetChangedEvent;
import org.openstreetmap.josm.data.osm.event.DataChangedEvent;
import org.openstreetmap.josm.data.osm.event.DataSetListener;

import java.awt.AlphaComposite;
import java.awt.Color;
import java.awt.Composite;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.TexturePaint;
import java.awt.geom.AffineTransform;
import java.awt.geom.Area;
import java.awt.image.AffineTransformOp;
import java.awt.image.BufferedImage;

import javax.swing.ImageIcon;
import javax.swing.Action;
import javax.swing.Icon;

import java.util.List;
import java.util.ArrayList;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * This class represents the layer shown in JOSM. There can only exist one
 * instance of this object.
 *
 * @author nokutu
 *
 */
public class MapillaryLayer extends AbstractModifiableLayer implements
    DataSetListener, EditLayerChangeListener, LayerChangeListener {

  /** Maximum distance for the red/blue lines. */
  public final static int SEQUENCE_MAX_JUMP_DISTANCE = Main.pref.getInteger(
      "mapillary.sequence-max-jump-distance", 100);

  /** If the download is in semiautomatic during this object lifetime. */
  public boolean TEMP_SEMIAUTOMATIC = false;

  /** Unique instance of the class. */
  public static MapillaryLayer INSTANCE;
  /** The image pointed by the blue line. */
  public static MapillaryImage BLUE;
  /** The image pointed by the red line. */
  public static MapillaryImage RED;

  /** {@link MapillaryData} object that stores the database. */
  private final MapillaryData data;

  /** Mode of the layer. */
  public AbstractMode mode;

  private int highlightPointRadius = Main.pref.getInteger(
      "mappaint.highlight.radius", 7);
  private int highlightStep = Main.pref
      .getInteger("mappaint.highlight.step", 4);

  private volatile TexturePaint hatched;

  private MapillaryLayer() {
    super(tr("Mapillary Images"));
    this.data = new MapillaryData();
    this.data.bounds = new CopyOnWriteArrayList<>();
    init();
  }

  /**
   * Initializes the Layer.
   */
  private void init() {
    if (INSTANCE == null)
      INSTANCE = this;
    if (Main.main != null && Main.map.mapView != null) {
      setMode(new SelectMode());
      Main.map.mapView.addLayer(this);
      MapView.addEditLayerChangeListener(this, false);
      MapView.addLayerChangeListener(this);
      if (Main.map.mapView.getEditLayer() != null)
        Main.map.mapView.getEditLayer().data.addDataSetListener(this);
      if (MapillaryDownloader.getMode() == MapillaryDownloader.AUTOMATIC)
        MapillaryDownloader.automaticDownload();
      if (MapillaryDownloader.getMode() == MapillaryDownloader.SEMIAUTOMATIC)
        this.mode.zoomChanged();
    }
    if (MapillaryPlugin.EXPORT_MENU != null) { // Does not execute when in
                                               // headless mode
      MapillaryPlugin.setMenuEnabled(MapillaryPlugin.EXPORT_MENU, true);
      if (!MapillaryMainDialog.getInstance().isShowing())
        MapillaryMainDialog.getInstance().getButton().doClick();
    }
    createHatchTexture();
    if (Main.main != null)
      MapillaryData.dataUpdated();
  }

  /**
   * Changes the mode the the given one.
   *
   * @param mode
   *          The mode that is going to be activated.
   */
  public void setMode(AbstractMode mode) {
    if (this.mode != null) {
      Main.map.mapView.removeMouseListener(this.mode);
      Main.map.mapView.removeMouseMotionListener(this.mode);
      NavigatableComponent.removeZoomChangeListener(this.mode);
    }
    this.mode = mode;
    if (mode != null) {
      Main.map.mapView.setNewCursor(mode.cursor, this);
      Main.map.mapView.addMouseListener(mode);
      Main.map.mapView.addMouseMotionListener(mode);
      NavigatableComponent.addZoomChangeListener(mode);
      MapillaryUtils.updateHelpText();
    }
  }

  /**
   * Returns the unique instance of this class.
   *
   * @return The unique instance of this class.
   */
  public synchronized static MapillaryLayer getInstance() {
    if (INSTANCE == null)
      return new MapillaryLayer();
    return MapillaryLayer.INSTANCE;
  }

  /**
   * Returns the {@link MapillaryData} object, which acts as the database of the
   * Layer.
   *
   * @return The {@link MapillaryData} object that stores the database.
   */
  public MapillaryData getData() {
    return this.data;
  }

  @Override
  public void destroy() {
    setMode(null);
    AbstractMode.resetThread();
    MapillaryDownloader.stopAll();
    MapillaryMainDialog.getInstance().setImage(null);
    MapillaryMainDialog.getInstance().updateImage();
    MapillaryPlugin.setMenuEnabled(MapillaryPlugin.EXPORT_MENU, false);
    MapillaryPlugin.setMenuEnabled(MapillaryPlugin.ZOOM_MENU, false);
    Main.map.mapView.removeMouseListener(this.mode);
    Main.map.mapView.removeMouseMotionListener(this.mode);
    MapView.removeEditLayerChangeListener(this);
    if (Main.map.mapView.getEditLayer() != null)
      Main.map.mapView.getEditLayer().data.removeDataSetListener(this);
    clearInstance();
    super.destroy();
  }

  /**
   * Clears the unique instance of this class.
   */
  public static void clearInstance() {
    INSTANCE = null;
  }

  /**
   * Zooms to fit all the {@link MapillaryAbstractImage} icons into the map
   * view.
   */
  public void showAllPictures() {
    double minLat = 90;
    double minLon = 180;
    double maxLat = -90;
    double maxLon = -180;
    for (MapillaryAbstractImage img : this.data.getImages()) {
      if (img.getLatLon().lat() < minLat)
        minLat = img.getLatLon().lat();
      if (img.getLatLon().lon() < minLon)
        minLon = img.getLatLon().lon();
      if (img.getLatLon().lat() > maxLat)
        maxLat = img.getLatLon().lat();
      if (img.getLatLon().lon() > maxLon)
        maxLon = img.getLatLon().lon();
    }
    Main.map.mapView.zoomTo(new Bounds(new LatLon(minLat, minLon), new LatLon(
        maxLat, maxLon)));
  }

  @Override
  public boolean isModified() {
    for (MapillaryAbstractImage image : this.data.getImages())
      if (image.isModified())
        return true;
    return false;
  }

  @Override
  public void setVisible(boolean visible) {
    super.setVisible(visible);
    for (MapillaryAbstractImage img : this.data.getImages())
      img.setVisible(visible);
    MapillaryFilterDialog.getInstance().refresh();
  }

  /**
   * Replies background color for downloaded areas.
   *
   * @return background color for downloaded areas. Black by default.
   */
  private static Color getBackgroundColor() {
    return Main.pref.getColor(marktr("background"), Color.BLACK);
  }

  /**
   * Replies background color for non-downloaded areas.
   *
   * @return background color for non-downloaded areas. Yellow by default.
   */
  private static Color getOutsideColor() {
    return Main.pref.getColor(marktr("outside downloaded area"), Color.YELLOW);
  }

  /**
   * Initialize the hatch pattern used to paint the non-downloaded area.
   */
  private void createHatchTexture() {
    BufferedImage bi = new BufferedImage(15, 15, BufferedImage.TYPE_INT_ARGB);
    Graphics2D big = bi.createGraphics();
    big.setColor(getBackgroundColor());
    Composite comp = AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.3f);
    big.setComposite(comp);
    big.fillRect(0, 0, 15, 15);
    big.setColor(getOutsideColor());
    big.drawLine(0, 15, 15, 0);
    Rectangle r = new Rectangle(0, 0, 15, 15);
    this.hatched = new TexturePaint(bi, r);
  }

  @Override
  public synchronized void paint(Graphics2D g, MapView mv, Bounds box) {
    if (Main.map.mapView.getActiveLayer() == this) {
      Rectangle b = mv.getBounds();
      // on some platforms viewport bounds seem to be offset from the left,
      // over-grow it just to be sure
      b.grow(100, 100);
      Area a = new Area(b);
      // now successively subtract downloaded areas
      for (Bounds bounds : this.data.bounds) {
        Point p1 = mv.getPoint(bounds.getMin());
        Point p2 = mv.getPoint(bounds.getMax());
        Rectangle r = new Rectangle(Math.min(p1.x, p2.x), Math.min(p1.y, p2.y),
            Math.abs(p2.x - p1.x), Math.abs(p2.y - p1.y));
        a.subtract(new Area(r));
      }
      // paint remainder
      g.setPaint(this.hatched);
      g.fill(a);
    }

    // Draw colored lines
    MapillaryLayer.BLUE = null;
    MapillaryLayer.RED = null;
    MapillaryMainDialog.getInstance().blueButton.setEnabled(false);
    MapillaryMainDialog.getInstance().redButton.setEnabled(false);

    // Sets blue and red lines and enables/disables the buttons
    if (this.data.getSelectedImage() != null) {
      MapillaryImage[] closestImages = getClosestImagesFromDifferentSequences();
      Point selected = mv.getPoint(this.data.getSelectedImage().getLatLon());
      if (closestImages[0] != null) {
        MapillaryLayer.BLUE = closestImages[0];
        g.setColor(Color.BLUE);
        g.drawLine(mv.getPoint(closestImages[0].getLatLon()).x,
            mv.getPoint(closestImages[0].getLatLon()).y, selected.x, selected.y);
        MapillaryMainDialog.getInstance().blueButton.setEnabled(true);
      }
      if (closestImages[1] != null) {
        MapillaryLayer.RED = closestImages[1];
        g.setColor(Color.RED);
        g.drawLine(mv.getPoint(closestImages[1].getLatLon()).x,
            mv.getPoint(closestImages[1].getLatLon()).y, selected.x, selected.y);
        MapillaryMainDialog.getInstance().redButton.setEnabled(true);
      }
    }
    g.setColor(Color.WHITE);
    for (MapillaryAbstractImage imageAbs : this.data.getImages()) {
      if (!imageAbs.isVisible())
        continue;
      Point p = mv.getPoint(imageAbs.getLatLon());

      Point nextp = null;
      // Draw sequence line
      if (imageAbs.getSequence() != null) {
        MapillaryAbstractImage tempImage = imageAbs.next();
        while (tempImage != null) {
          if (tempImage.isVisible()) {
            nextp = mv.getPoint(tempImage.getLatLon());
            break;
          }
          tempImage = tempImage.next();
        }
        if (nextp != null)
          g.drawLine(p.x, p.y, nextp.x, nextp.y);
      }

      if (imageAbs instanceof MapillaryImage) {
        MapillaryImage image = (MapillaryImage) imageAbs;
        ImageIcon icon;
        if (!this.data.getMultiSelectedImages().contains(image))
          icon = MapillaryPlugin.MAP_ICON;
        else
          icon = MapillaryPlugin.MAP_ICON_SELECTED;
        draw(g, image, icon, p);
        if (!image.getSigns().isEmpty()) {
          g.drawImage(MapillaryPlugin.MAP_SIGN.getImage(),
              p.x + icon.getIconWidth() / 2, p.y - icon.getIconHeight() / 2,
              Main.map.mapView);
        }
      } else if (imageAbs instanceof MapillaryImportedImage) {
        MapillaryImportedImage image = (MapillaryImportedImage) imageAbs;
        ImageIcon icon;
        if (!this.data.getMultiSelectedImages().contains(image))
          icon = MapillaryPlugin.MAP_ICON_IMPORTED;
        else
          icon = MapillaryPlugin.MAP_ICON_SELECTED;
        draw(g, image, icon, p);
      }
    }
    if (this.mode instanceof JoinMode) {
      this.mode.paint(g, mv, box);
    }
  }

  /**
   * Draws the highlight of the icon.
   *
   * @param g
   * @param p
   * @param size
   */
  private void drawPointHighlight(Graphics2D g, Point p, int size) {
    Color oldColor = g.getColor();
    Color highlightColor = PaintColors.HIGHLIGHT.get();
    Color highlightColorTransparent = new Color(highlightColor.getRed(),
        highlightColor.getGreen(), highlightColor.getBlue(), 100);
    g.setColor(highlightColorTransparent);
    int s = size + this.highlightPointRadius;
    while (s >= size) {
      int r = (int) Math.floor(s / 2d);
      g.fillRoundRect(p.x - r, p.y - r, s, s, r, r);
      s -= this.highlightStep;
    }
    g.setColor(oldColor);
  }

  /**
   * Draws the given icon of an image. Also checks if the mouse is over the
   * image.
   *
   * @param g
   * @param image
   * @param icon
   * @param p
   */
  private void draw(Graphics2D g, MapillaryAbstractImage image, ImageIcon icon,
      Point p) {
    Image imagetemp = icon.getImage();
    BufferedImage bi = (BufferedImage) imagetemp;
    int width = icon.getIconWidth();
    int height = icon.getIconHeight();

    // Rotate the image
    double rotationRequired = Math.toRadians(image.getCa());
    double locationX = width / 2;
    double locationY = height / 2;
    AffineTransform tx = AffineTransform.getRotateInstance(rotationRequired,
        locationX, locationY);
    AffineTransformOp op = new AffineTransformOp(tx,
        AffineTransformOp.TYPE_BILINEAR);

    g.drawImage(op.filter(bi, null), p.x - (width / 2), p.y - (height / 2),
        Main.map.mapView);
    if (this.data.getHighlightedImage() == image) {
      drawPointHighlight(g, p, 16);
    }
  }

  @Override
  public Icon getIcon() {
    return MapillaryPlugin.ICON16;
  }

  @Override
  public boolean isMergable(Layer other) {
    return false;
  }

  @Override
  public void mergeFrom(Layer from) {
    throw new UnsupportedOperationException(
        "This layer does not support merging yet");
  }

  @Override
  public Action[] getMenuEntries() {
    List<Action> actions = new ArrayList<>();
    actions.add(LayerListDialog.getInstance().createShowHideLayerAction());
    actions.add(LayerListDialog.getInstance().createDeleteLayerAction());
    actions.add(new LayerListPopup.InfoAction(this));
    return actions.toArray(new Action[actions.size()]);
  }

  /**
   * Returns the 2 closest images belonging to a different sequence.
   *
   * @return An array of length 2 containing the two closest images belonging to
   *         different sequences.
   */
  private MapillaryImage[] getClosestImagesFromDifferentSequences() {
    if (!(this.data.getSelectedImage() instanceof MapillaryImage))
      return new MapillaryImage[2];
    MapillaryImage selected = (MapillaryImage) this.data.getSelectedImage();
    MapillaryImage[] ret = new MapillaryImage[2];
    double[] distances = { SEQUENCE_MAX_JUMP_DISTANCE,
        SEQUENCE_MAX_JUMP_DISTANCE };
    LatLon selectedCoords = this.data.getSelectedImage().getLatLon();
    for (MapillaryAbstractImage imagePrev : this.data.getImages()) {
      if (!(imagePrev instanceof MapillaryImage))
        continue;
      if (!imagePrev.isVisible())
        continue;
      MapillaryImage image = (MapillaryImage) imagePrev;
      if (image.getLatLon().greatCircleDistance(selectedCoords) < SEQUENCE_MAX_JUMP_DISTANCE
          && selected.getSequence() != image.getSequence()) {
        if ((ret[0] == null && ret[1] == null)
            || (image.getLatLon().greatCircleDistance(selectedCoords) < distances[0] && (ret[1] == null || image
                .getSequence() != ret[1].getSequence()))) {
          ret[0] = image;
          distances[0] = image.getLatLon().greatCircleDistance(selectedCoords);
        } else if ((ret[1] == null || image.getLatLon().greatCircleDistance(
            selectedCoords) < distances[1])
            && image.getSequence() != ret[0].getSequence()) {
          ret[1] = image;
          distances[1] = image.getLatLon().greatCircleDistance(selectedCoords);
        }
      }
    }
    // Predownloads the thumbnails
    if (ret[0] != null)
      CacheUtils.downloadPicture(ret[0]);
    if (ret[1] != null)
      CacheUtils.downloadPicture(ret[1]);
    return ret;
  }

  @Override
  public Object getInfoComponent() {
    StringBuilder sb = new StringBuilder();
    sb.append(tr("Mapillary layer"));
    sb.append("\n");
    sb.append(tr("Total images:"));
    sb.append(" ");
    sb.append(this.data.size());
    sb.append("\n");
    return sb.toString();
  }

  @Override
  public String getToolTipText() {
    return this.data.size() + " " + tr("images");
  }

  @Override
  public void editLayerChanged(OsmDataLayer oldLayer, OsmDataLayer newLayer) {
    if (oldLayer == null && newLayer != null) {
      newLayer.data.addDataSetListener(this);

    } else if (oldLayer != null && newLayer == null) {
      oldLayer.data.removeDataSetListener(this);
    }
  }

  @Override
  public void dataChanged(DataChangedEvent event) {
    // When more data is downloaded, a delayed update is thrown, in order to
    // wait for the data bounds to be set.
    Main.worker.submit(new delayedDownload());
  }

  private class delayedDownload extends Thread {

    @Override
    public void run() {
      try {
        sleep(1000);
      } catch (InterruptedException e) {
        Main.error(e);
      }
      MapillaryDownloader.automaticDownload();
    }
  }

  @Override
  public void primitivesAdded(PrimitivesAddedEvent event) {
    // Nothing
  }

  @Override
  public void primitivesRemoved(PrimitivesRemovedEvent event) {
    // Nothing
  }

  @Override
  public void tagsChanged(TagsChangedEvent event) {
    // Nothing
  }

  @Override
  public void nodeMoved(NodeMovedEvent event) {
    // Nothing
  }

  @Override
  public void wayNodesChanged(WayNodesChangedEvent event) {
    // Nothing
  }

  @Override
  public void relationMembersChanged(RelationMembersChangedEvent event) {
    // Nothing
  }

  @Override
  public void otherDatasetChange(AbstractDatasetChangedEvent event) {
    // Nothing
  }

  @Override
  public void visitBoundingBox(BoundingXYVisitor v) {
    // Nothing
  }

  @Override
  public void activeLayerChange(Layer oldLayer, Layer newLayer) {
    if (newLayer == this) {
      MapillaryUtils.updateHelpText();
      MapillaryPlugin.setMenuEnabled(MapillaryPlugin.JOIN_MENU, true);
    } else
      MapillaryPlugin.setMenuEnabled(MapillaryPlugin.JOIN_MENU, false);
  }

  @Override
  public void layerAdded(Layer newLayer) {
    // Nothing
  }

  @Override
  public void layerRemoved(Layer oldLayer) {
    // Nothing
  }
}
