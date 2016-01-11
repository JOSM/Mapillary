// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary;

import static org.openstreetmap.josm.tools.I18n.marktr;
import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.AlphaComposite;
import java.awt.Color;
import java.awt.Composite;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.TexturePaint;
import java.awt.event.ActionEvent;
import java.awt.geom.AffineTransform;
import java.awt.geom.Area;
import java.awt.image.AffineTransformOp;
import java.awt.image.BufferedImage;
import java.util.concurrent.CopyOnWriteArrayList;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.KeyStroke;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.data.osm.event.AbstractDatasetChangedEvent;
import org.openstreetmap.josm.data.osm.event.DataChangedEvent;
import org.openstreetmap.josm.data.osm.event.DataSetListener;
import org.openstreetmap.josm.data.osm.event.NodeMovedEvent;
import org.openstreetmap.josm.data.osm.event.PrimitivesAddedEvent;
import org.openstreetmap.josm.data.osm.event.PrimitivesRemovedEvent;
import org.openstreetmap.josm.data.osm.event.RelationMembersChangedEvent;
import org.openstreetmap.josm.data.osm.event.TagsChangedEvent;
import org.openstreetmap.josm.data.osm.event.WayNodesChangedEvent;
import org.openstreetmap.josm.data.osm.visitor.BoundingXYVisitor;
import org.openstreetmap.josm.data.osm.visitor.paint.PaintColors;
import org.openstreetmap.josm.gui.MapView;
import org.openstreetmap.josm.gui.MapView.EditLayerChangeListener;
import org.openstreetmap.josm.gui.MapView.LayerChangeListener;
import org.openstreetmap.josm.gui.NavigatableComponent;
import org.openstreetmap.josm.gui.dialogs.LayerListDialog;
import org.openstreetmap.josm.gui.dialogs.LayerListPopup;
import org.openstreetmap.josm.gui.layer.AbstractModifiableLayer;
import org.openstreetmap.josm.gui.layer.Layer;
import org.openstreetmap.josm.gui.layer.OsmDataLayer;
import org.openstreetmap.josm.plugins.mapillary.cache.CacheUtils;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryFilterDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryMainDialog;
import org.openstreetmap.josm.plugins.mapillary.history.MapillaryRecord;
import org.openstreetmap.josm.plugins.mapillary.history.commands.CommandDelete;
import org.openstreetmap.josm.plugins.mapillary.io.download.MapillaryDownloader;
import org.openstreetmap.josm.plugins.mapillary.mode.AbstractMode;
import org.openstreetmap.josm.plugins.mapillary.mode.JoinMode;
import org.openstreetmap.josm.plugins.mapillary.mode.SelectMode;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;

/**
 * This class represents the layer shown in JOSM. There can only exist one
 * instance of this object.
 *
 * @author nokutu
 *
 */
public final class MapillaryLayer extends AbstractModifiableLayer implements
    DataSetListener, EditLayerChangeListener, LayerChangeListener {

  /** Maximum distance for the red/blue lines. */
  public static final int SEQUENCE_MAX_JUMP_DISTANCE = Main.pref.getInteger(
      "mapillary.sequence-max-jump-distance", 100);

  /** If the download is in semiautomatic during this object lifetime. */
  public boolean tempSemiautomatic;

  /** Unique instance of the class. */
  private static MapillaryLayer instance;
  /** The image pointed by the blue line. */
  private MapillaryImage blue;
  /** The image pointed by the red line. */
  private MapillaryImage red;
  /** {@link MapillaryData} object that stores the database. */
  private final MapillaryData data;

  /** Mode of the layer. */
  public AbstractMode mode;

  private final int highlightPointRadius = Main.pref.getInteger("mappaint.highlight.radius", 7);
  private final int highlightStep = Main.pref.getInteger("mappaint.highlight.step", 4);

  private volatile TexturePaint hatched;

  private MapillaryLayer() {
    super(tr("Mapillary Images"));
    this.data = new MapillaryData();
    this.data.bounds = new CopyOnWriteArrayList<>();
  }

  /**
   * Initializes the Layer.
   */
  private void init() {
    if (Main.main != null && Main.map.mapView != null) {
      setMode(new SelectMode());
      Main.map.mapView.addLayer(this);
      MapView.addEditLayerChangeListener(this, false);
      MapView.addLayerChangeListener(this);
      if (Main.map.mapView.getEditLayer() != null)
        Main.map.mapView.getEditLayer().data.addDataSetListener(this);
      if (MapillaryDownloader.getMode() == MapillaryDownloader.MODES.Automatic)
        MapillaryDownloader.automaticDownload();
      if (MapillaryDownloader.getMode() == MapillaryDownloader.MODES.Semiautomatic)
        this.mode.zoomChanged();
    }
    // Does not execute when in headless mode
    if (MapillaryPlugin.getExportMenu() != null) {
      MapillaryPlugin.setMenuEnabled(MapillaryPlugin.getExportMenu(), true);
      if (!MapillaryMainDialog.getInstance().isShowing())
        MapillaryMainDialog.getInstance().getButton().doClick();
    }
    createHatchTexture();
    if (Main.main != null) {
      MapillaryMainDialog.getInstance()
          .getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW)
          .put(KeyStroke.getKeyStroke("DELETE"), "MapillaryDel");
      MapillaryMainDialog.getInstance().getActionMap()
          .put("MapillaryDel", new DeleteImageAction());
    }

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
   * Clears the unique instance of this class.
   */
  public static void clearInstance() {
    instance = null;
  }

  /**
   * Returns the unique instance of this class.
   *
   * @return The unique instance of this class.
   */
  public static synchronized MapillaryLayer getInstance() {
    if (instance == null) {
      instance = new MapillaryLayer();
      instance.init();
    }
    return instance;
  }

  /**
   * @return if the unique instance of this layer is currently instantiated
   */
  public static boolean hasInstance() {
    return instance != null;
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

  /**
   * @return The image that is linked to the current image with a blue line
   */
  public MapillaryImage getBlue() {
    return blue;
  }

  /**
   * @return The image that is linked to the current image with a blue line
   */
  public MapillaryImage getRed() {
    return red;
  }

  @Override
  public void destroy() {
    setMode(null);
    MapillaryRecord.getInstance().reset();
    AbstractMode.resetThread();
    MapillaryDownloader.stopAll();
    MapillaryMainDialog.getInstance().setImage(null);
    MapillaryMainDialog.getInstance().updateImage();
    MapillaryPlugin.setMenuEnabled(MapillaryPlugin.getExportMenu(), false);
    MapillaryPlugin.setMenuEnabled(MapillaryPlugin.getZoomMenu(), false);
    Main.map.mapView.removeMouseListener(this.mode);
    Main.map.mapView.removeMouseMotionListener(this.mode);
    MapView.removeEditLayerChangeListener(this);
    if (Main.map.mapView.getEditLayer() != null)
      Main.map.mapView.getEditLayer().data.removeDataSetListener(this);
    clearInstance();
    super.destroy();
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
    for (MapillaryAbstractImage img : this.data.getImages()) {
      img.setVisible(visible);
    }
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
    blue = null;
    red = null;
    MapillaryMainDialog.getInstance().blueButton.setEnabled(false);
    MapillaryMainDialog.getInstance().redButton.setEnabled(false);

    // Sets blue and red lines and enables/disables the buttons
    if (this.data.getSelectedImage() != null) {
      MapillaryImage[] closestImages = getClosestImagesFromDifferentSequences();
      Point selected = mv.getPoint(this.data.getSelectedImage().getLatLon());
      if (closestImages[0] != null) {
        blue = closestImages[0];
        g.setColor(Color.BLUE);
        g.drawLine(mv.getPoint(closestImages[0].getLatLon()).x,
            mv.getPoint(closestImages[0].getLatLon()).y, selected.x, selected.y);
        MapillaryMainDialog.getInstance().blueButton.setEnabled(true);
      }
      if (closestImages[1] != null) {
        red = closestImages[1];
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
      // Draws icons
      if (imageAbs instanceof MapillaryImage) {
        MapillaryImage image = (MapillaryImage) imageAbs;
        ImageIcon icon;
        icon = this.data.getMultiSelectedImages().contains(image) ? MapillaryPlugin.MAP_ICON_SELECTED : MapillaryPlugin.MAP_ICON;
        draw(g, image, icon, p);
        if (!image.getSigns().isEmpty()) {
          g.drawImage(MapillaryPlugin.MAP_SIGN.getImage(),
              p.x + icon.getIconWidth() / 2, p.y - icon.getIconHeight() / 2,
              Main.map.mapView);
        }
      } else if (imageAbs instanceof MapillaryImportedImage) {
        MapillaryImportedImage image = (MapillaryImportedImage) imageAbs;
        ImageIcon icon = this.data.getMultiSelectedImages().contains(image)
            ? MapillaryPlugin.MAP_ICON_SELECTED
            : MapillaryPlugin.MAP_ICON_IMPORTED;
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
   * @param g  the graphics context
   * @param p  the {@link Point} where the image must be set.
   * @param size  the width in pixels of the highlight.
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
   * @param g  the graphics context
   * @param image  The {@link MapillaryAbstractImage} which is being drown.
   * @param icon  The {@link ImageIcon} that represents the image.
   * @param p  The P¡{@link Point} when the image lies.
   */
  private void draw(Graphics2D g, MapillaryAbstractImage image, ImageIcon icon, Point p) {
    Image imagetemp = icon.getImage();
    BufferedImage bi = (BufferedImage) imagetemp;
    int width = icon.getIconWidth();
    int height = icon.getIconHeight();

    // Rotate the image
    double rotationRequired = Math.toRadians(image.getCa());
    double locationX = width / 2d;
    double locationY = height / 2d;
    AffineTransform tx = AffineTransform.getRotateInstance(rotationRequired, locationX, locationY);
    AffineTransformOp op = new AffineTransformOp(tx, AffineTransformOp.TYPE_BILINEAR);

    g.drawImage(op.filter(bi, null), p.x - (width / 2), p.y - (height / 2), Main.map.mapView);
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
    return new Action[]{
      LayerListDialog.getInstance().createShowHideLayerAction(),
      LayerListDialog.getInstance().createDeleteLayerAction(),
      new LayerListPopup.InfoAction(this)
    };
  }

  /**
   * Returns the 2 closest images belonging to a different sequence and
   * different from the currently selected one.
   *
   * @return An array of length 2 containing the two closest images belonging to
   *         different sequences.
   */
  private MapillaryImage[] getClosestImagesFromDifferentSequences() {
    if (!(this.data.getSelectedImage() instanceof MapillaryImage))
      return new MapillaryImage[]{null, null};
    MapillaryImage selected = (MapillaryImage) this.data.getSelectedImage();
    MapillaryImage[] ret = new MapillaryImage[2];
    double[] distances = {
        SEQUENCE_MAX_JUMP_DISTANCE,
        SEQUENCE_MAX_JUMP_DISTANCE
    };
    LatLon selectedCoords = this.data.getSelectedImage().getLatLon();
    for (MapillaryAbstractImage imagePrev : this.data.getImages()) {
      if (!(imagePrev instanceof MapillaryImage))
        continue;
      if (!imagePrev.isVisible())
        continue;
      MapillaryImage image = (MapillaryImage) imagePrev;
      if (image.getLatLon().greatCircleDistance(selectedCoords) < SEQUENCE_MAX_JUMP_DISTANCE
          && selected.getSequence() != image.getSequence()) {
        if (
            ret[0] == null && ret[1] == null
            || image.getLatLon().greatCircleDistance(selectedCoords) < distances[0]
            && (ret[1] == null || image.getSequence() != ret[1].getSequence())
        ) {
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
    return new StringBuilder(35)
      .append(tr("Mapillary layer"))
      .append('\n')
      .append(tr("Total images:"))
      .append(' ')
      .append(this.data.size())
      .append('\n')
      .toString();
  }

  @Override
  public String getToolTipText() {
    return this.data.size() + (' ' + tr("images"));
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
    Main.worker.submit(new DelayedDownload());
  }

  @Override
  public void primitivesAdded(PrimitivesAddedEvent event) {
    // Required by DataSetListener. But we are not interested in what changed, only _that_ something changed.
  }

  @Override
  public void primitivesRemoved(PrimitivesRemovedEvent event) {
    // Required by DataSetListener. But we are not interested in what changed, only _that_ something changed.
  }

  @Override
  public void tagsChanged(TagsChangedEvent event) {
    // Required by DataSetListener. But we are not interested in what changed, only _that_ something changed.
  }

  @Override
  public void nodeMoved(NodeMovedEvent event) {
    // Required by DataSetListener. But we are not interested in what changed, only _that_ something changed.
  }

  @Override
  public void wayNodesChanged(WayNodesChangedEvent event) {
    // Required by DataSetListener. But we are not interested in what changed, only _that_ something changed.
  }

  @Override
  public void relationMembersChanged(RelationMembersChangedEvent event) {
    // Required by DataSetListener. But we are not interested in what changed, only _that_ something changed.
  }

  @Override
  public void otherDatasetChange(AbstractDatasetChangedEvent event) {
    // Required by DataSetListener. But we are not interested in what changed, only _that_ something changed.
  }

  @Override
  public void visitBoundingBox(BoundingXYVisitor v) {
  }

  @Override
  public void activeLayerChange(Layer oldLayer, Layer newLayer) {
    if (newLayer == this) {
      MapillaryUtils.updateHelpText();
      MapillaryPlugin.setMenuEnabled(MapillaryPlugin.getJoinMenu(), true);
    } else
      MapillaryPlugin.setMenuEnabled(MapillaryPlugin.getJoinMenu(), false);
  }

  @Override
  public void layerAdded(Layer newLayer) {
    // Do nothing, we're only interested in layer change, not addition
  }

  @Override
  public void layerRemoved(Layer oldLayer) {
    // Do nothing, we're only interested in layer change, not removal
  }

  /**
   * Threads that runs a delayed Mapillary download.
   *
   * @author nokutu
   *
   */
  private static class DelayedDownload extends Thread {

    @Override
    public void run() {
      try {
        sleep(1500);
      } catch (InterruptedException e) {
        Main.error(e);
      }
      MapillaryDownloader.automaticDownload();
    }
  }

  /**
   * Action used to delete images.
   *
   * @author nokutu
   *
   */
  private class DeleteImageAction extends AbstractAction {

    private static final long serialVersionUID = -982809854631863962L;

    @Override
    public void actionPerformed(ActionEvent e) {
      if (instance != null)
        MapillaryRecord.getInstance().addCommand(
            new CommandDelete(getData().getMultiSelectedImages()));
    }
  }
}
