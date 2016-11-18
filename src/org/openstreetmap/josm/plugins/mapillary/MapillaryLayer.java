// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.AlphaComposite;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Composite;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.TexturePaint;
import java.awt.event.ActionEvent;
import java.awt.geom.Line2D;
import java.awt.geom.Path2D;
import java.awt.image.BufferedImage;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Icon;
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
import org.openstreetmap.josm.data.preferences.IntegerProperty;
import org.openstreetmap.josm.gui.MapView;
import org.openstreetmap.josm.gui.NavigatableComponent;
import org.openstreetmap.josm.gui.dialogs.LayerListDialog;
import org.openstreetmap.josm.gui.dialogs.LayerListPopup;
import org.openstreetmap.josm.gui.layer.AbstractModifiableLayer;
import org.openstreetmap.josm.gui.layer.Layer;
import org.openstreetmap.josm.gui.layer.MainLayerManager.ActiveLayerChangeEvent;
import org.openstreetmap.josm.gui.layer.MainLayerManager.ActiveLayerChangeListener;
import org.openstreetmap.josm.plugins.mapillary.cache.CacheUtils;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryChangesetDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryFilterDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryMainDialog;
import org.openstreetmap.josm.plugins.mapillary.history.MapillaryRecord;
import org.openstreetmap.josm.plugins.mapillary.history.commands.CommandDelete;
import org.openstreetmap.josm.plugins.mapillary.io.download.MapillaryDownloader;
import org.openstreetmap.josm.plugins.mapillary.io.download.MapillaryDownloader.DOWNLOAD_MODE;
import org.openstreetmap.josm.plugins.mapillary.mode.AbstractMode;
import org.openstreetmap.josm.plugins.mapillary.mode.JoinMode;
import org.openstreetmap.josm.plugins.mapillary.mode.SelectMode;
import org.openstreetmap.josm.plugins.mapillary.utils.MapViewGeometryUtil;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryColorScheme;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;

/**
 * This class represents the layer shown in JOSM. There can only exist one
 * instance of this object.
 *
 * @author nokutu
 */
public final class MapillaryLayer extends AbstractModifiableLayer implements
  DataSetListener, ActiveLayerChangeListener {

  /** Maximum distance for the red/blue lines. */
  public static final int SEQUENCE_MAX_JUMP_DISTANCE = MapillaryProperties.SEQUENCE_MAX_JUMP_DISTANCE.get();

  /** The radius of the image marker */
  private static final int IMG_MARKER_RADIUS = 7;
  /** The radius of the circular sector that indicates the camera angle */
  private static final int CA_INDICATOR_RADIUS = 15;
  /** The angle of the circular sector that indicates the camera angle */
  private static final int CA_INDICATOR_ANGLE = 40;
  /** Length of the edge of the small sign, which indicates that traffic signs have been found in an image. */
  private static final int TRAFFIC_SIGN_SIZE = 6;
  /** A third of the height of the sign, for easier calculations */
  private static final double TRAFFIC_SIGN_HEIGHT_3RD = Math.sqrt(
    Math.pow(TRAFFIC_SIGN_SIZE, 2) - Math.pow(TRAFFIC_SIGN_SIZE / 2d, 2)
  ) / 3;

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

  private volatile TexturePaint hatched;
  private final MapillaryLocationChangeset locationChangeset = new MapillaryLocationChangeset();

  private MapillaryLayer() {
    super(tr("Mapillary Images"));
    this.data = new MapillaryData();
  }

  /**
   * Initializes the Layer.
   */
  private void init() {
    if (Main.main != null && Main.isDisplayingMapView()) {
      setMode(new SelectMode());
      Main.getLayerManager().addLayer(this);
      Main.getLayerManager().addActiveLayerChangeListener(this);
      if (Main.getLayerManager().getEditLayer() != null) {
        Main.getLayerManager().getEditLayer().data.addDataSetListener(this);
      }
      if (MapillaryDownloader.getMode() == DOWNLOAD_MODE.OSM_AREA) {
        MapillaryDownloader.downloadOSMArea();
      }
      if (MapillaryDownloader.getMode() == DOWNLOAD_MODE.VISIBLE_AREA) {
        this.mode.zoomChanged();
      }
    }
    // Does not execute when in headless mode
    if (MapillaryPlugin.getExportMenu() != null) {
      MapillaryPlugin.setMenuEnabled(MapillaryPlugin.getExportMenu(), true);
      if (!MapillaryMainDialog.getInstance().isShowing()) {
        MapillaryMainDialog.getInstance().getButton().doClick();
      }
    }
    createHatchTexture();
    if (Main.main != null) {
      MapillaryMainDialog.getInstance()
        .getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW)
        .put(KeyStroke.getKeyStroke("DELETE"), "MapillaryDel");
      MapillaryMainDialog.getInstance().getActionMap()
        .put("MapillaryDel", new DeleteImageAction());

      MapillaryData.dataUpdated();
      getLocationChangeset().addChangesetListener(MapillaryChangesetDialog.getInstance());
    }
  }

  /**
   * Changes the mode the the given one.
   *
   * @param mode The mode that is going to be activated.
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
   * Returns the {@link MapillaryLocationChangeset} object, which acts as the database of the
   * Layer.
   *
   * @return The {@link MapillaryData} object that stores the database.
   */
  public MapillaryLocationChangeset getLocationChangeset() {
    return locationChangeset;
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
    Main.getLayerManager().removeActiveLayerChangeListener(this);
    if (Main.getLayerManager().getEditLayer() != null)
      Main.getLayerManager().getEditLayer().data.removeDataSetListener(this);
    clearInstance();
    super.destroy();
  }

  @Override
  public boolean isModified() {
    return this.data.getImages().parallelStream().anyMatch(MapillaryAbstractImage::isModified);
  }

  @Override
  public void setVisible(boolean visible) {
    super.setVisible(visible);
    this.data.getImages().parallelStream().forEach(img -> img.setVisible(visible));
    if (Main.map != null) {
      MapillaryFilterDialog.getInstance().refresh();
    }
  }

  /**
   * Initialize the hatch pattern used to paint the non-downloaded area.
   */
  private void createHatchTexture() {
    BufferedImage bi = new BufferedImage(15, 15, BufferedImage.TYPE_INT_ARGB);
    Graphics2D big = bi.createGraphics();
    big.setColor(MapillaryProperties.BACKGROUND.get());
    Composite comp = AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.3f);
    big.setComposite(comp);
    big.fillRect(0, 0, 15, 15);
    big.setColor(MapillaryProperties.OUTSIDE_DOWNLOADED_AREA.get());
    big.drawLine(0, 15, 15, 0);
    Rectangle r = new Rectangle(0, 0, 15, 15);
    this.hatched = new TexturePaint(bi, r);
  }

  @Override
  public synchronized void paint(final Graphics2D g, final MapView mv, final Bounds box) {
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
    if (Main.getLayerManager().getActiveLayer() == this) {
      // paint remainder
      g.setPaint(this.hatched);
      g.fill(MapViewGeometryUtil.getNonDownloadedArea(mv, this.data.getBounds()));
    }

    // Draw colored lines
    MapillaryMainDialog.getInstance().blueButton.setEnabled(false);
    MapillaryMainDialog.getInstance().redButton.setEnabled(false);
    blue = null;
    red = null;

    // Draw the blue and red line and enable/disable the buttons
    if (this.data.getSelectedImage() != null) {
      MapillaryImage[] closestImages = getClosestImagesFromDifferentSequences();
      Point selected = mv.getPoint(this.data.getSelectedImage().getMovingLatLon());
      if (closestImages[0] != null) {
        blue = closestImages[0];
        g.setColor(Color.BLUE);
        final Point p = mv.getPoint(closestImages[0].getMovingLatLon());
        g.draw(new Line2D.Double(p.getX(), p.getY(), selected.getX(), selected.getY()));
        MapillaryMainDialog.getInstance().blueButton.setEnabled(true);
      }
      if (closestImages[1] != null) {
        red = closestImages[1];
        g.setColor(Color.RED);
        final Point p = mv.getPoint(closestImages[1].getMovingLatLon());
        g.draw(new Line2D.Double(p.getX(), p.getY(), selected.getX(), selected.getY()));
        MapillaryMainDialog.getInstance().redButton.setEnabled(true);
      }
    }
    // Draw sequence line
    g.setStroke(new BasicStroke(2));
    final MapillaryAbstractImage selectedImage = getData().getSelectedImage();
    for (MapillarySequence seq : getData().getSequences()) {
      if (seq.getImages().contains(selectedImage)) {
        g.setColor(
          seq.getKey() == null ? MapillaryColorScheme.SEQ_IMPORTED_SELECTED : MapillaryColorScheme.SEQ_SELECTED
        );
      } else {
        g.setColor(
          seq.getKey() == null ? MapillaryColorScheme.SEQ_IMPORTED_UNSELECTED : MapillaryColorScheme.SEQ_UNSELECTED
        );
      }
      g.draw(MapViewGeometryUtil.getSequencePath(mv, seq));
    }
    for (MapillaryAbstractImage imageAbs : this.data.getImages()) {
      if (imageAbs.isVisible() && Main.map.mapView.contains(Main.map.mapView.getPoint(imageAbs.getMovingLatLon()))) {
        drawImageMarker(g, imageAbs);
      }
    }
    if (this.mode instanceof JoinMode) {
      this.mode.paint(g, mv, box);
    }
  }

  /**
   * Draws an image marker onto the given Graphics context.
   * @param g the Graphics context
   * @param img the image to be drawn onto the Graphics context
   */
  private void drawImageMarker(final Graphics2D g, final MapillaryAbstractImage img) {
    if (img == null || img.getLatLon() == null) {
      Main.warn("An image is not painted, because it is null or has no LatLon!");
      return;
    }
    final MapillaryAbstractImage selectedImg = getData().getSelectedImage();
    final Point p = Main.map.mapView.getPoint(img.getMovingLatLon());

    // Determine colors
    final Color markerC;
    final Color directionC;
    if (selectedImg != null && getData().getMultiSelectedImages().contains(img)) {
      markerC = img instanceof MapillaryImportedImage
        ? MapillaryColorScheme.SEQ_IMPORTED_HIGHLIGHTED
        : MapillaryColorScheme.SEQ_HIGHLIGHTED;
      directionC = img instanceof MapillaryImportedImage
        ? MapillaryColorScheme.SEQ_IMPORTED_HIGHLIGHTED_CA
        : MapillaryColorScheme.SEQ_HIGHLIGHTED_CA;
    } else if (selectedImg != null && selectedImg.getSequence() != null && selectedImg.getSequence().equals(img.getSequence())) {
      markerC = img instanceof MapillaryImportedImage
        ? MapillaryColorScheme.SEQ_IMPORTED_SELECTED
        : MapillaryColorScheme.SEQ_SELECTED;
      directionC = img instanceof MapillaryImportedImage
        ? MapillaryColorScheme.SEQ_IMPORTED_SELECTED_CA
        : MapillaryColorScheme.SEQ_SELECTED_CA;
    } else {
      markerC = img instanceof MapillaryImportedImage
        ? MapillaryColorScheme.SEQ_IMPORTED_UNSELECTED
        : MapillaryColorScheme.SEQ_UNSELECTED;
      directionC = img instanceof MapillaryImportedImage
        ? MapillaryColorScheme.SEQ_IMPORTED_UNSELECTED_CA
        : MapillaryColorScheme.SEQ_UNSELECTED_CA;
    }

    // Paint direction indicator
    g.setColor(directionC);
    g.fillArc(p.x - CA_INDICATOR_RADIUS, p.y - CA_INDICATOR_RADIUS, 2 * CA_INDICATOR_RADIUS, 2 * CA_INDICATOR_RADIUS, (int) (90 - img.getMovingCa() - CA_INDICATOR_ANGLE / 2), CA_INDICATOR_ANGLE);
    // Paint image marker
    g.setColor(markerC);
    g.fillOval(p.x - IMG_MARKER_RADIUS, p.y - IMG_MARKER_RADIUS, 2 * IMG_MARKER_RADIUS, 2 * IMG_MARKER_RADIUS);

    // Paint highlight for selected or highlighted images
    if (img.equals(getData().getHighlightedImage()) || getData().getMultiSelectedImages().contains(img)) {
      g.setColor(Color.WHITE);
      g.setStroke(new BasicStroke(2));
      g.drawOval(p.x - IMG_MARKER_RADIUS, p.y - IMG_MARKER_RADIUS, 2 * IMG_MARKER_RADIUS, 2 * IMG_MARKER_RADIUS);
    }


    if (img instanceof MapillaryImage && !((MapillaryImage) img).getSigns().isEmpty()) {
      Path2D trafficSign = new Path2D.Double();
      trafficSign.moveTo(p.getX() - TRAFFIC_SIGN_SIZE / 2d, p.getY() - TRAFFIC_SIGN_HEIGHT_3RD);
      trafficSign.lineTo(p.getX() + TRAFFIC_SIGN_SIZE / 2d, p.getY() - TRAFFIC_SIGN_HEIGHT_3RD);
      trafficSign.lineTo(p.getX(), p.getY() + 2 * TRAFFIC_SIGN_HEIGHT_3RD);
      trafficSign.closePath();
      g.setColor(Color.WHITE);
      g.fill(trafficSign);
      g.setStroke(new BasicStroke(1));
      g.setColor(Color.RED);
      g.draw(trafficSign);
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
   * different sequences.
   */
  private MapillaryImage[] getClosestImagesFromDifferentSequences() {
    if (!(this.data.getSelectedImage() instanceof MapillaryImage))
      return new MapillaryImage[2];
    MapillaryImage selected = (MapillaryImage) this.data.getSelectedImage();
    MapillaryImage[] ret = new MapillaryImage[2];
    double[] distances = {
      SEQUENCE_MAX_JUMP_DISTANCE,
      SEQUENCE_MAX_JUMP_DISTANCE
    };
    LatLon selectedCoords = this.data.getSelectedImage().getMovingLatLon();
    for (MapillaryAbstractImage imagePrev : this.data.getImages()) {
      if (!(imagePrev instanceof MapillaryImage))
        continue;
      if (!imagePrev.isVisible())
        continue;
      MapillaryImage image = (MapillaryImage) imagePrev;
      if (image.getMovingLatLon().greatCircleDistance(selectedCoords) < SEQUENCE_MAX_JUMP_DISTANCE
        && selected.getSequence() != image.getSequence()) {
        if (
          ret[0] == null && ret[1] == null
            || image.getMovingLatLon().greatCircleDistance(selectedCoords) < distances[0]
            && (ret[1] == null || image.getSequence() != ret[1].getSequence())
          ) {
          ret[0] = image;
          distances[0] = image.getMovingLatLon().greatCircleDistance(selectedCoords);
        } else if ((ret[1] == null || image.getMovingLatLon().greatCircleDistance(
          selectedCoords) < distances[1])
          && image.getSequence() != ret[0].getSequence()) {
          ret[1] = image;
          distances[1] = image.getMovingLatLon().greatCircleDistance(selectedCoords);
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
    return tr("Mapillary layer") +
      '\n' +
      tr("Total images:") +
      ' ' +
      this.data.size() +
      '\n';
  }

  @Override
  public String getToolTipText() {
    return this.data.size() + (' ' + tr("images"));
  }

  @Override
  public void activeOrEditLayerChanged(ActiveLayerChangeEvent e) {
    if (Main.getLayerManager().getActiveLayer() == this) {
      MapillaryUtils.updateHelpText();
    }
    MapillaryPlugin.setMenuEnabled(MapillaryPlugin.getJoinMenu(), Main.getLayerManager().getActiveLayer() == this);

    if (Main.getLayerManager().getEditLayer() != e.getPreviousEditLayer()) {
      if (Main.getLayerManager().getEditLayer() != null) {
        Main.getLayerManager().getEditLayer().data.addDataSetListener(this);
      }
      if (e.getPreviousEditLayer() != null) {
        e.getPreviousEditLayer().data.removeDataSetListener(this);
      }
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

  /**
   * Threads that runs a delayed Mapillary download.
   *
   * @author nokutu
   */
  private static class DelayedDownload extends Thread {

    @Override
    public void run() {
      try {
        sleep(1500);
      } catch (InterruptedException e) {
        Main.error(e);
      }
      MapillaryDownloader.downloadOSMArea();
    }
  }

  /**
   * Action used to delete images.
   *
   * @author nokutu
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
