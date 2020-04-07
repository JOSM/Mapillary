// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.layer;

import static org.openstreetmap.josm.tools.I18n.marktr;
import static org.openstreetmap.josm.tools.I18n.tr;
import static org.openstreetmap.josm.tools.I18n.trn;

import java.awt.AlphaComposite;
import java.awt.Composite;
import java.awt.Graphics2D;
import java.awt.GridBagLayout;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.TexturePaint;
import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.geom.Area;
import java.awt.geom.Path2D;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonException;
import javax.json.JsonObject;
import javax.json.JsonString;
import javax.json.JsonValue;
import javax.json.stream.JsonParser;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;

import org.openstreetmap.josm.actions.RenameLayerAction;
import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.data.Data;
import org.openstreetmap.josm.data.DataSource;
import org.openstreetmap.josm.data.coor.EastNorth;
import org.openstreetmap.josm.data.osm.DataSelectionListener;
import org.openstreetmap.josm.data.osm.DataSet;
import org.openstreetmap.josm.data.osm.DataSourceChangeEvent;
import org.openstreetmap.josm.data.osm.DataSourceListener;
import org.openstreetmap.josm.data.osm.DownloadPolicy;
import org.openstreetmap.josm.data.osm.HighlightUpdateListener;
import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.data.osm.Node;
import org.openstreetmap.josm.data.osm.OsmData;
import org.openstreetmap.josm.data.osm.OsmPrimitive;
import org.openstreetmap.josm.data.osm.UploadPolicy;
import org.openstreetmap.josm.data.osm.event.AbstractDatasetChangedEvent;
import org.openstreetmap.josm.data.osm.event.DataSetListenerAdapter;
import org.openstreetmap.josm.data.osm.event.DataSetListenerAdapter.Listener;
import org.openstreetmap.josm.data.osm.visitor.BoundingXYVisitor;
import org.openstreetmap.josm.data.osm.visitor.paint.AbstractMapRenderer;
import org.openstreetmap.josm.data.osm.visitor.paint.MapRendererFactory;
import org.openstreetmap.josm.data.osm.visitor.paint.relations.MultipolygonCache;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.MapView;
import org.openstreetmap.josm.gui.MapViewState.MapViewPoint;
import org.openstreetmap.josm.gui.dialogs.LayerListDialog;
import org.openstreetmap.josm.gui.layer.AbstractOsmDataLayer;
import org.openstreetmap.josm.gui.layer.Layer;
import org.openstreetmap.josm.gui.layer.LayerManager.LayerAddEvent;
import org.openstreetmap.josm.gui.layer.LayerManager.LayerChangeListener;
import org.openstreetmap.josm.gui.layer.LayerManager.LayerOrderChangeEvent;
import org.openstreetmap.josm.gui.layer.LayerManager.LayerRemoveEvent;
import org.openstreetmap.josm.gui.layer.OsmDataLayer;
import org.openstreetmap.josm.gui.layer.OsmDataLayer.DataCountVisitor;
import org.openstreetmap.josm.gui.mappaint.MapPaintStyles;
import org.openstreetmap.josm.gui.mappaint.loader.MapPaintStyleLoader;
import org.openstreetmap.josm.gui.mappaint.mapcss.MapCSSStyleSource;
import org.openstreetmap.josm.gui.preferences.display.DrawingPreference;
import org.openstreetmap.josm.gui.progress.NullProgressMonitor;
import org.openstreetmap.josm.gui.progress.swing.PleaseWaitProgressMonitor;
import org.openstreetmap.josm.io.GeoJSONReader;
import org.openstreetmap.josm.io.IllegalDataException;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryData;
import org.openstreetmap.josm.plugins.mapillary.MapillaryDataListener;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.data.osm.event.FilterEventListener;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryMainDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.dialog.MapillaryExpertFilterDialog;
import org.openstreetmap.josm.plugins.mapillary.model.ImageDetection;
import org.openstreetmap.josm.plugins.mapillary.oauth.MapillaryUser;
import org.openstreetmap.josm.plugins.mapillary.oauth.OAuthUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL;
import org.openstreetmap.josm.tools.GBC;
import org.openstreetmap.josm.tools.HttpClient;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.ImageProvider.ImageSizes;
import org.openstreetmap.josm.tools.Logging;
import org.openstreetmap.josm.tools.OpenBrowser;

/**
 * Mapillary Point Object layer
 */
public class PointObjectLayer extends AbstractOsmDataLayer
  implements DataSourceListener, MouseListener, Listener, HighlightUpdateListener, DataSelectionListener,
  MapillaryDataListener, LayerChangeListener {
  private final Collection<DataSource> dataSources = new HashSet<>();
  private static final String NAME = marktr("Mapillary Point Objects");
  private static final int HATCHED_SIZE = 15;
  private final DataSet followDataSet;
  private final FilterEventListener tableModelListener;
  private static final String PAINT_STYLE_SOURCE = "resource://mapcss/Mapillary.mapcss";
  private static MapCSSStyleSource mapcss;
  private final DataSet data;
  private final DataSetListenerAdapter dataSetListenerAdapter;

  /**
   * a texture for non-downloaded area Copied from OsmDataLayer
   */
  private static volatile BufferedImage hatched;

  static {
    createHatchTexture();
  }

  private static MapCSSStyleSource getMapCSSStyle() {
    List<MapCSSStyleSource> styles = MapPaintStyles.getStyles().getStyleSources().parallelStream()
        .filter(MapCSSStyleSource.class::isInstance).map(MapCSSStyleSource.class::cast)
        .filter(s -> PAINT_STYLE_SOURCE.equals(s.url)).collect(Collectors.toList());
    mapcss = styles.isEmpty() ? new MapCSSStyleSource(PAINT_STYLE_SOURCE, "Mapillary", "Mapillary Point Objects")
        : styles.get(0);
    return mapcss;
  }

  /**
   * Initialize the hatch pattern used to paint the non-downloaded area
   */
  public static void createHatchTexture() {
    BufferedImage bi = new BufferedImage(HATCHED_SIZE, HATCHED_SIZE, BufferedImage.TYPE_INT_ARGB);
    Graphics2D big = bi.createGraphics();
    big.setColor(OsmDataLayer.getBackgroundColor());
    Composite comp = AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.3f);
    big.setComposite(comp);
    big.fillRect(0, 0, HATCHED_SIZE, HATCHED_SIZE);
    big.setColor(OsmDataLayer.getOutsideColor());
    big.drawLine(-1, 6, 6, -1);
    big.drawLine(4, 16, 16, 4);
    hatched = bi;
  }

  public PointObjectLayer() {
    super(NAME);
    data = new DataSet();
    data.setUploadPolicy(UploadPolicy.BLOCKED);
    data.setDownloadPolicy(DownloadPolicy.BLOCKED);
    data.lock();
    followDataSet = MainApplication.getLayerManager().getActiveDataSet();
    followDataSet.addDataSourceListener(this);
    this.setName(NAME + ": " + MainApplication.getLayerManager().getActiveDataLayer().getName());
    MainApplication.worker.execute(() -> followDataSet.getDataSources().forEach(this::getData));
    getMapCSSStyle();
    if (!MapPaintStyles.getStyles().getStyleSources().contains(mapcss)) {
      MapPaintStyles.addStyle(mapcss);
      mapcss.active = true;
      MapPaintStyleLoader.reloadStyle(mapcss);
      MapPaintStyles.fireMapPaintStyleEntryUpdated(MapPaintStyles.getStyles().getStyleSources().indexOf(mapcss));
    }
    tableModelListener = new FilterEventListener(this, data);
    MapillaryExpertFilterDialog.getInstance().getFilterModel().addTableModelListener(tableModelListener);
    MainApplication.getMap().filterDialog.getFilterModel().addTableModelListener(tableModelListener);
    tableModelListener.tableChanged(null);

    this.data.setName(NAME);
    this.dataSetListenerAdapter = new DataSetListenerAdapter(this);
    data.addDataSetListener(dataSetListenerAdapter);
    data.addDataSetListener(MultipolygonCache.getInstance());
    data.addHighlightUpdateListener(this);
    data.addSelectionListener(this);
    if (MapillaryLayer.hasInstance()) {
      MapillaryLayer.getInstance().getData().addListener(this);
    }
    MainApplication.getLayerManager().addLayerChangeListener(this);
  }

  @Override
  public void dataSourceChange(DataSourceChangeEvent event) {
    if (SwingUtilities.isEventDispatchThread()) {
      MainApplication.worker.execute(() -> event.getAdded().forEach(this::getData));
    } else {
      event.getAdded().forEach(this::getData);
    }
  }

  public void getData(DataSource dataSource) {
    if (dataSources.add(dataSource)) {
      try {
        data.unlock();
        realGetData(dataSource.bounds, data);
        data.addDataSource(dataSource);
      } catch (IllegalDataException | IOException e) {
        Logging.error(e);
        dataSources.remove(dataSource);
      } finally {
        data.lock();
        this.tableModelListener.updateAndRunFilters();
      }
    }
  }

  private static void realGetData(Bounds bound, DataSet data) throws IllegalDataException, IOException {
    URL url = MapillaryURL.APIv3.searchMapPointObjects(bound);
    do {
      HttpClient client = HttpClient.create(url);
      if (MapillaryUser.getUsername() != null)
        OAuthUtils.addAuthenticationHeader(client);
      client.connect();
      try (InputStream stream = client.getResponse().getContent()) {
        DataSet ds = GeoJSONReader.parseDataSet(stream, NullProgressMonitor.INSTANCE);
        ds.allPrimitives().parallelStream().filter(p -> p.hasKey("detections"))
            .forEach(p -> p.put("detections_num", Integer.toString(p.get("detections").split("detection_key").length)));
        ds.allPrimitives().forEach(p -> p.setModified(false));
        synchronized (PointObjectLayer.class) {
          data.mergeFrom(ds);
        }
        url = MapillaryURL.APIv3.parseNextFromLinkHeaderValue(client.getResponse().getHeaderField("Link"));
      } finally {
        client.disconnect();
      }
    } while (url != null);
  }

  @Override
  public Icon getIcon() {
    return MapillaryPlugin.LOGO.setSize(ImageSizes.LAYER).get();
  }

  @Override
  public void paint(final Graphics2D g, final MapView mv, Bounds box) {
    boolean active = mv.getLayerManager().getActiveLayer() == this;
    boolean inactive = false;
    boolean virtual = mv.isVirtualNodesEnabled();

    // draw the hatched area for non-downloaded region. only draw if we're the active
    // and bounds are defined; don't draw for inactive layers or loaded GPX files etc
    if (active && Boolean.TRUE.equals(DrawingPreference.SOURCE_BOUNDS_PROP.get()) && !data.getDataSources().isEmpty()) {
      // initialize area with current viewport
      Rectangle b = mv.getBounds();
      // on some platforms viewport bounds seem to be offset from the left,
      // over-grow it just to be sure
      b.grow(100, 100);
      Path2D p = new Path2D.Double();

      // combine successively downloaded areas
      for (Bounds bounds : data.getDataSourceBounds()) {
        if (bounds.isCollapsed()) {
          continue;
        }
        p.append(mv.getState().getArea(bounds), false);
      }
      // subtract combined areas
      Area a = new Area(b);
      a.subtract(new Area(p));

      // paint remainder
      MapViewPoint anchor = mv.getState().getPointFor(new EastNorth(0, 0));
      Rectangle2D anchorRect = new Rectangle2D.Double(anchor.getInView().getX() % HATCHED_SIZE,
          anchor.getInView().getY() % HATCHED_SIZE, HATCHED_SIZE, HATCHED_SIZE);
      if (hatched != null) {
        g.setPaint(new TexturePaint(hatched, anchorRect));
      }
      try {
        g.fill(a);
      } catch (ArrayIndexOutOfBoundsException e) {
        // #16686 - AIOOBE in java.awt.TexturePaintContext$Int.setRaster
        Logging.error(e);
      }
    }

    AbstractMapRenderer painter = MapRendererFactory.getInstance().createActiveRenderer(g, mv, inactive);
    painter.enableSlowOperations(mv.getMapMover() == null || !mv.getMapMover().movementInProgress()
        || !OsmDataLayer.PROPERTY_HIDE_LABELS_WHILE_DRAGGING.get());
    painter.render(data, virtual, box);
    MainApplication.getMap().conflictDialog.paintConflicts(g, mv);
  }

  @Override
  public void selectionChanged(SelectionChangeEvent event) {
    OsmPrimitive prim = event.getSelection().parallelStream().filter(p -> p.hasKey("detections")).findFirst()
        .orElse(null);
    if (prim != null && MapillaryLayer.hasInstance()) {
      List<Map<String, String>> detections = parseDetections(prim.get("detections"));

      MapillaryData mapillaryData = MapillaryLayer.getInstance().getData();
      MapillaryImage selectedImage = mapillaryData.getSelectedImage() instanceof MapillaryImage
          ? (MapillaryImage) mapillaryData.getSelectedImage()
          : null;
      List<MapillaryImage> images = getImagesForDetections(mapillaryData, detections);
      MapillaryImage toSelect = images.isEmpty() ? null : images.get(0);
      boolean inDetections = selectedImage != null && images.contains(selectedImage);

      if (!inDetections && (selectedImage == null || !selectedImage.equals(toSelect))) {
        mapillaryData.setSelectedImage(toSelect);
      }
    }
    SwingUtilities.invokeLater(() -> MapillaryMainDialog.getInstance().mapillaryImageDisplay.repaint());
  }

  @Override
  public void hookUpMapView() {
    super.hookUpMapView();
    MainApplication.getMap().mapView.addMouseListener(this);
  }

  /**
   * Parse detections
   *
   * @param detectionsValue The value from the detections key
   * @return A list of Map<String, String> of detections. Keys usually include detection_key, image_key, and user_key.
   */
  public static List<Map<String, String>> parseDetections(String detectionsValue) {
    List<Map<String, String>> detections = new ArrayList<>();
    try (JsonParser parser = Json
        .createParser(new ByteArrayInputStream(detectionsValue.getBytes(StandardCharsets.UTF_8)))) {
      while (parser.hasNext() && JsonParser.Event.START_ARRAY == parser.next()) {
        JsonArray array = parser.getArray();
        for (JsonObject obj : array.getValuesAs(JsonObject.class)) {
          Map<String, String> detection = new HashMap<>();
          for (Map.Entry<String, JsonValue> entry : obj.entrySet()) {
            if (entry.getValue().getValueType().equals(JsonValue.ValueType.STRING)) {
              detection.putIfAbsent(entry.getKey(), ((JsonString) entry.getValue()).getString());
            } else {
              detection.putIfAbsent(entry.getKey(), entry.getValue().toString());
            }
          }
          detections.add(detection);
        }
      }
    } catch (JsonException e) {
      Logging.error(e);
    }
    return detections;
  }

  private static List<MapillaryImage> getImagesForDetections(MapillaryData data, List<Map<String, String>> detections) {
    return detections.stream().filter(m -> m.containsKey("image_key")).map(m -> m.get("image_key")).map(data::getImage)
        .collect(Collectors.toList());
  }

  @Override
  public Action[] getMenuEntries() {
    List<Action> actions = new ArrayList<>();
    actions.addAll(Arrays.asList(LayerListDialog.getInstance().createActivateLayerAction(this),
        LayerListDialog.getInstance().createShowHideLayerAction(),
        LayerListDialog.getInstance().createDeleteLayerAction(), SeparatorLayerAction.INSTANCE,
        LayerListDialog.getInstance().createMergeLayerAction(this)));
    actions.addAll(Arrays.asList(SeparatorLayerAction.INSTANCE, new RenameLayerAction(getAssociatedFile(), this),
        SeparatorLayerAction.INSTANCE, new RequestDataAction(followDataSet)));
    return actions.toArray(new Action[0]);
  }

  static class RequestDataAction extends AbstractAction {
    private static final long serialVersionUID = 8823333297547249069L;
    private final OsmData<?, ?, ?, ?> data;

    public RequestDataAction(DataSet data) {
      super(tr("Request Data"));
      this.data = data;
      MapillaryPlugin.LOGO.getResource().attachImageIcon(this, true);
    }

    @Override
    public void actionPerformed(ActionEvent e) {
      String bbox = "?bbox="
        + String.join(";",
          data.getDataSourceBounds().stream()
          .map(Bounds::toBBox).map(b -> b.toStringCSV(",")).collect(Collectors.toList()));
      OpenBrowser.displayUrl("https://mapillary.github.io/mapillary_solutions/data-request" + bbox);
    }
  }

  @Override
  public synchronized void destroy() {
    super.destroy();
    followDataSet.removeDataSourceListener(this);
    MainApplication.getMap().filterDialog.getFilterModel().removeTableModelListener(tableModelListener);
    MainApplication.getMap().mapView.removeMouseListener(this);
    List<? extends PointObjectLayer> layers = MainApplication.getLayerManager().getLayersOfType(this.getClass());
    if (layers.isEmpty() || (layers.size() == 1 && this.equals(layers.get(0))))
      MapPaintStyles.removeStyle(mapcss);
    data.removeDataSetListener(dataSetListenerAdapter);
    data.removeDataSetListener(MultipolygonCache.getInstance());
    data.removeHighlightUpdateListener(this);
    data.removeSelectionListener(this);
    if (MapillaryLayer.hasInstance()) {
      MapillaryLayer.getInstance().getData().removeListener(this);
    }
    MainApplication.getLayerManager().removeLayerChangeListener(this);

  }

  @Override
  public DataSet getDataSet() {
    return data;
  }

  // @Override Depends upon #18801
  public Data getData() {
    return getDataSet();
  }

  @Override
  public String getToolTipText() {
    DataCountVisitor counter = new DataCountVisitor();
    for (final OsmPrimitive osm : data.allPrimitives()) {
      osm.accept(counter);
    }
    int nodes = counter.nodes - counter.deletedNodes;
    int ways = counter.ways - counter.deletedWays;
    int rels = counter.relations - counter.deletedRelations;

    StringBuilder tooltip = new StringBuilder("<html>").append(trn("{0} node", "{0} nodes", nodes, nodes))
        .append("<br>").append(trn("{0} way", "{0} ways", ways, ways)).append("<br>")
        .append(trn("{0} relation", "{0} relations", rels, rels));

    File f = getAssociatedFile();
    if (f != null) {
      tooltip.append("<br>").append(f.getPath());
    }
    tooltip.append("</html>");
    return tooltip.toString();
  }

  @Override
  public void mergeFrom(Layer from) {
    if (isMergable(from)) {
      DataSet fromData = ((PointObjectLayer) from).getDataSet();
      final PleaseWaitProgressMonitor monitor = new PleaseWaitProgressMonitor(tr("Merging layers"));
      monitor.setCancelable(false);
      fromData.unlock();
      data.mergeFrom(fromData, monitor);
      fromData.lock();
      monitor.close();
    }
  }

  @Override
  public boolean isMergable(Layer other) {
    return other instanceof PointObjectLayer;
  }

  @Override
  public Object getInfoComponent() {
    final DataCountVisitor counter = new DataCountVisitor();
    for (final OsmPrimitive osm : data.allPrimitives()) {
      osm.accept(counter);
    }
    final JPanel p = new JPanel(new GridBagLayout());

    String nodeText = trn("{0} node", "{0} nodes", counter.nodes, counter.nodes);
    String wayText = trn("{0} way", "{0} ways", counter.ways, counter.ways);
    String relationText = trn("{0} relation", "{0} relations", counter.relations, counter.relations);

    p.add(new JLabel(tr("{0} consists of:", getName())), GBC.eol());
    p.add(new JLabel(nodeText, ImageProvider.get("data", "node"), SwingConstants.HORIZONTAL),
        GBC.eop().insets(15, 0, 0, 0));
    p.add(new JLabel(wayText, ImageProvider.get("data", "way"), SwingConstants.HORIZONTAL),
        GBC.eop().insets(15, 0, 0, 0));
    p.add(new JLabel(relationText, ImageProvider.get("data", "relation"), SwingConstants.HORIZONTAL),
        GBC.eop().insets(15, 0, 0, 0));

    return p;
  }

  @Override
  public void mouseClicked(MouseEvent e) {
    if (!SwingUtilities.isLeftMouseButton(e) || this.equals(MainApplication.getLayerManager().getActiveLayer())) {
      return;
    }
    Point clickPoint = e.getPoint();
    double snapDistance = 20;
    double minDistance = Double.MAX_VALUE;
    final int iconHeight = ImageProvider.ImageSizes.SMALLICON.getAdjustedHeight();
    Node closestNode = null;
    for (Node node : data.getNodes().parallelStream().filter(n -> !n.isDisabled()).collect(Collectors.toList())) {
      Point notePoint = MainApplication.getMap().mapView.getPoint(node.getCoor());
      // move the note point to the center of the icon where users are most likely to click when selecting
      notePoint.setLocation(notePoint.getX(), notePoint.getY() - iconHeight / 2d);
      double dist = clickPoint.distanceSq(notePoint);
      if (minDistance > dist && clickPoint.distance(notePoint) < snapDistance) {
        minDistance = dist;
        closestNode = node;
      }
    }
    data.setSelected(closestNode);
  }

  @Override
  public void mousePressed(MouseEvent e) {
    // Do nothing
  }

  @Override
  public void mouseReleased(MouseEvent e) {
    // Do nothing
  }

  @Override
  public void mouseEntered(MouseEvent e) {
    // Do nothing
  }

  @Override
  public void mouseExited(MouseEvent e) {
    // Do nothing
  }

  @Override
  public boolean isModified() {
    return false;
  }

  @Override
  public void visitBoundingBox(BoundingXYVisitor v) {
    for (final Node n : data.getNodes()) {
      if (n.isUsable()) {
        v.visit(n);
      }
    }
  }

  @Override
  public void highlightUpdated(HighlightUpdateEvent e) {
    invalidate();
  }

  @Override
  public void processDatasetEvent(AbstractDatasetChangedEvent event) {
    invalidate();
  }

  @Override
  public void layerAdded(LayerAddEvent e) {
    if (e.getAddedLayer() instanceof MapillaryLayer) {
      ((MapillaryLayer) e.getAddedLayer()).getData().addListener(this);
    }
  }

  @Override
  public void layerRemoving(LayerRemoveEvent e) {
    if (e.getRemovedLayer() instanceof MapillaryLayer) {
      ((MapillaryLayer) e.getRemovedLayer()).getData().removeListener(this);
    }
  }

  @Override
  public void layerOrderChanged(LayerOrderChangeEvent e) {
    // Don't care
  }

  @Override
  public void imagesAdded() {
    // Don't care
  }

  @Override
  public void selectedImageChanged(MapillaryAbstractImage oldImage, MapillaryAbstractImage newImage) {
    data.clearSelection();
    if (newImage instanceof MapillaryImage) {
      MapillaryImage image = (MapillaryImage) newImage;
      Collection<INode> nodes = image.getDetections().parallelStream().map(ImageDetection::getKey).flatMap(
        d -> data.getNodes().parallelStream().filter(n -> n.hasKey("detections") && n.get("detections").contains(d))
      ).collect(Collectors.toList());
      data.setSelected(nodes);
    }
  }

}
