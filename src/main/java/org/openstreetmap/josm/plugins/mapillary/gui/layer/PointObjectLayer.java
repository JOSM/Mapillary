// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.layer;

import static org.openstreetmap.josm.tools.I18n.marktr;
import static org.openstreetmap.josm.tools.I18n.tr;
import static org.openstreetmap.josm.tools.I18n.trn;

import java.awt.AlphaComposite;
import java.awt.Color;
import java.awt.Composite;
import java.awt.Dimension;
import java.awt.Graphics2D;
import java.awt.GridBagLayout;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.TexturePaint;
import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.awt.geom.Area;
import java.awt.geom.Path2D;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonException;
import javax.json.JsonObject;
import javax.json.JsonString;
import javax.json.JsonValue;
import javax.json.stream.JsonParser;
import javax.json.stream.JsonParser.Event;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JWindow;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;

import org.openstreetmap.josm.actions.RenameLayerAction;
import org.openstreetmap.josm.command.AddPrimitivesCommand;
import org.openstreetmap.josm.command.Command;
import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.data.Data;
import org.openstreetmap.josm.data.DataSource;
import org.openstreetmap.josm.data.UndoRedoHandler;
import org.openstreetmap.josm.data.coor.EastNorth;
import org.openstreetmap.josm.data.osm.DataSelectionListener;
import org.openstreetmap.josm.data.osm.DataSet;
import org.openstreetmap.josm.data.osm.DataSourceChangeEvent;
import org.openstreetmap.josm.data.osm.DataSourceListener;
import org.openstreetmap.josm.data.osm.DownloadPolicy;
import org.openstreetmap.josm.data.osm.HighlightUpdateListener;
import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.data.osm.IPrimitive;
import org.openstreetmap.josm.data.osm.Node;
import org.openstreetmap.josm.data.osm.OsmPrimitive;
import org.openstreetmap.josm.data.osm.UploadPolicy;
import org.openstreetmap.josm.data.osm.Way;
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
import org.openstreetmap.josm.gui.dialogs.LayerListPopup;
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
import org.openstreetmap.josm.gui.tagging.presets.TaggingPreset;
import org.openstreetmap.josm.gui.util.GuiHelper;
import org.openstreetmap.josm.plugins.mapillary.MapillaryData;
import org.openstreetmap.josm.plugins.mapillary.MapillaryDataListener;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.actions.MapillaryDownloadAction;
import org.openstreetmap.josm.plugins.mapillary.data.image.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.data.image.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.AdditionalInstructions;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.ObjectDetections;
import org.openstreetmap.josm.plugins.mapillary.data.osm.event.FilterEventListener;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryMainDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.dialog.MapillaryExpertFilterDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.dialog.MapillaryFilterDialog;
import org.openstreetmap.josm.plugins.mapillary.io.download.MapillaryDownloader;
import org.openstreetmap.josm.plugins.mapillary.io.download.ObjectDetectionsDownloadRunnable;
import org.openstreetmap.josm.plugins.mapillary.model.ImageDetection;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL;
import org.openstreetmap.josm.tools.GBC;
import org.openstreetmap.josm.tools.Geometry;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.ImageProvider.ImageSizes;
import org.openstreetmap.josm.tools.Logging;
import org.openstreetmap.josm.tools.Pair;

/**
 * Mapillary Point Object layer
 */
public class PointObjectLayer extends AbstractOsmDataLayer implements DataSourceListener, MouseListener, Listener,
  HighlightUpdateListener, DataSelectionListener, MapillaryDataListener, LayerChangeListener {
  private final Collection<DataSource> dataSources = new HashSet<>();
  private static final String[] NAMES = new String[] { marktr("Mapillary point features"),
    marktr("Mapillary traffic signs") };
  private static final int HATCHED_SIZE = 15;
  public final DataSet followDataSet;
  private final FilterEventListener tableModelListener;
  private static final String PAINT_STYLE_SOURCE = "resource://mapcss/Mapillary.mapcss";
  private static MapCSSStyleSource mapcss;
  private final DataSet data;
  private final DataSetListenerAdapter dataSetListenerAdapter;
  /** If true, display traffic signs. If false, display point objects. */
  private final boolean trafficSigns;
  private final Map<IPrimitive, JWindow> displayedWindows = new HashMap<>();
  private static final String DETECTIONS = "detections";

  private boolean showingPresetWindow;

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

  public PointObjectLayer(boolean trafficSigns) {
    super(tr(trafficSigns ? NAMES[1] : NAMES[0]));
    String name = trafficSigns ? NAMES[1] : NAMES[0];
    this.trafficSigns = trafficSigns;
    data = new DataSet();
    data.setUploadPolicy(UploadPolicy.BLOCKED);
    data.setDownloadPolicy(DownloadPolicy.BLOCKED);
    data.lock();
    followDataSet = MainApplication.getLayerManager().getActiveDataSet();
    followDataSet.addDataSourceListener(this);
    this.setName(name + ": " + followDataSet.getName());
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

    this.data.setName(name);
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
    if (MapillaryDownloader.DOWNLOAD_MODE.OSM_AREA
      .equals(MapillaryDownloader.DOWNLOAD_MODE.fromPrefId(MapillaryProperties.DOWNLOAD_MODE.get()))) {
      if (SwingUtilities.isEventDispatchThread()) {
        MainApplication.worker.execute(() -> event.getAdded().forEach(this::getData));
      } else {
        event.getAdded().forEach(this::getData);
      }
    }
    String name = trafficSigns ? NAMES[1] : NAMES[0];
    this.setName(name + ": " + followDataSet.getName());
    this.data.setName(name);
  }

  public void getData(DataSource dataSource) {
    if (SwingUtilities.isEventDispatchThread()) {
      MainApplication.worker.submit(() -> getData(dataSource));
    } else if (dataSources.add(dataSource)) {
      ObjectDetectionsDownloadRunnable runnable = new ObjectDetectionsDownloadRunnable(data, dataSource.bounds,
        trafficSigns ? MapillaryURL.APIv3::searchMapObjects : MapillaryURL.APIv3::searchMapPointObjects,
        NullProgressMonitor.INSTANCE);
      try {
        data.unlock();
        runnable.run();
        data.addDataSource(dataSource);
      } finally {
        data.lock();
        if (this.tableModelListener != null)
          this.tableModelListener.updateAndRunFilters();
      }
    }
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
    boolean slowOperations = mv.getMapMover() == null || !mv.getMapMover().movementInProgress()
      || !OsmDataLayer.PROPERTY_HIDE_LABELS_WHILE_DRAGGING.get();
    painter.enableSlowOperations(slowOperations);
    painter.render(data, virtual, box);
    MainApplication.getMap().conflictDialog.paintConflicts(g, mv);
    if (slowOperations) {
      // TODO is the box the same thing?
      List<OsmPrimitive> selectedInView = this.getDataSet().getSelected().parallelStream().filter(p -> {
        Point point = mv.getPoint(p.getBBox().getCenter());
        return mv.contains(point);
      }).collect(Collectors.toList());
      if (Boolean.TRUE.equals(MapillaryProperties.SMART_EDIT.get()) && MainApplication.getMap().mapView
        .getDist100Pixel() <= MapillaryProperties.SMART_ADD_MIN_DIST_PER_PIXEL.get()) {
        selectedInView.forEach(p -> paintAdditionalPanel(p, g, mv, box));
      }
    } else {
      this.displayedWindows.forEach((o, w) -> hideWindow(w));
    }
  }

  private void paintAdditionalPanel(IPrimitive mapillaryObject, final Graphics2D g, final MapView mv, Bounds box) {
    if (!(mapillaryObject instanceof INode)) {
      Logging.error("Mapillary Additional actions does not support {0}", mapillaryObject.getType());
      return;
    } else if (this.showingPresetWindow) {
      // Don't draw additional windows when preset window is open
      return;
    }
    int iconWidth = ImageProvider.ImageSizes.MAP.getAdjustedWidth();
    int iconHeight = ImageProvider.ImageSizes.MAP.getAdjustedHeight();
    Point p = mv.getPoint((INode) mapillaryObject);
    final JWindow displayedWindow = displayedWindows.getOrDefault(mapillaryObject,
      new JWindow(MainApplication.getMainFrame()));

    int xl = p.x - (iconWidth / 2) - 5;
    int xr = p.x + (iconWidth / 2) + 5;
    int yb = p.y - iconHeight - 1;
    int yt = p.y + (iconHeight / 2) + 2;
    Point pTooltip;

    AdditionalActionPanel displayedPanel = Stream.of(displayedWindow.getComponents())
      .filter(AdditionalActionPanel.class::isInstance).map(AdditionalActionPanel.class::cast).findAny().orElse(null);

    if (displayedPanel == null) {
      displayedPanel = new AdditionalActionPanel(mapillaryObject);
      pTooltip = fixPanelSizeAndLocation(mv, displayedPanel, xl, xr, yt, yb);
      displayedWindow.setAutoRequestFocus(false);
      displayedWindow.add(displayedPanel);
      // Forward mouse wheel scroll event to MapMover
      displayedWindow.addMouseWheelListener(e -> mv.getMapMover()
        .mouseWheelMoved((MouseWheelEvent) SwingUtilities.convertMouseEvent(displayedWindow, e, mv)));
    } else {
      pTooltip = fixPanelSizeAndLocation(mv, displayedPanel, xl, xr, yt, yb);
    }

    if (displayedPanel.hasContent()) {
      displayedWindow.pack();
      displayedWindow.setLocation(pTooltip);
      displayedWindow.setVisible(mv.contains(p));
      displayedWindows.put(mapillaryObject, displayedWindow);
    }
  }

  private class AdditionalActionPanel extends JPanel {
    private boolean hasContent;

    /**
     * @param mapillaryObject
     */
    public AdditionalActionPanel(IPrimitive mapillaryObject) {
      this.setBackground(UIManager.getColor("ToolTip.background"));
      this.setForeground(UIManager.getColor("ToolTip.foreground"));
      this.setFont(UIManager.getFont("ToolTip.font"));
      this.setBorder(BorderFactory.createLineBorder(Color.black));
      final ObjectDetections detection = ObjectDetections.valueOfMapillaryValue(mapillaryObject.get("value"));
      if (!detection.getTaggingPresets().isEmpty()) {
        this.hasContent = true;
        JButton add = new JButton(tr("Add"));
        add.setAction(new AbstractAction(tr("Add")) {
          @Override
          public void actionPerformed(ActionEvent e) {
            addMapillaryPrimitiveToOsm(mapillaryObject, detection);
          }
        });
        this.add(add);
      }
    }

    /**
     * @return {@code true} if there is content to show
     */
    public boolean hasContent() {
      return this.hasContent;
    }

  }

  /**
   * @param mv The current MapView
   * @param displayedPanel The panel to display
   * @param xl left x coord of icon
   * @param xr right x coord of icon
   * @param yt top y coord of icon
   * @param yb bottom y coord of icon
   * @return A point that will keep the panel in view
   */
  private static Point fixPanelSizeAndLocation(MapView mv, JPanel displayedPanel, int xl, int xr, int yt, int yb) {
    int leftMaxWidth = (int) (0.95 * xl);
    int rightMaxWidth = (int) (0.95 * mv.getWidth() - xr);
    int topMaxHeight = (int) (0.95 * yt);
    int bottomMaxHeight = (int) (0.95 * mv.getHeight() - yb);
    Dimension d = displayedPanel.getPreferredSize();
    // place tooltip on left or right side of icon, based on its width
    Point screenloc = mv.getLocationOnScreen();
    return new Point(screenloc.x + (d.width > rightMaxWidth && d.width <= leftMaxWidth ? xl - d.width : xr),
      screenloc.y + (d.height > bottomMaxHeight && d.height <= topMaxHeight ? yt - d.height - 10 : yb));
  }

  /**
   * Hide a window
   *
   * @param window The window to hide
   */
  private static void hideWindow(JWindow window) {
    GuiHelper.runInEDT(() -> {
      if (window != null) {
        window.setVisible(false);
        for (MouseWheelListener listener : window.getMouseWheelListeners()) {
          window.removeMouseWheelListener(listener);
        }
        window.dispose();
      }
    });
  }

  @Override
  public void selectionChanged(SelectionChangeEvent event) {
    Collection<OsmPrimitive> selection = event.getSelection();
    OsmPrimitive prim = selection.parallelStream().filter(p -> !p.isDeleted() && p.hasKey(DETECTIONS)).findFirst()
      .orElse(null);
    this.displayedWindows.entrySet().parallelStream().filter(e -> !selection.contains(e.getKey()))
      .forEach(e -> hideWindow(e.getValue()));
    Map<IPrimitive, JWindow> temporaryWindows = this.displayedWindows.entrySet().parallelStream()
      .filter(e -> selection.contains(e.getKey())).collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
    this.displayedWindows.clear();
    this.displayedWindows.putAll(temporaryWindows);

    if (prim != null && (MapillaryLayer.hasInstance() || Boolean.TRUE.equals(MapillaryProperties.SMART_EDIT.get()))) {
      List<Map<String, String>> detections = parseDetections(prim.get(DETECTIONS));

      MapillaryData mapillaryData = MapillaryLayer.getInstance().getData();
      if (!MainApplication.getLayerManager().containsLayer(MapillaryLayer.getInstance())) {
        MapillaryDownloadAction.addLayer();
      }
      MapillaryImage selectedImage = mapillaryData.getSelectedImage() instanceof MapillaryImage
        ? (MapillaryImage) mapillaryData.getSelectedImage()
        : null;
      List<MapillaryImage> images = getImagesForDetections(mapillaryData, detections);
      MapillaryImage toSelect = images.isEmpty() ? null : getBestImage(prim, images);
      boolean inDetections = selectedImage != null && images.contains(selectedImage);

      if (!inDetections && (selectedImage == null || !selectedImage.equals(toSelect))) {
        mapillaryData.setSelectedImage(toSelect);
      }
    }
    GuiHelper.runInEDT(() -> MapillaryMainDialog.getInstance().imageViewer.repaint());
    GuiHelper.runInEDT(() -> MapillaryFilterDialog.getInstance().updateFilteredImages());
  }

  /**
   * Get a decent image to look at a primiitive
   *
   * @param <T> The type of images
   * @param primitive The primitive to get the best image for
   * @param images The images to look through
   * @return The best image (hopefully)
   */
  private static <T extends MapillaryAbstractImage> T getBestImage(OsmPrimitive primitive, Collection<T> images) {
    List<Pair<T, Double>> distance = images.stream().filter(Objects::nonNull)
      .map(image -> new Pair<>(image, Geometry.getDistance(primitive, new Node(image.getExifCoor()))))
      .sorted(Comparator.comparing(p -> p.b)).collect(Collectors.toList());
    for (int i : new int[] { 5, 4, 3, Integer.MIN_VALUE, 2, 1 }) {
      Optional<T> imageOptional = distance.stream().map(p -> p.a).filter(image -> image.getQuality() == i).findFirst();
      if (imageOptional.isPresent()) {
        return imageOptional.get();
      }
    }
    return null;
  }

  /**
   * Add a Mapillary Object Detection Primitive to OSM
   *
   * @param mapillaryObject The primitive to add to OSM
   * @param detection
   */
  void addMapillaryPrimitiveToOsm(final IPrimitive mapillaryObject, final ObjectDetections detection) {
    hideWindow(this.displayedWindows.get(mapillaryObject));
    Collection<TaggingPreset> presets = detection.getTaggingPresets();
    DataSet dataSet = MainApplication.getLayerManager().getActiveDataSet();
    if (dataSet != null && !dataSet.isLocked() && presets.size() == 1) {
      TaggingPreset preset = presets.iterator().next();
      OsmPrimitive basePrimitive;
      Collection<OsmPrimitive> toAdd;
      if (mapillaryObject instanceof Node) {
        basePrimitive = new Node((Node) mapillaryObject);
        toAdd = Collections.singleton(basePrimitive);
      } else if (mapillaryObject instanceof Way) {
        Way way = new Way((Way) mapillaryObject);
        way.removeAll();
        way.setNodes(Collections.emptyList());
        ((Way) mapillaryObject).getNodes().forEach(node -> way.addNode(new Node(node)));
        basePrimitive = way;
        toAdd = new HashSet<>();
        toAdd.add(basePrimitive);
        toAdd.addAll(((Way) basePrimitive).getNodes());
      } else {
        return;
      }
      basePrimitive.removeAll();
      detection.getOsmKeys().forEach(basePrimitive::put);

      AddPrimitivesCommand add = new AddPrimitivesCommand(
        toAdd.stream().map(OsmPrimitive::save).collect(Collectors.toList()), dataSet);
      UndoRedoHandler.getInstance().add(add);
      // preset.showAndApply(new HashSet<>(add.getParticipatingPrimitives()));
      this.showingPresetWindow = true;
      int userSelection = TaggingPreset.DIALOG_ANSWER_CANCEL;
      basePrimitive = dataSet.getPrimitiveById(basePrimitive.getPrimitiveId());
      Command updateTagsCommand = null;
      try {
        userSelection = preset.showDialog(Collections.singleton(basePrimitive), false);
        updateTagsCommand = TaggingPreset.createCommand(Collections.singleton(basePrimitive), preset.getChangedTags());
      } finally {
        this.showingPresetWindow = false;
      }
      // Closing the window returns 0. Not in the TaggingPreset public answers at this time.
      if ((userSelection == 0 || userSelection == TaggingPreset.DIALOG_ANSWER_CANCEL)
        && UndoRedoHandler.getInstance().hasUndoCommands()) {
        // Technically, it would be easier to do one undo, but this avoids corner cases
        // where a user makes some modifications while the preset window is open.
        List<Command> undoCommands = UndoRedoHandler.getInstance().getUndoCommands();
        int index = undoCommands.size() - undoCommands.indexOf(add);
        UndoRedoHandler.getInstance().undo(index);
      } else if (basePrimitive.isTagged() && mapillaryObject.getDataSet() instanceof DataSet) {
        Command deleteOriginal = new Command(dataSet) {
          @Override
          public String getDescriptionText() {
            return tr("Delete Mapillary object");
          }

          @Override
          public void fillModifiedData(Collection<OsmPrimitive> modified, Collection<OsmPrimitive> deleted,
            Collection<OsmPrimitive> added) {
            if (mapillaryObject instanceof OsmPrimitive)
              deleted.add((OsmPrimitive) mapillaryObject);
          }

          @Override
          public boolean executeCommand() {
            DataSet mapillaryObjectData = (DataSet) mapillaryObject.getDataSet();
            boolean locked = mapillaryObjectData.isLocked();
            try {
              mapillaryObjectData.unlock();
              mapillaryObject.setDeleted(true);
            } finally {
              if (locked) {
                mapillaryObjectData.lock();
              }
            }
            return true;
          }

          @Override
          public void undoCommand() {
            DataSet mapillaryObjectData = (DataSet) mapillaryObject.getDataSet();
            boolean locked = mapillaryObjectData.isLocked();
            try {
              mapillaryObjectData.unlock();
              mapillaryObject.setDeleted(false);
            } finally {
              if (locked) {
                mapillaryObjectData.lock();
              }
            }
          }
        };
        String detections = mapillaryObject.get("detections");
        UndoRedoHandler.getInstance().add(deleteOriginal);
        UndoRedoHandler.getInstance().add(updateTagsCommand);
        try (JsonParser parser = Json
          .createParser(new ByteArrayInputStream(detections.getBytes(StandardCharsets.UTF_8)))) {
          while (parser.hasNext()) {
            Event event = parser.next();
            if (event == Event.START_ARRAY) {
              break;
            }
          }
          JsonArray array = parser.getArray();
          if (!array.isEmpty()) {
            Optional<JsonString> imageKey = array.stream().filter(JsonObject.class::isInstance)
              .map(JsonObject.class::cast).map(o -> o.getJsonString("image_key")).filter(Objects::nonNull).findFirst();
            if (imageKey.isPresent()) {
              basePrimitive.put("mapillary:image", imageKey.get().getString());
            }
          }
          if (mapillaryObject.hasKey("key")) {
            basePrimitive.put("mapillary:map_feature", mapillaryObject.get("key"));
          }
          final AdditionalInstructions additionalInstructions = detection.getAdditionalInstructions();
          if (additionalInstructions != null) {
            final Command additionalCommand = additionalInstructions.apply(basePrimitive);
            if (additionalCommand != null) {
              UndoRedoHandler.getInstance().add(additionalCommand);
            }
          }
        }
      }
    }
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
            if (entry.getValue().getValueType() == JsonValue.ValueType.STRING) {
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
    List<String> keys = detections.stream().filter(m -> m.containsKey("image_key")).map(m -> m.get("image_key"))
      .collect(Collectors.toList());
    String[] missing = keys.stream().filter(image -> data.getImage(image) == null).toArray(String[]::new);
    if (keys.isEmpty())
      MapillaryDownloader.downloadImages(missing);
    else
      MainApplication.worker.execute(() -> MapillaryDownloader.downloadImages(missing));
    return keys.stream().map(data::getImage).collect(Collectors.toList());
  }

  @Override
  public Action[] getMenuEntries() {
    List<Action> actions = new ArrayList<>();
    actions.addAll(Arrays.asList(LayerListDialog.getInstance().createActivateLayerAction(this),
      LayerListDialog.getInstance().createShowHideLayerAction(),
      LayerListDialog.getInstance().createDeleteLayerAction(), SeparatorLayerAction.INSTANCE,
      LayerListDialog.getInstance().createMergeLayerAction(this)));
    actions.addAll(Arrays.asList(SeparatorLayerAction.INSTANCE, new RenameLayerAction(getAssociatedFile(), this),
      SeparatorLayerAction.INSTANCE, SeparatorLayerAction.INSTANCE, new LayerListPopup.InfoAction(this)));
    return actions.toArray(new Action[0]);
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
    this.displayedWindows.forEach((i, w) -> hideWindow(w));
    this.displayedWindows.clear();
    if (MapillaryLayer.hasInstance()) {
      MapillaryLayer.getInstance().getData().removeListener(this);
    }
    MainApplication.getLayerManager().removeLayerChangeListener(this);
  }

  @Override
  public DataSet getDataSet() {
    return data;
  }

  @Override
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
      try {
        fromData.unlock();
        data.unlock();
        data.mergeFrom(fromData, monitor);
      } finally {
        fromData.lock();
        data.lock();
        monitor.close();
        ((PointObjectLayer) from).destroy();
      }
    }
  }

  @Override
  public boolean isMergable(Layer other) {
    return other instanceof PointObjectLayer && this.trafficSigns == ((PointObjectLayer) other).trafficSigns;
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
    if (!SwingUtilities.isLeftMouseButton(e) || this.equals(MainApplication.getLayerManager().getActiveLayer())
      || e.isConsumed()) {
      return;
    }
    Point clickPoint = e.getPoint();
    double snapDistance = ImageProvider.ImageSizes.MAP.getAdjustedHeight();
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
    if (closestNode != null || Boolean.FALSE.equals(MapillaryProperties.SMART_EDIT.get())
      || e.getClickCount() == MapillaryProperties.DESELECT_CLICK_COUNT.get()) {
      data.setSelected(closestNode);
    }
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
    Collection<OsmPrimitive> currentSelection = data.getSelected();
    if (newImage instanceof MapillaryImage) {
      MapillaryImage image = (MapillaryImage) newImage;
      Collection<IPrimitive> nodes = image.getDetections(true).parallelStream().map(ImageDetection::getKey)
        .flatMap(
          d -> data.getNodes().parallelStream().filter(n -> n.hasKey(DETECTIONS) && n.get(DETECTIONS).contains(d)))
        .collect(Collectors.toList());
      if (!nodes.containsAll(currentSelection) && !image.getDetections().isEmpty()
        && !Boolean.TRUE.equals(MapillaryProperties.SMART_EDIT.get())) {
        data.setSelected(nodes);
      }
    } else {
      data.clearSelection();
    }
  }

  /**
   * @return true if this layer has traffic signs
   */
  public boolean hasTrafficSigns() {
    return this.trafficSigns;
  }

  /**
   * @return true if this layer has point features (does not include traffic signs)
   */
  public boolean hasPointFeatures() {
    return !this.trafficSigns;
  }
}
