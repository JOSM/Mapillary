// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.layer;

import org.openstreetmap.josm.actions.RenameLayerAction;
import org.openstreetmap.josm.command.AddPrimitivesCommand;
import org.openstreetmap.josm.command.Command;
import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.data.UndoRedoHandler;
import org.openstreetmap.josm.data.imagery.ImageryInfo;
import org.openstreetmap.josm.data.imagery.vectortile.mapbox.MVTTile;
import org.openstreetmap.josm.data.osm.DataSet;
import org.openstreetmap.josm.data.osm.HighlightUpdateListener;
import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.data.osm.IPrimitive;
import org.openstreetmap.josm.data.osm.IRelation;
import org.openstreetmap.josm.data.osm.IWay;
import org.openstreetmap.josm.data.osm.Node;
import org.openstreetmap.josm.data.osm.OsmPrimitive;
import org.openstreetmap.josm.data.osm.Way;
import org.openstreetmap.josm.data.osm.event.AbstractDatasetChangedEvent;
import org.openstreetmap.josm.data.osm.event.DataSetListenerAdapter.Listener;
import org.openstreetmap.josm.data.osm.visitor.BoundingXYVisitor;
import org.openstreetmap.josm.data.osm.visitor.PrimitiveVisitor;
import org.openstreetmap.josm.data.osm.visitor.paint.AbstractMapRenderer;
import org.openstreetmap.josm.data.osm.visitor.paint.MapRendererFactory;
import org.openstreetmap.josm.data.vector.VectorDataSet;
import org.openstreetmap.josm.data.vector.VectorNode;
import org.openstreetmap.josm.data.vector.VectorPrimitive;
import org.openstreetmap.josm.data.vector.VectorRelation;
import org.openstreetmap.josm.data.vector.VectorWay;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.MapView;
import org.openstreetmap.josm.gui.dialogs.LayerListDialog;
import org.openstreetmap.josm.gui.dialogs.LayerListPopup;
import org.openstreetmap.josm.gui.layer.LayerManager.LayerAddEvent;
import org.openstreetmap.josm.gui.layer.LayerManager.LayerChangeListener;
import org.openstreetmap.josm.gui.layer.LayerManager.LayerOrderChangeEvent;
import org.openstreetmap.josm.gui.layer.LayerManager.LayerRemoveEvent;
import org.openstreetmap.josm.gui.layer.OsmDataLayer;
import org.openstreetmap.josm.gui.layer.imagery.MVTLayer;
import org.openstreetmap.josm.gui.mappaint.MapPaintStyles;
import org.openstreetmap.josm.gui.mappaint.loader.MapPaintStyleLoader;
import org.openstreetmap.josm.gui.mappaint.mapcss.MapCSSStyleSource;
import org.openstreetmap.josm.gui.tagging.presets.TaggingPreset;
import org.openstreetmap.josm.gui.util.GuiHelper;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.actions.MapillaryDownloadAction;
import org.openstreetmap.josm.plugins.mapillary.cache.MapillaryCache;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.AdditionalInstructions;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.ObjectDetections;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.VectorDataSelectionListener;
import org.openstreetmap.josm.plugins.mapillary.data.osm.event.FilterEventListener;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryMainDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.dialog.MapillaryExpertFilterDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.dialog.MapillaryFilterDialog;
import org.openstreetmap.josm.plugins.mapillary.io.download.MapillaryDownloader;
import org.openstreetmap.josm.plugins.mapillary.io.download.TileAddEventSource;
import org.openstreetmap.josm.plugins.mapillary.io.download.TileAddListener;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryKeys;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;
import org.openstreetmap.josm.tools.GBC;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.ImageProvider.ImageSizes;
import org.openstreetmap.josm.tools.ListenerList;
import org.openstreetmap.josm.tools.Logging;
import org.openstreetmap.josm.tools.Pair;

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
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics2D;
import java.awt.GridBagLayout;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
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
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.openstreetmap.josm.tools.I18n.tr;
import static org.openstreetmap.josm.tools.I18n.trn;

/**
 * Mapillary Point Object layer
 */
public class PointObjectLayer extends MVTLayer implements Listener, HighlightUpdateListener,
  VectorDataSelectionListener, LayerChangeListener, TileAddEventSource<MVTTile> {
  private final FilterEventListener tableModelListener;
  private static final String PAINT_STYLE_SOURCE = "resource://mapcss/Mapillary.mapcss";
  private static MapCSSStyleSource mapcss;
  private final Map<IPrimitive, JWindow> displayedWindows = new HashMap<>();

  private boolean showingPresetWindow;
  private final ListenerList<TileAddListener<MVTTile>> listeners = ListenerList.create();

  private static MapCSSStyleSource getMapCSSStyle() {
    List<MapCSSStyleSource> styles = MapPaintStyles.getStyles().getStyleSources().parallelStream()
      .filter(MapCSSStyleSource.class::isInstance).map(MapCSSStyleSource.class::cast)
      .filter(s -> PAINT_STYLE_SOURCE.equals(s.url)).collect(Collectors.toList());
    mapcss = styles.isEmpty() ? new MapCSSStyleSource(PAINT_STYLE_SOURCE, "Mapillary", "Mapillary Point Objects")
      : styles.get(0);
    return mapcss;
  }

  public PointObjectLayer(ImageryInfo info) {
    super(info);
    getMapCSSStyle();
    if (!MapPaintStyles.getStyles().getStyleSources().contains(mapcss)) {
      MapPaintStyles.addStyle(mapcss);
      mapcss.active = true;
      MapPaintStyleLoader.reloadStyle(mapcss);
      MapPaintStyles.fireMapPaintStyleEntryUpdated(MapPaintStyles.getStyles().getStyleSources().indexOf(mapcss));
    }
    tableModelListener = new FilterEventListener(this, this.getData());
    MapillaryExpertFilterDialog.getInstance().getFilterModel().addTableModelListener(tableModelListener);
    MainApplication.getMap().filterDialog.getFilterModel().addTableModelListener(tableModelListener);
    tableModelListener.tableChanged(null);

    VectorDataSet data = this.getData();
    data.addHighlightUpdateListener(this);
    data.addSelectionListener(this);
    if (MapillaryLayer.hasInstance()) {
      MapillaryLayer.getInstance().getData().addSelectionListener(this);
    }
    MainApplication.getLayerManager().addLayerChangeListener(this);
  }

  @Override
  public void finishedLoading(MVTTile tile) {
    super.finishedLoading(tile);
    // This is required for the mapcss to work properly
    tile.getData().getAllPrimitives().forEach(primitive -> primitive.put("layer", primitive.getLayer()));
    this.listeners.fireEvent(listener -> listener.tileAdded(tile));
  }

  @Override
  public Icon getIcon() {
    return MapillaryPlugin.LOGO.setSize(ImageSizes.LAYER).get();
  }

  @Override
  public void paint(final Graphics2D g, final MapView mv, Bounds box) {
    boolean virtual = mv.isVirtualNodesEnabled();

    AbstractMapRenderer painter = MapRendererFactory.getInstance().createActiveRenderer(g, mv, false);
    boolean slowOperations = mv.getMapMover() == null || !mv.getMapMover().movementInProgress()
      || !OsmDataLayer.PROPERTY_HIDE_LABELS_WHILE_DRAGGING.get();
    painter.enableSlowOperations(slowOperations);
    painter.render(this.getData(), virtual, box);
    MainApplication.getMap().conflictDialog.paintConflicts(g, mv);
    if (slowOperations) {
      // TODO is the box the same thing?
      List<IPrimitive> selectedInView = this.getData().getSelected().parallelStream().filter(p -> {
        Point point = mv.getPoint(p.getBBox().getCenter());
        return mv.contains(point);
      }).collect(Collectors.toList());
      if (Boolean.TRUE.equals(MapillaryProperties.SMART_EDIT.get()) && MainApplication.getMap().mapView
        .getDist100Pixel() <= MapillaryProperties.SMART_ADD_MIN_DIST_PER_PIXEL.get()) {
        selectedInView.forEach(p -> paintAdditionalPanel(p, mv));
      }
    } else {
      this.displayedWindows.forEach((o, w) -> hideWindow(w));
    }
  }

  private void paintAdditionalPanel(IPrimitive mapillaryObject, final MapView mv) {
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

  @Override
  public <L extends TileAddListener<MVTTile>> boolean addListener(L listener) {
    if (!this.listeners.containsListener(listener)) {
      this.listeners.addListener(listener);
    }
    return this.listeners.containsListener(listener);
  }

  @Override
  public <L extends TileAddListener<MVTTile>> boolean removeListener(L listener) {
    if (this.listeners.containsListener(listener)) {
      this.listeners.removeListener(listener);
    }
    return this.listeners.containsListener(listener);
  }

  private class AdditionalActionPanel extends JPanel {
    private boolean hasContent;

    /**
     * @param mapillaryObject The Mapillary object which should have additional actions
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
     * Check if there is content in the layer
     *
     * @return {@code true} if there is content to show
     */
    public boolean hasContent() {
      return this.hasContent;
    }

  }

  /**
   * @param mv The current MapView
   * @param displayedPanel The panel to display
   * @param xl left x coordinate of icon
   * @param xr right x coordinate of icon
   * @param yt top y coordinate of icon
   * @param yb bottom y coordinate of icon
   * @return A point that will keep the panel in view
   */
  private static Point fixPanelSizeAndLocation(MapView mv, JPanel displayedPanel, int xl, int xr, int yt, int yb) {
    int leftMaxWidth = (int) (0.95 * xl);
    int rightMaxWidth = (int) (0.95 * mv.getWidth() - xr);
    int topMaxHeight = (int) (0.95 * yt);
    int bottomMaxHeight = (int) (0.95 * mv.getHeight() - yb);
    Dimension d = displayedPanel.getPreferredSize();
    // place tooltip on left or right side of icon, based on its width
    Point screenLocation = mv.getLocationOnScreen();
    return new Point(screenLocation.x + (d.width > rightMaxWidth && d.width <= leftMaxWidth ? xl - d.width : xr),
      screenLocation.y + (d.height > bottomMaxHeight && d.height <= topMaxHeight ? yt - d.height - 10 : yb));
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
  public void selectionChanged(
    SelectionChangeEvent<VectorPrimitive, VectorNode, VectorWay, VectorRelation, VectorDataSet> event) {
    Collection<VectorPrimitive> selection = event.getSelection();
    VectorPrimitive prim = selection.parallelStream().filter(p -> !p.isDeleted() && p.hasKey(MapillaryKeys.DETECTIONS))
      .findFirst().orElse(null);
    this.displayedWindows.entrySet().parallelStream().filter(e -> !selection.contains(e.getKey()))
      .forEach(e -> hideWindow(e.getValue()));
    Map<IPrimitive, JWindow> temporaryWindows = this.displayedWindows.entrySet().parallelStream()
      .filter(e -> selection.contains(e.getKey())).collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
    this.displayedWindows.clear();
    this.displayedWindows.putAll(temporaryWindows);

    if (prim != null && (MapillaryLayer.hasInstance() || Boolean.TRUE.equals(MapillaryProperties.SMART_EDIT.get()))) {
      List<Map<String, String>> detections = parseDetections(prim.get(MapillaryKeys.DETECTIONS));

      VectorDataSet mapillaryData = MapillaryLayer.getInstance().getData();
      if (!MainApplication.getLayerManager().containsLayer(MapillaryLayer.getInstance())) {
        MapillaryDownloadAction.addLayer();
      }
      INode selectedImage = mapillaryData.getSelectedNodes().stream().findFirst().orElse(null);
      List<INode> images = getImagesForDetections(mapillaryData, detections);
      INode toSelect = images.isEmpty() ? null : getBestImage(prim, images);
      boolean inDetections = selectedImage != null && images.contains(selectedImage);

      if (!inDetections && (selectedImage == null || !selectedImage.equals(toSelect))) {
        mapillaryData.setSelected(toSelect);
      }
    }
    GuiHelper.runInEDT(() -> MapillaryMainDialog.getInstance().imageViewer.repaint());
    GuiHelper.runInEDT(() -> MapillaryFilterDialog.getInstance().updateFilteredImages());
  }

  /**
   * Get a decent image to look at a primitive
   *
   * @param <T> The type of images
   * @param primitive The primitive to get the best image for
   * @param images The images to look through
   * @return The best image (hopefully)
   */
  private static <T extends INode> T getBestImage(IPrimitive primitive, Collection<T> images) {
    if (primitive instanceof INode) {
      List<Pair<T, Double>> distance = images.stream().filter(Objects::nonNull)
        .map(image -> new Pair<>(image, ((INode) primitive).getCoor().distanceSq(image.getCoor())))
        .sorted(Comparator.comparing(p -> p.b)).collect(Collectors.toList());
      for (float i : new float[] { 0.8f, 0.6f, 0.4f, 0.2f, 0.0f }) {
        Optional<T> imageOptional = distance.stream().map(p -> p.a)
          .filter(image -> MapillaryImageUtils.getQuality(image) == Float.MIN_VALUE
            || MapillaryImageUtils.getQuality(image) == i)
          .findFirst();
        if (imageOptional.isPresent()) {
          return imageOptional.get();
        }
      }
    }
    return null;
  }

  /**
   * Add a Mapillary Object Detection Primitive to OSM
   *
   * @param mapillaryObject The primitive to add to OSM
   * @param detection The detections for the primitive
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
      preset.showAndApply(new HashSet<>(add.getParticipatingPrimitives()));
      this.showingPresetWindow = true;
      basePrimitive = dataSet.getPrimitiveById(basePrimitive.getPrimitiveId());
      final int userSelection;
      final Command updateTagsCommand;
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
        String detections = mapillaryObject.get(MapillaryKeys.DETECTIONS);
        UndoRedoHandler.getInstance().add(deleteOriginal);
        if (updateTagsCommand != null) {
          UndoRedoHandler.getInstance().add(updateTagsCommand);
        }
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

  /**
   * Parse detections
   *
   * @param detectionsValue The value from the detections key
   * @return A list of {@code Map<String, String>} of detections. Keys usually include detection_key, image_key, and
   *         user_key.
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

  private static List<INode> getImagesForDetections(VectorDataSet data, List<Map<String, String>> detections) {
    List<String> keys = detections.stream().filter(m -> m.containsKey(MapillaryKeys.IMAGE_KEY))
      .map(m -> m.get(MapillaryKeys.IMAGE_KEY)).collect(Collectors.toList());
    Set<String> keySet = data.getNodes().stream().filter(img -> img.hasKey(MapillaryKeys.KEY))
      .map(img -> img.get(MapillaryKeys.KEY)).collect(Collectors.toSet());
    String[] missing = keys.stream().filter(key -> !keySet.contains(key)).toArray(String[]::new);
    if (keys.isEmpty())
      MapillaryDownloader.downloadImages(missing);
    else
      MapillaryUtils.getForkJoinPool(MapillaryCache.class).execute(() -> MapillaryDownloader.downloadImages(missing));
    Map<String, INode> nodeMap = data.getNodes().stream().filter(img -> img.hasKey(MapillaryKeys.KEY))
      .collect(Collectors.toMap(i -> i.get(MapillaryKeys.KEY), i -> i));
    return keys.stream().map(nodeMap::get).collect(Collectors.toList());
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
    MainApplication.getMap().filterDialog.getFilterModel().removeTableModelListener(tableModelListener);
    List<? extends PointObjectLayer> layers = MainApplication.getLayerManager().getLayersOfType(this.getClass());
    if (layers.isEmpty() || (layers.size() == 1 && this.equals(layers.get(0))))
      MapPaintStyles.removeStyle(mapcss);
    VectorDataSet data = this.getData();
    data.removeHighlightUpdateListener(this);
    data.removeSelectionListener(this);
    this.displayedWindows.forEach((i, w) -> hideWindow(w));
    this.displayedWindows.clear();
    if (MapillaryLayer.hasInstance()) {
      MapillaryLayer.getInstance().getData().removeSelectionListener(this);
    }
    MainApplication.getLayerManager().removeLayerChangeListener(this);
  }

  private static class DataCountVisitor implements PrimitiveVisitor {
    int nodes;
    int ways;
    int relations;

    @Override
    public void visit(INode n) {
      nodes++;
    }

    @Override
    public void visit(IWay<?> w) {
      ways++;
    }

    @Override
    public void visit(IRelation<?> r) {
      relations++;
    }
  }

  @Override
  public String getToolTipText() {
    DataCountVisitor counter = new DataCountVisitor();
    for (final IPrimitive osm : this.getData().allPrimitives()) {
      osm.accept(counter);
    }
    int nodes = counter.nodes;
    int ways = counter.ways;
    int relations = counter.relations;

    StringBuilder tooltip = new StringBuilder("<html>").append(trn("{0} node", "{0} nodes", nodes, nodes))
      .append("<br>").append(trn("{0} way", "{0} ways", ways, ways)).append("<br>")
      .append(trn("{0} relation", "{0} relations", relations, relations));

    File f = getAssociatedFile();
    if (f != null) {
      tooltip.append("<br>").append(f.getPath());
    }
    tooltip.append("</html>");
    return tooltip.toString();
  }

  @Override
  public Object getInfoComponent() {
    final DataCountVisitor counter = new DataCountVisitor();
    for (final IPrimitive osm : this.getData().allPrimitives()) {
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
  public void visitBoundingBox(BoundingXYVisitor v) {
    for (final INode n : this.getData().getNodes()) {
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
    if (e.getAddedLayer() instanceof MVTLayer
      && ((MVTLayer) e.getAddedLayer()).getInfo().equals(MapillaryKeys.MAPILLARY_IMAGES)) {
      ((MVTLayer) e.getAddedLayer()).getData().addSelectionListener(this);
    }
  }

  @Override
  public void layerRemoving(LayerRemoveEvent e) {
    if (e.getRemovedLayer() instanceof MVTLayer
      && ((MVTLayer) e.getRemovedLayer()).getInfo().equals(MapillaryKeys.MAPILLARY_IMAGES)) {
      ((MVTLayer) e.getRemovedLayer()).getData().removeSelectionListener(this);
    }
  }

  @Override
  public void layerOrderChanged(LayerOrderChangeEvent e) {
    // Don't care
  }

  @Override
  public void zoomChanged() {
    super.zoomChanged();
    this.getData().setZoom(this.getZoomLevel());
  }

  /*
   * TODO finish porting
   * @Override
   * public void selectedImageChanged(INode oldImage, INode newImage) {
   * final VectorDataSet data = this.getData();
   * Collection<VectorPrimitive> currentSelection = data.getSelected();
   * if (newImage.hasKey(MapillaryKeys.DETECTIONS)) {
   * String key = newImage.hasKey(MapillaryKeys.KEY) ? newImage.get(MapillaryKeys.KEY) : null;
   * Collection<IPrimitive> nodes = newImage.get(MapillaryKeys.DETECTIONS).getDetections(false).parallelStream()
   * .map(ImageDetection::getKey).flatMap(
   * // Create a new ArrayList to avoid a ConcurrentModificationException
   * d -> new ArrayList<>(data.getNodes()).parallelStream()
   * .filter(n -> n.hasKey(MapillaryKeys.DETECTIONS) && n.get(MapillaryKeys.DETECTIONS).contains(d)))
   * .collect(Collectors.toList());
   * if (!nodes.containsAll(currentSelection) && !newImageDetections.getDetections().isEmpty()
   * && !Boolean.TRUE.equals(MapillaryProperties.SMART_EDIT.get()) && currentSelection.stream()
   * .filter(i -> i.hasKey(MapillaryKeys.DETECTIONS)).anyMatch(i -> !i.get(MapillaryKeys.DETECTIONS).contains(key))) {
   * data.setSelected(nodes);
   * }
   * } else {
   * data.clearSelection();
   * }
   * }
   */
}
