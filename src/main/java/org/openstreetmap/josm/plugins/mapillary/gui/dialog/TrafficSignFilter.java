// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.dialog;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ItemEvent;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.TreeMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.SpinnerNumberModel;
import javax.swing.SwingUtilities;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import org.openstreetmap.josm.data.osm.BBox;
import org.openstreetmap.josm.data.osm.DataSet;
import org.openstreetmap.josm.data.osm.DataSourceChangeEvent;
import org.openstreetmap.josm.data.osm.DataSourceListener;
import org.openstreetmap.josm.data.osm.Filter;
import org.openstreetmap.josm.data.osm.TagMap;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.layer.LayerManager.LayerAddEvent;
import org.openstreetmap.josm.gui.layer.LayerManager.LayerChangeListener;
import org.openstreetmap.josm.gui.layer.LayerManager.LayerOrderChangeEvent;
import org.openstreetmap.josm.gui.layer.LayerManager.LayerRemoveEvent;
import org.openstreetmap.josm.gui.layer.OsmDataLayer;
import org.openstreetmap.josm.gui.util.GuiHelper;
import org.openstreetmap.josm.gui.widgets.FilterField;
import org.openstreetmap.josm.plugins.datepicker.IDatePicker;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.DetectionType;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.ObjectDetections;
import org.openstreetmap.josm.plugins.mapillary.gui.DeveloperToggleAction;
import org.openstreetmap.josm.plugins.mapillary.gui.ImageCheckBoxButton;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.PointObjectLayer;
import org.openstreetmap.josm.spi.preferences.Config;
import org.openstreetmap.josm.tools.Destroyable;
import org.openstreetmap.josm.tools.GBC;
import org.openstreetmap.josm.tools.I18n;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.Logging;

/**
 * This filters traffic signs
 *
 * @author Taylor Smock
 */
public class TrafficSignFilter extends JPanel implements Destroyable, LayerChangeListener, DataSourceListener {
  private static final long serialVersionUID = 1177890183422385423L;
  private boolean destroyed;
  private final List<ImageCheckBoxButton> buttons;
  private boolean showRelevant;
  private boolean showAddable;
  private Boolean[] show = new Boolean[] { showRelevant, showAddable };
  private final FilterField filterField;
  private final SpinnerNumberModel showMaxNumberModel;
  private int detectionPage;
  private final JCheckBox toggleVisibleCheckbox;
  private static final String NEARBY_KEY = "nearby_osm_objects";

  private interface ResetListener {
    void reset();
  }

  private List<ResetListener> resetObjects = new ArrayList<>();

  public TrafficSignFilter() {
    setLayout(new GridBagLayout());

    /* First/last seen */
    createFirstLastSeen(this, "first");
    createFirstLastSeen(this, "last");
    /* End first/last seen */
    JPanel layers = new JPanel();
    layers.setLayout(new FlowLayout(FlowLayout.LEFT));
    layers.add(new JLabel(I18n.tr("Layer")));
    for (String[] layer : Arrays.asList(new String[] { "trafficsigns", I18n.marktr("Traffic Signs") },
      new String[] { "points", I18n.marktr("Point Objects") })) {
      JCheckBox lbox = new JCheckBox(I18n.tr(layer[1]));
      layers.add(lbox);
      lbox.addItemListener(TrafficSignFilter::updateLayers);
      lbox.setSelected(true);
      lbox.putClientProperty("layer", layer[0]);
    }
    add(layers);

    /* Filter signs */
    filterField = new FilterField().filter(this::filterButtons);
    filterField.setToolTipText(I18n.tr("Filter Mapillary Detections"));
    add(filterField, GBC.eol().fill(GridBagConstraints.HORIZONTAL));
    toggleVisibleCheckbox = new JCheckBox(I18n.tr("Select Visible"));
    JCheckBox showRelevantObjs = new JCheckBox(I18n.tr("Show Relevant"));
    JCheckBox showAddableObjs = new JCheckBox(I18n.tr("Only show addable objects"));
    JCheckBox hideNearbyAddableObjs = new JCheckBox(I18n.tr("hide when nearby OSM objects"));
    DeveloperToggleAction.addVisibilitySwitcher(showAddableObjs);
    DeveloperToggleAction.addVisibilitySwitcher(hideNearbyAddableObjs);
    JPanel pagination = new JPanel(new GridBagLayout());
    JButton previousButtonPagination = new JButton(ImageProvider.get("svpLeft"));
    JButton nextButtonPagination = new JButton(ImageProvider.get("svpRight"));
    nextButtonPagination.addActionListener(l -> updateDetectionPage(1));
    previousButtonPagination.addActionListener(l -> updateDetectionPage(-1));
    pagination.add(previousButtonPagination, GBC.std().anchor(GridBagConstraints.EAST));
    JSpinner showMax = new JSpinner();
    showMax.setMaximumSize(new Dimension(60, showMax.getMaximumSize().height));
    showMaxNumberModel = new SpinnerNumberModel(100, 10, Integer.MAX_VALUE, 10);
    showMax.setModel(showMaxNumberModel);
    showMaxNumberModel.addChangeListener(l -> updateShown(showMaxNumberModel));
    showMax.addChangeListener(l -> updateShown(showMaxNumberModel));
    pagination.add(showMax, GBC.std().anchor(GridBagConstraints.CENTER));
    pagination.add(nextButtonPagination, GBC.std().anchor(GridBagConstraints.WEST));
    showRelevantObjs.addItemListener(l -> showRelevantObjects(l.getStateChange() == ItemEvent.SELECTED));
    showAddableObjs.addItemListener(l -> showAddableObjects(l.getStateChange() == ItemEvent.SELECTED));
    hideNearbyAddableObjs.addItemListener(l -> hideNearbyAddableObjs(l.getStateChange() == ItemEvent.SELECTED));
    toggleVisibleCheckbox.addItemListener(l -> toggleVisible(l.getStateChange() == ItemEvent.SELECTED));
    add(showAddableObjs, GBC.std().anchor(GridBagConstraints.WEST));
    add(hideNearbyAddableObjs, GBC.eol().anchor(GridBagConstraints.CENTER));
    add(showRelevantObjs, GBC.std().anchor(GridBagConstraints.WEST));
    add(toggleVisibleCheckbox, GBC.std().anchor(GridBagConstraints.CENTER));
    add(pagination, GBC.eol().anchor(GridBagConstraints.EAST));

    this.resetObjects.add(() -> showAddableObjs.setSelected(false));
    this.resetObjects.add(() -> showRelevantObjs.setSelected(true));
    this.resetObjects.add(() -> hideNearbyAddableObjs.setSelected(false));
    this.resetObjects.add(() -> filterField.setText(""));
    this.resetObjects.add(() -> showMaxNumberModel.setValue(100));

    buttons = new ArrayList<>();
    addButtons();

    if (!showRelevantObjs.isSelected()) {
      showRelevantObjs.doClick();
    }
    /* End filter signs */

    MainApplication.getLayerManager().addAndFireLayerChangeListener(this);
  }

  private static void updateLayers(ItemEvent itemEvent) {
    if (itemEvent.getItem() instanceof JCheckBox) {
      JCheckBox box = (JCheckBox) itemEvent.getItem();
      String layerName = (String) box.getClientProperty("layer");
      if (layerName != null) {
        MapillaryFilterTableModel model = MapillaryExpertFilterDialog.getInstance().getFilterModel();
        Filter layerFilter = model.getFilters().parallelStream().filter(p -> p.text.contains(layerName)).findFirst()
          .orElseGet(() -> {
            Filter filter = new Filter();
            filter.enable = false;
            filter.hiding = true;
            filter.text = "layer=" + layerName;
            model.addFilter(filter);
            return filter;
          });
        layerFilter.enable = !box.isSelected();
        int index = model.getFilters().indexOf(layerFilter);
        MapillaryExpertFilterDialog.getInstance().getFilterModel().fireTableRowsUpdated(index, index);
      }
    }
  }

  private void updateDetectionPage(int difference) {
    detectionPage += difference;
    if (detectionPage < 0) {
      detectionPage = 0;
    }
    long visible = buttons.parallelStream().filter(b -> b.isFiltered(filterField.getText())).count();
    while (detectionPage * showMaxNumberModel.getNumber().intValue() > visible) {
      detectionPage--;
    }
    updateShown(showMaxNumberModel);
  }

  /**
   * Update the shown buttons
   *
   * @param model The model used to paginate the buttons.
   */
  private void updateShown(SpinnerNumberModel model) {
    buttons.parallelStream().forEach(i -> SwingUtilities.invokeLater(() -> i.setVisible(false)));
    buttons.stream().filter(i -> checkRelevant(i, filterField.getText()))
      .skip(detectionPage * model.getNumber().longValue()).limit(model.getNumber().longValue())
      .forEach(i -> SwingUtilities.invokeLater(() -> i.setVisible(true)));
    long notSelected = buttons.parallelStream().filter(Component::isVisible).filter(i -> !i.isSelected()).count();
    long selected = buttons.parallelStream().filter(Component::isVisible).filter(ImageCheckBoxButton::isSelected)
      .count();
    toggleVisibleCheckbox.setSelected(notSelected < selected);
    toggleVisibleCheckbox.invalidate();
  }

  /**
   * Check if a button should be shown
   *
   * @param button The button to check
   * @param expr The filter text
   * @return {@code true} if the button should be shown
   */
  private boolean checkRelevant(ImageCheckBoxButton button, String expr) {
    boolean relevant = !this.showRelevant || button.isRelevant();
    boolean addable = !this.showAddable || ObjectDetections.getTaggingPresetsFor(button.getDetectionName()).length > 0;
    return button.isFiltered(expr) && (relevant && addable);
  }

  private static void createFirstLastSeen(JPanel panel, String firstLast) {
    JPanel firstSeen = new JPanel(new GridBagLayout());
    JPanel lastSeen = new JPanel(new GridBagLayout());
    panel.add(firstSeen);
    panel.add(lastSeen, GBC.eol());
    if ("first".equals(firstLast)) {
      firstSeen.add(new JLabel(I18n.tr("First Seen Start")), GBC.eol());
      lastSeen.add(new JLabel(I18n.tr("First Seen End")), GBC.eol());
    } else {
      firstSeen.add(new JLabel(I18n.tr("Last Seen Start")), GBC.eol());
      lastSeen.add(new JLabel(I18n.tr("Last Seen End")), GBC.eol());
    }

    final IDatePicker<?> firstSeenPicker = IDatePicker.getNewDatePicker();
    final IDatePicker<?> lastSeenPicker = IDatePicker.getNewDatePicker();
    firstSeen.add(firstSeenPicker.getComponent(), GBC.eol());
    lastSeen.add(lastSeenPicker.getComponent(), GBC.eol());
    firstSeenPicker.addEventHandler(t -> updateDates(firstLast, firstSeenPicker, firstSeenPicker, lastSeenPicker));
    lastSeenPicker.addEventHandler(t -> updateDates(firstLast, lastSeenPicker, firstSeenPicker, lastSeenPicker));
  }

  private static void updateDates(String position, IDatePicker<?> modified, IDatePicker<?> firstSeen,
    IDatePicker<?> lastSeen) {
    LocalDate start = firstSeen.getDate();
    LocalDate end = lastSeen.getDate();
    if (start != null && end != null) {
      if (firstSeen.equals(modified) && start.compareTo(end) > 0) {
        lastSeen.setDate(start);
      } else if (lastSeen.equals(modified) && start.compareTo(end) > 0) {
        lastSeen.setDate(end);
      }
    }
    Filter dateFilter = MapillaryExpertFilterDialog.getInstance().getFilterModel().getFilters().parallelStream()
      .filter(p -> p.text.contains(position + "_seen_at")).findFirst().orElseGet(Filter::new);
    StringBuilder filterText = new StringBuilder();
    if (start != null) {
      filterText.append(position).append("_seen_at > ").append(start.format(DateTimeFormatter.ISO_LOCAL_DATE));
    }
    if (start != null && end != null) {
      filterText.append(" && ");
    }
    if (end != null) {
      filterText.append(position).append("_seen_at < ").append(end.format(DateTimeFormatter.ISO_LOCAL_DATE));
    }
    dateFilter.text = filterText.toString();
    dateFilter.inverted = true;
    dateFilter.enable = !dateFilter.text.isEmpty();
    doFilterAddRemoveWork(dateFilter);
    List<Filter> toRemove = MapillaryExpertFilterDialog.getInstance().getFilterModel().getFilters().parallelStream()
      .filter(f -> f.text.isEmpty()).collect(Collectors.toList());
    for (Filter f : toRemove) {
      int index = MapillaryExpertFilterDialog.getInstance().getFilterModel().getFilters().indexOf(f);
      MapillaryExpertFilterDialog.getInstance().getFilterModel().removeFilter(index);
    }
  }

  private static void doFilterAddRemoveWork(Filter filter) {
    int index = MapillaryExpertFilterDialog.getInstance().getFilterModel().getFilters().indexOf(filter);
    if (index < 0 && filter.enable && !filter.text.isEmpty()) {
      MapillaryExpertFilterDialog.getInstance().getFilterModel().addFilter(filter);
    } else if (filter.enable) {
      MapillaryExpertFilterDialog.getInstance().getFilterModel().fireTableRowsUpdated(index, index);
    } else {
      MapillaryExpertFilterDialog.getInstance().getFilterModel().removeFilter(index);
    }

  }

  private void hideNearbyAddableObjs(boolean hideObjects) {
    Filter filter = MapillaryExpertFilterDialog.getInstance().getFilterModel().getFilters().parallelStream()
      .filter(f -> f.text.equals(NEARBY_KEY)).findAny().orElseGet(() -> {
        Filter nfilter = new Filter();
        nfilter.hiding = true;
        nfilter.text = NEARBY_KEY;
        return nfilter;
      });
    int index = MapillaryExpertFilterDialog.getInstance().getFilterModel().getFilters().indexOf(filter);

    if (hideObjects) {
      final Collection<ObjectDetections> osmEquivalentPossible = Stream.of(ObjectDetections.values())
        .filter(obj -> !obj.getOsmKeys().isEmpty()).collect(Collectors.toList());
      final double distance = Config.getPref().getDouble("mapillary.nearby_osm_objects", 20.0); // meters
      MainApplication.getLayerManager().getLayersOfType(PointObjectLayer.class).forEach(layer -> {
        DataSet dataSet = layer.getDataSet();
        boolean locked = dataSet.isLocked();
        try {
          dataSet.beginUpdate();
          dataSet.unlock();
          dataSet.allNonDeletedPrimitives().stream().filter(p -> !p.hasKey(NEARBY_KEY)
            && osmEquivalentPossible.contains(ObjectDetections.valueOfMapillaryValue(p.get("value")))).forEach(p -> {
              TagMap tags = ObjectDetections.valueOfMapillaryValue(p.get("value")).getOsmKeys();
              BBox searchBBox = p.getBBox();
              searchBBox.addPrimitive(p, distance / 111000); // convert meters to degrees (roughly)
              String nearby = MainApplication.getLayerManager().getLayersOfType(OsmDataLayer.class).stream()
                .map(OsmDataLayer::getDataSet).flatMap(d -> d.searchPrimitives(searchBBox).stream())
                .filter(pr -> !pr.isDeleted()).filter(pr -> tagMapIsSubset(pr.getKeys(), tags))
                .map(o -> o.getOsmPrimitiveId().toString()).collect(Collectors.joining(";"));
              if (nearby != null && !nearby.trim().isEmpty()) {
                p.put(NEARBY_KEY, nearby);
              }
            });
        } finally {
          if (locked) {
            dataSet.lock();
          }
          dataSet.endUpdate();
        }
      });
      filter.enable = true;
      if (index < 0) {
        MapillaryExpertFilterDialog.getInstance().getFilterModel().addFilter(filter);
      }
    } else {
      filter.enable = false;
      MapillaryExpertFilterDialog.getInstance().getFilterModel().removeFilter(index);
    }
  }

  /**
   * Check if a TagMap is a subset of another tagmap
   *
   * @param superSet The TagMap that should contain all of the other TagMap
   * @param subSet The TagMap that should be contained by the other TagMap
   * @return {@code true} if the subSet TagMap is actually a subset of the superSet TagMap.
   */
  private boolean tagMapIsSubset(TagMap superSet, TagMap subSet) {
    return subSet.entrySet().stream()
      .allMatch(t -> superSet.containsKey(t.getKey()) && superSet.get(t.getKey()).equals(t.getValue()));
  }

  private void showRelevantObjects(boolean showRelevant) {
    this.showRelevant = showRelevant;
    this.updateShownButtons();
  }

  private void showAddableObjects(boolean showAddable) {
    this.showAddable = showAddable;
    Collection<ImageCheckBoxButton> nonAddable = this.buttons.stream().filter(ImageCheckBoxButton::isRelevant)
      .filter(button -> Stream.of(button.getDetections()).allMatch(d -> d.getTaggingPresets().isEmpty()))
      .collect(Collectors.toList());
    this.updateShownButtons();
    GuiHelper.runInEDT(() -> {
      MapillaryExpertFilterDialog.getInstance().getFilterModel().pauseUpdates();
      nonAddable.forEach(b -> b.setSelected(this.showAddable));
      MapillaryExpertFilterDialog.getInstance().getFilterModel().resumeUpdates();
    });
  }

  private void updateShownButtons() {
    filterButtons(filterField.getText());
    updateShown(showMaxNumberModel);
  }

  private void toggleVisible(boolean check) {
    if (SwingUtilities.isEventDispatchThread()) {
      MainApplication.worker.execute(() -> toggleVisible(check));
      return;
    }
    MapillaryExpertFilterDialog.getInstance().getFilterModel().pauseUpdates();
    List<Future<?>> futures = buttons.stream().filter(ImageCheckBoxButton::isVisible).map(b -> b.setSelected(check))
      .filter(Objects::nonNull).collect(Collectors.toList());

    for (Future<?> future : futures) {
      try {
        future.get();
      } catch (InterruptedException e) {
        Logging.error(e);
        Thread.currentThread().interrupt();
      } catch (ExecutionException e) {
        Logging.error(e);
      }
    }
    MapillaryExpertFilterDialog.getInstance().getFilterModel().resumeUpdates();
  }

  private void addButtons() {
    JPanel scrollPane = new JPanel(new GridBagLayout());
    getIcons(scrollPane);
    add(scrollPane, GBC.eol().fill().anchor(GridBagConstraints.WEST));
  }

  /**
   * Filter buttons (along with a relevancy check).
   *
   * @param expr An expression to filter buttons with
   */
  private void filterButtons(String expr) {
    SwingUtilities.invokeLater(() -> buttons.stream().forEach(b -> b.setVisible(this.checkRelevant(b, expr))));
    SwingUtilities.invokeLater(this::invalidate);
  }

  public void getIcons(JComponent panel) {
    if (SwingUtilities.isEventDispatchThread()) {
      MainApplication.worker.execute(() -> getIcons(panel));
      return;
    }
    Map<String, List<ObjectDetections>> collected = Stream.of(ObjectDetections.values())
      .filter(v -> v != ObjectDetections.UNKNOWN).collect(Collectors.groupingBy(ObjectDetections::getBaseKey));
    collected = new TreeMap<>(collected); // Create a sorted map
    for (Map.Entry<String, List<ObjectDetections>> entry : collected.entrySet()) {
      final ImageIcon icon = entry.getValue().stream().map(detection -> {
        for (DetectionType type : detection.getDetectionTypes()) {
          if (type.getImageLocationString() == null)
            continue;
          ImageIcon tIcon = ImageProvider.getIfAvailable(type.getImageLocationString(), detection.getKey());
          if (tIcon != null) {
            return tIcon;
          }
        }
        return null;
      }).filter(Objects::nonNull).findFirst().orElse(null);
      if (icon != null) {
        GuiHelper.runInEDTAndWait(() -> {
          ImageCheckBoxButton button = new ImageCheckBoxButton(icon, entry.getKey(),
            entry.getValue().toArray(new ObjectDetections[0]));
          buttons.add(button);
          panel.add(button, GBC.eol().fill(GridBagConstraints.HORIZONTAL).anchor(GridBagConstraints.WEST));
        });
      }
    }
    Stream.of(showMaxNumberModel.getListeners(ChangeListener.class))
      .forEach(i -> SwingUtilities.invokeLater(() -> i.stateChanged(new ChangeEvent(this))));
  }

  @Override
  public void destroy() {
    if (!destroyed) {
      buttons.forEach(ImageCheckBoxButton::destroy);
      destroyed = true;
    }
  }

  /**
   * Reset everything
   */
  public void reset() {
    this.resetObjects.forEach(ResetListener::reset);
    List<Future<?>> futures = buttons.stream().map(b -> b.setSelected(false)).collect(Collectors.toList());
    futures.add(MainApplication.worker.submit(() -> {
      while (!MapillaryExpertFilterDialog.getInstance().getFilterModel().getFilters().isEmpty()) {
        MapillaryExpertFilterDialog.getInstance().getFilterModel().removeFilter(0);
      }
    }));
    Stream.of(getComponents()).forEach(this::resetSubPanels);
    MainApplication.worker.execute(() -> {
      for (Future<?> future : futures) {
        try {
          future.get();
        } catch (InterruptedException e) {
          Logging.error(e);
          Thread.currentThread().interrupt();
        } catch (ExecutionException e) {
          Logging.error(e);
          Logging.error(e.getCause());
        }
      }
    });

    // Remove the NEARBY_KEY tag
    MainApplication.getLayerManager().getLayersOfType(PointObjectLayer.class).forEach(layer -> {
      DataSet dataSet = layer.getDataSet();
      boolean locked = dataSet.isLocked();
      try {
        dataSet.unlock();
        dataSet.beginUpdate();
        dataSet.allPrimitives().stream().filter(p -> p.hasKey(NEARBY_KEY)).forEach(p -> p.remove(NEARBY_KEY));
      } finally {
        dataSet.endUpdate();
        if (locked) {
          dataSet.lock();
        }
      }
    });
  }

  private void resetSubPanels(Component component) {
    if (component instanceof IDatePicker) {
      ((IDatePicker<?>) component).reset();
    } else if (component instanceof JSpinner && ((JSpinner) component).getModel() instanceof SpinnerNumberModel
      && !((JSpinner) component).getModel().equals(showMaxNumberModel)) {
      ((SpinnerNumberModel) ((JSpinner) component).getModel()).setValue(0);
    }
    if (component instanceof JComponent) {
      Stream.of(((JComponent) component).getComponents()).forEach(this::resetSubPanels);
    }
  }

  @Override
  public void layerAdded(LayerAddEvent e) {
    if (e.getAddedLayer() instanceof PointObjectLayer) {
      PointObjectLayer layer = (PointObjectLayer) e.getAddedLayer();
      layer.getDataSet().addDataSourceListener(this);
    }
  }

  @Override
  public void layerRemoving(LayerRemoveEvent e) {
    if (e.getRemovedLayer() instanceof PointObjectLayer) {
      PointObjectLayer layer = (PointObjectLayer) e.getRemovedLayer();
      layer.getDataSet().removeDataSourceListener(this);
    }
  }

  @Override
  public void layerOrderChanged(LayerOrderChangeEvent e) {
    // Do nothing (we don't care about layer order change)
  }

  @Override
  public void dataSourceChange(DataSourceChangeEvent event) {
    if (Stream.of(this.show).anyMatch(Boolean.TRUE::equals)) {
      this.updateShownButtons();
    }
  }
}
