// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.dialog;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ItemEvent;
import java.time.Instant;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.TreeMap;
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

import org.openstreetmap.josm.data.imagery.vectortile.mapbox.MVTTile;
import org.openstreetmap.josm.data.osm.BBox;
import org.openstreetmap.josm.data.osm.Filter;
import org.openstreetmap.josm.data.osm.IPrimitive;
import org.openstreetmap.josm.data.preferences.AbstractProperty.ValueChangeEvent;
import org.openstreetmap.josm.data.vector.VectorDataSet;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.layer.LayerManager.LayerAddEvent;
import org.openstreetmap.josm.gui.layer.LayerManager.LayerChangeListener;
import org.openstreetmap.josm.gui.layer.LayerManager.LayerOrderChangeEvent;
import org.openstreetmap.josm.gui.layer.LayerManager.LayerRemoveEvent;
import org.openstreetmap.josm.gui.layer.OsmDataLayer;
import org.openstreetmap.josm.gui.util.GuiHelper;
import org.openstreetmap.josm.gui.widgets.FilterField;
import org.openstreetmap.josm.plugins.datepicker.IDatePicker;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.ImageMode;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.ObjectDetections;
import org.openstreetmap.josm.plugins.mapillary.gui.DeveloperToggleAction;
import org.openstreetmap.josm.plugins.mapillary.gui.ImageCheckBoxButton;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.PointObjectLayer;
import org.openstreetmap.josm.plugins.mapillary.io.download.TileAddListener;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryMapFeatureUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.spi.preferences.Config;
import org.openstreetmap.josm.tools.Destroyable;
import org.openstreetmap.josm.tools.GBC;
import org.openstreetmap.josm.tools.I18n;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.Pair;
import org.openstreetmap.josm.tools.Utils;

/**
 * This filters traffic signs
 *
 * @author Taylor Smock
 */
public final class TrafficSignFilter extends JPanel
    implements Destroyable, LayerChangeListener, TileAddListener<MVTTile> {
    private static final long serialVersionUID = 1177890183422385423L;
    private boolean destroyed;
    private final List<ImageCheckBoxButton> buttons;
    private boolean showRelevant;
    private boolean smartEditModeEnabled = Boolean.TRUE.equals(MapillaryProperties.SMART_EDIT.get());
    private final Boolean[] show = new Boolean[] { showRelevant, smartEditModeEnabled };
    private final FilterField filterField;
    private final SpinnerNumberModel showMaxNumberModel;
    private int detectionPage;
    private final JCheckBox toggleVisibleCheckbox;
    private static final String NEARBY_KEY = "nearby_osm_objects";

    private final List<ResetListener> resetObjects = new ArrayList<>();

    /**
     * Create a new {@link TrafficSignFilter}
     */
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
            // TODO remove when it listens to layers.
            // Maybe get rid of Traffic Sign/Point object layer filters (no longer needed due to separate layers) OR
            // clicking on them adds the appropriate layer. Affect hide/show action of layers instead of adding filters?
            DeveloperToggleAction.addVisibilitySwitcher(lbox);
        }

        JCheckBox smartEditModeBox = new JCheckBox(I18n.tr("Smart Edit Mode"));
        smartEditModeBox.setToolTipText(
            I18n.tr("Add Mapillary Objects to OpenStreetMap; some additional client-side filtering will occur."));
        smartEditModeBox.setSelected(Boolean.TRUE.equals(MapillaryProperties.SMART_EDIT.get()));
        MapillaryProperties.SMART_EDIT.addListener(l -> updateSmartEdit(l, smartEditModeBox));
        smartEditModeBox.addItemListener(l -> smartEditMode(l.getStateChange() == ItemEvent.SELECTED));
        add(smartEditModeBox, GBC.eol().anchor(GridBagConstraints.WEST));

        add(layers, GBC.eol().fill(GridBagConstraints.HORIZONTAL));

        /* Filter signs */
        filterField = new FilterField().filter(this::filterButtons);
        filterField.setToolTipText(I18n.tr("Filter Mapillary Detections"));
        add(filterField, GBC.eol().fill(GridBagConstraints.HORIZONTAL));
        toggleVisibleCheckbox = new JCheckBox(I18n.tr("Select Visible"));
        this.toggleVisibleCheckbox.setToolTipText(I18n.tr("Select/Deselect all object detections on the current page"));
        JCheckBox showRelevantObjs = new JCheckBox(I18n.tr("Show Relevant"));
        showRelevantObjs.setToolTipText(I18n.tr("Only show objects on detection layers (\"Relevant objects\")"));
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
        previousButtonPagination.setToolTipText(I18n.tr("Show the previous page of items"));
        showMax.setToolTipText(I18n.tr("Show this number of items on each page"));
        nextButtonPagination.setToolTipText(I18n.tr("Show the next page of items"));
        pagination.add(showMax, GBC.std().anchor(GridBagConstraints.CENTER));
        pagination.add(nextButtonPagination, GBC.std().anchor(GridBagConstraints.WEST));
        showRelevantObjs.addItemListener(l -> showRelevantObjects(l.getStateChange() == ItemEvent.SELECTED));
        toggleVisibleCheckbox.addItemListener(l -> toggleVisible(l.getStateChange() == ItemEvent.SELECTED));
        add(showRelevantObjs, GBC.std().anchor(GridBagConstraints.WEST));
        add(toggleVisibleCheckbox, GBC.eol().anchor(GridBagConstraints.CENTER));

        this.buttons = Collections.synchronizedList(new ArrayList<>());
        addButtons();

        add(pagination, GBC.eol().anchor(GridBagConstraints.WEST).fill(GridBagConstraints.HORIZONTAL));

        this.resetObjects.add(() -> smartEditModeBox.setSelected(false));
        this.resetObjects.add(() -> showRelevantObjs.setSelected(true));
        this.resetObjects.add(() -> filterField.setText(""));
        this.resetObjects.add(() -> showMaxNumberModel.setValue(100));
        this.resetObjects.add(() -> MapillaryProperties.SMART_EDIT.put(false));

        if (!showRelevantObjs.isSelected()) {
            showRelevantObjs.doClick();
        }
        /* End filter signs */

        // TODO Default Smart Edit mode to off on each JOSM start, for now.
        if (MapillaryProperties.SMART_EDIT.isSet()) {
            this.reset();
        }

        MainApplication.getLayerManager().addAndFireLayerChangeListener(this);
    }

    private void updateSmartEdit(ValueChangeEvent<? extends Boolean> l, JCheckBox smartEditMode) {
        this.smartEditModeEnabled = Boolean.TRUE.equals(l.getProperty().get());
        smartEditMode.setSelected(this.smartEditModeEnabled);
        if (MapillaryFilterDialog.getInstance() != null) {
            MapillaryFilterDialog.getInstance().refresh();
        }
        if (this.smartEditModeEnabled) {
            ImageMode.setMode(ImageMode.SMART_EDIT);
        } else {
            ImageMode.setMode(ImageMode.NORMAL);
        }
    }

    private static void updateLayers(ItemEvent itemEvent) {
        if (itemEvent.getItem() instanceof JCheckBox) {
            JCheckBox box = (JCheckBox) itemEvent.getItem();
            String layerName = (String) box.getClientProperty("layer");
            if (layerName != null) {
                MapillaryFilterTableModel model = MapillaryExpertFilterDialog.getInstance().getFilterModel();
                Filter layerFilter = model.getFilters().parallelStream().filter(p -> p.text.contains(layerName))
                    .findFirst().orElseGet(() -> {
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
        long visible;
        synchronized (this.buttons) {
            final Collection<String> detections = MainApplication.getLayerManager()
                .getLayersOfType(PointObjectLayer.class).stream().map(PointObjectLayer::getData)
                .flatMap(ds -> ds.allPrimitives().stream()).map(MapillaryMapFeatureUtils::getValue)
                .filter(Objects::nonNull).collect(Collectors.toSet());
            visible = this.buttons.stream().filter(b -> checkRelevant(b, this.filterField.getText(), detections))
                .count();
        }
        while ((long) detectionPage * showMaxNumberModel.getNumber().intValue() > visible) {
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
        long notSelected;
        long selected;
        synchronized (this.buttons) {
            this.buttons.forEach(i -> SwingUtilities.invokeLater(() -> i.setVisible(false)));
            final Collection<String> detections = MainApplication.getLayerManager()
                .getLayersOfType(PointObjectLayer.class).stream().map(PointObjectLayer::getData)
                .flatMap(ds -> ds.allPrimitives().stream()).map(MapillaryMapFeatureUtils::getValue)
                .filter(Objects::nonNull).collect(Collectors.toSet());
            this.buttons.stream().filter(i -> checkRelevant(i, this.filterField.getText(), detections))
                .skip(this.detectionPage * model.getNumber().longValue()).limit(model.getNumber().longValue())
                .forEach(i -> SwingUtilities.invokeLater(() -> i.setVisible(true)));
            notSelected = this.buttons.stream().filter(Component::isVisible).filter(i -> !i.isSelected()).count();
            selected = this.buttons.stream().filter(Component::isVisible).filter(ImageCheckBoxButton::isSelected)
                .count();
        }
        toggleVisibleCheckbox.setSelected(notSelected < selected);
        toggleVisibleCheckbox.invalidate();
    }

    /**
     * Check if a button should be shown
     *
     * @param button The button to check
     * @param expr The filter text
     * @param detections The detections to use to check and see if the button is relevant
     * @return {@code true} if the button should be shown
     */
    private boolean checkRelevant(ImageCheckBoxButton button, String expr, Collection<String> detections) {
        boolean relevant = !this.showRelevant || button.isRelevant(detections);
        boolean addable = Boolean.FALSE.equals(MapillaryProperties.SMART_EDIT.get())
            || ObjectDetections.getTaggingPresetsFor(button.getDetectionName()).length > 0;
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
        Instant start = firstSeen.getInstant();
        Instant end = lastSeen.getInstant();
        final boolean knownStartEnd = !Instant.MIN.equals(start) && !Instant.MIN.equals(end);
        if (knownStartEnd) {
            if (firstSeen.equals(modified) && start.compareTo(end) > 0) {
                lastSeen.setInstant(start);
            } else if (lastSeen.equals(modified) && start.compareTo(end) > 0) {
                lastSeen.setInstant(end);
            }
        }
        Filter dateFilter = MapillaryExpertFilterDialog.getInstance().getFilterModel().getFilters().parallelStream()
            .filter(p -> p.text.contains(position + "_seen_at")).findFirst().orElseGet(Filter::new);
        StringBuilder filterText = new StringBuilder();
        if (!Instant.MIN.equals(start)) {
            filterText.append(position).append("_seen_at > ")
                .append(DateTimeFormatter.ISO_LOCAL_DATE.withZone(ZoneId.of("UTC")).format(start));
        }
        if (knownStartEnd) {
            filterText.append(" && ");
        }
        if (!Instant.MIN.equals(end)) {
            filterText.append(position).append("_seen_at < ")
                .append(DateTimeFormatter.ISO_LOCAL_DATE.withZone(ZoneId.of("UTC")).format(end));
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

    private static void hideNearbyAddableObjs(boolean hideObjects) {
        MapillaryFilterTableModel model = MapillaryExpertFilterDialog.getInstance().getFilterModel();
        Filter filter = model.getFilters().stream().filter(f -> NEARBY_KEY.equals(f.text)).findAny().orElseGet(() -> {
            Filter nfilter = new Filter();
            nfilter.hiding = true;
            nfilter.text = NEARBY_KEY;
            return nfilter;
        });

        int index = model.getFilters().indexOf(filter);

        if (hideObjects) {
            MainApplication.getLayerManager().getLayersOfType(PointObjectLayer.class)
                .forEach(TrafficSignFilter::updateNearbyOsmKey);
            filter.enable = true;
            if (index < 0) {
                model.addFilter(filter);
            }
        } else {
            filter.enable = false;
            if (index >= 0) {
                model.removeFilter(index);
            }
        }
    }

    private static void updateNearbyOsmKey(PointObjectLayer layer) {
        VectorDataSet dataSet = layer.getData();
        boolean locked = dataSet.isLocked();
        try {
            dataSet.unlock();
            updateNearbyOsmKey(dataSet.allNonDeletedPrimitives());
        } finally {
            if (locked) {
                dataSet.lock();
            }
        }
    }

    /**
     * Update the {@link #NEARBY_KEY} value for detections (uses all {@link OsmDataLayer}s in
     * {@link MainApplication#getLayerManager()})
     *
     * @param primitives The primitives to update
     */
    private static void updateNearbyOsmKey(Collection<? extends IPrimitive> primitives) {
        final double distance = Config.getPref().getDouble("mapillary.nearby_osm_objects", 15.0); // meters
        primitives.stream()
            .filter(p -> !p.hasKey(NEARBY_KEY)
                && ObjectDetections.valueOfMapillaryValue(MapillaryMapFeatureUtils.getValue(p)).hasOsmKeys())
            .forEach(p -> {
                Map<String, String> tags = ObjectDetections.valueOfMapillaryValue(MapillaryMapFeatureUtils.getValue(p))
                    .getOsmKeys();
                BBox searchBBox = new BBox(p.getBBox());
                searchBBox.addPrimitive(p, distance / 111_000); // convert meters to degrees (roughly)
                String nearby = MainApplication.getLayerManager().getLayersOfType(OsmDataLayer.class).stream()
                    .map(OsmDataLayer::getDataSet).flatMap(d -> d.searchPrimitives(searchBBox).stream())
                    .filter(pr -> !pr.isDeleted()).filter(pr -> tagMapIsSubset(pr.getKeys(), tags))
                    .map(o -> o.getOsmPrimitiveId().toString()).collect(Collectors.joining(";"));
                if (!Utils.isBlank(nearby)) {
                    p.put(NEARBY_KEY, nearby);
                }
            });
    }

    /**
     * Check if a TagMap is a subset of another tagmap
     *
     * @param superSet The TagMap that should contain all of the other TagMap
     * @param subSet The TagMap that should be contained by the other TagMap
     * @return {@code true} if the subSet TagMap is actually a subset of the superSet TagMap.
     */
    private static boolean tagMapIsSubset(Map<String, String> superSet, Map<String, String> subSet) {
        return subSet.entrySet().stream()
            .allMatch(t -> superSet.containsKey(t.getKey()) && superSet.get(t.getKey()).equals(t.getValue()));
    }

    private void showRelevantObjects(boolean showRelevant) {
        this.showRelevant = showRelevant;
        this.updateShownButtons();
    }

    /**
     * Turn smart edit mode on or off
     *
     * @param smartEditMode {@code true} to turn smart edit mode on
     */
    private void smartEditMode(boolean smartEditMode) {
        SwingUtilities.invokeLater(() -> MapillaryProperties.SMART_EDIT.put(smartEditMode));
        this.smartEditModeEnabled = smartEditMode;
        this.updateSmartEditFilters();
    }

    /**
     * Update the filters
     */
    private void updateSmartEditFilters() {
        MapillaryFilterTableModel filterModel = MapillaryExpertFilterDialog.getInstance().getFilterModel();
        filterModel.selectionModel.clearSelection();
        filterModel.clearFilters();
        if (filterModel.notManyChanges()) {
            hideNearbyAddableObjs(this.smartEditModeEnabled);
        }
        final Collection<ImageCheckBoxButton> nonAddable;
        synchronized (this.buttons) {
            List<PointObjectLayer> layers = MainApplication.getLayerManager().getLayersOfType(PointObjectLayer.class);
            Set<String> detections = layers.stream().map(PointObjectLayer::getData)
                .flatMap(ds -> ds.allPrimitives().stream()).map(MapillaryMapFeatureUtils::getValue)
                .filter(Objects::nonNull).collect(Collectors.toSet());
            nonAddable = this.buttons.stream().filter(button -> button.isRelevant(detections))
                .filter(button -> Stream.of(button.getDetections())
                    .allMatch(d -> !d.shouldBeAddable() || d.getTaggingPresets().isEmpty()))
                .collect(Collectors.toList());
        }
        this.updateShownButtons();
        filterModel.doManyUpdates(() -> nonAddable.forEach(b -> b.setSelected(this.smartEditModeEnabled)));
    }

    private void updateShownButtons() {
        filterButtons(filterField.getText());
        updateShown(showMaxNumberModel);
    }

    private void toggleVisible(boolean check) {
        MapillaryExpertFilterDialog.getInstance().getFilterModel().doManyUpdates(() -> {
            synchronized (this.buttons) {
                this.buttons.stream().filter(ImageCheckBoxButton::isVisible).forEach(b -> b.setSelected(check));
            }
        });
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
        synchronized (this.buttons) {
            final Collection<String> detections = MainApplication.getLayerManager()
                .getLayersOfType(PointObjectLayer.class).stream().map(PointObjectLayer::getData)
                .flatMap(ds -> ds.allPrimitives().stream()).map(MapillaryMapFeatureUtils::getValue)
                .filter(Objects::nonNull).collect(Collectors.toSet());
            this.buttons.stream().map(checkbox -> new Pair<>(checkbox, this.checkRelevant(checkbox, expr, detections)))
                .forEach(pair -> GuiHelper.runInEDT(() -> pair.a.setVisible(pair.b)));
        }
        GuiHelper.runInEDT(this::invalidate);
    }

    /**
     * Get the {@link ImageIcon}s for the panel
     *
     * @param panel The panel to add the {@link ImageIcon} to
     */
    private void getIcons(JComponent panel) {
        Map<String, List<ObjectDetections>> collected = Stream.of(ObjectDetections.values())
            .filter(v -> v != ObjectDetections.UNKNOWN).collect(Collectors.groupingBy(ObjectDetections::getBaseKey));
        collected = new TreeMap<>(collected); // Create a sorted map
        for (Map.Entry<String, List<ObjectDetections>> entry : collected.entrySet()) {
            final ImageProvider icon = entry.getValue().stream().map(ObjectDetections::getImageProvider)
                .filter(Objects::nonNull).findFirst().orElse(null);
            ImageCheckBoxButton button = new ImageCheckBoxButton(icon, entry.getKey(),
                entry.getValue().toArray(new ObjectDetections[0]));
            buttons.add(button);
            panel.add(button, GBC.eol().fill(GridBagConstraints.HORIZONTAL).anchor(GridBagConstraints.WEST));
        }
        Stream.of(showMaxNumberModel.getListeners(ChangeListener.class))
            .forEach(i -> i.stateChanged(new ChangeEvent(this)));
    }

    @Override
    public void destroy() {
        if (!destroyed) {
            synchronized (this.buttons) {
                this.buttons.forEach(ImageCheckBoxButton::destroy);
            }
            destroyed = true;
        }
    }

    /**
     * Reset everything
     */
    public void reset() {
        this.resetObjects.forEach(ResetListener::reset);
        final MapillaryFilterTableModel filterModel = MapillaryExpertFilterDialog.getInstance().getFilterModel();
        filterModel.doManyUpdates(() -> {
            synchronized (this.buttons) {
                this.buttons.forEach(b -> b.setSelected(false));
            }
            filterModel.clearFilters();
        });
        Stream.of(getComponents()).forEach(this::resetSubPanels);

        // Remove the NEARBY_KEY tag
        MainApplication.getLayerManager().getLayersOfType(PointObjectLayer.class).forEach(layer -> {
            VectorDataSet dataSet = layer.getData();
            boolean locked = dataSet.isLocked();
            try {
                dataSet.unlock();
                dataSet.allPrimitives().stream().filter(p -> p.hasKey(NEARBY_KEY)).forEach(p -> p.remove(NEARBY_KEY));
            } finally {
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
            ((JSpinner) component).getModel().setValue(0);
        }
        if (component instanceof JComponent) {
            Stream.of(((JComponent) component).getComponents()).forEach(this::resetSubPanels);
        }
    }

    @Override
    public void layerAdded(LayerAddEvent e) {
        if (e.getAddedLayer() instanceof PointObjectLayer) {
            PointObjectLayer layer = (PointObjectLayer) e.getAddedLayer();
            layer.addListener(this);
        }
    }

    @Override
    public void layerRemoving(LayerRemoveEvent e) {
        if (e.getRemovedLayer() instanceof PointObjectLayer) {
            PointObjectLayer layer = (PointObjectLayer) e.getRemovedLayer();
            layer.removeListener(this);
        }
    }

    @Override
    public void layerOrderChanged(LayerOrderChangeEvent e) {
        // Do nothing (we don't care about layer order change)
    }

    @Override
    public void tileAdded(MVTTile tile) {
        if (Arrays.asList(this.show).contains(Boolean.TRUE)) {
            this.updateShownButtons();
        }
        if (this.smartEditModeEnabled) {
            updateNearbyOsmKey(tile.getData().getAllPrimitives());
            // This update needs to be run in the EDT to avoid deadlocks
            GuiHelper.runInEDT(this::updateSmartEditFilters);
        }
    }
}
