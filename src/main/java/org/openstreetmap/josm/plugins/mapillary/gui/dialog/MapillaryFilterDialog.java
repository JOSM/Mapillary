// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.dialog;

import static org.openstreetmap.josm.tools.I18n.marktr;
import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;
import java.awt.event.KeyEvent;
import java.io.Serial;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.function.Consumer;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.LongStream;
import java.util.stream.Stream;

import javax.swing.AbstractAction;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.SpinnerNumberModel;

import jakarta.annotation.Nonnull;
import org.openstreetmap.josm.data.Version;
import org.openstreetmap.josm.data.imagery.vectortile.mapbox.MVTTile;
import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.data.osm.IPrimitive;
import org.openstreetmap.josm.data.osm.Tagged;
import org.openstreetmap.josm.data.vector.VectorNode;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.SideButton;
import org.openstreetmap.josm.gui.dialogs.ToggleDialog;
import org.openstreetmap.josm.gui.layer.AbstractOsmDataLayer;
import org.openstreetmap.josm.gui.layer.MainLayerManager;
import org.openstreetmap.josm.gui.layer.OsmDataLayer;
import org.openstreetmap.josm.gui.util.GuiHelper;
import org.openstreetmap.josm.gui.widgets.JosmComboBox;
import org.openstreetmap.josm.plugins.datepicker.IDatePicker;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.OrganizationRecord;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.OrganizationRecord.OrganizationRecordListener;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryPreferenceSetting;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.PointObjectLayer;
import org.openstreetmap.josm.plugins.mapillary.gui.widget.DisableShortcutsOnFocusGainedJSpinner;
import org.openstreetmap.josm.plugins.mapillary.model.UserProfile;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;
import org.openstreetmap.josm.tools.Destroyable;
import org.openstreetmap.josm.tools.GBC;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.ListenerList;
import org.openstreetmap.josm.tools.Logging;
import org.openstreetmap.josm.tools.Shortcut;
import org.openstreetmap.josm.tools.Utils;

/**
 * ToggleDialog that lets you filter the images that are being shown.
 *
 * @author nokutu
 */
public final class MapillaryFilterDialog extends ToggleDialog
    implements OrganizationRecordListener, UserProfile.UserProfileListener, MVTTile.TileListener {

    @Serial
    private static final long serialVersionUID = -4192029663670922103L;

    private static MapillaryFilterDialog instance;

    private static final String[] TIME_LIST = { tr("Years"), tr("Months"), tr("Days") };

    private static final long[] TIME_FACTOR = new long[] { 31_536_000_000L, // = 365 * 24 * 60 * 60 * 1000 = number of
        // ms in a year
        2_592_000_000L, // = 30 * 24 * 60 * 60 * 1000 = number of ms in a month
        86_400_000 // = 24 * 60 * 60 * 1000 = number of ms in a day
    };

    /** Reset objects (reset done in order added) */
    private final transient ListenerList<ResetListener> resetObjects = ListenerList.create();
    private final transient ListenerList<Destroyable> destroyable = ListenerList.create();

    private final JLabel organizationLabel = new JLabel(tr("Org"));
    final JosmComboBox<OrganizationRecord> organizations = new JosmComboBox<>();

    private final JLabel userLabel = new JLabel(tr("User"));
    final JosmComboBox<UserProfile> users = new JosmComboBox<>();

    private boolean destroyed;

    private final transient ImageFilterPredicate shouldHidePredicate = new ImageFilterPredicate();
    private IDatePicker<JComponent> startDate;
    private IDatePicker<JComponent> endDate;

    private MapillaryFilterDialog() {
        super(tr("Mapillary filter"), "mapillary-filter", tr("Open Mapillary filter dialog"),
            Shortcut.registerShortcut("mapillary:filter:dialog", tr("Mapillary images Filter"), KeyEvent.CHAR_UNDEFINED,
                Shortcut.NONE),
            200, false, MapillaryPreferenceSetting.class);

        final var panel = new JPanel(new GridBagLayout());
        panel.add(new JLabel(tr("Picture Filters")), GBC.eol().anchor(GridBagConstraints.LINE_START));
        final var imageLine = new JPanel();
        panel.add(imageLine, GBC.eol().anchor(GridBagConstraints.LINE_START));
        this.addTimeFilters(panel);
        this.addUserGroupFilters(panel);
        final JComboBox<ImageTypes> imageTypes = new JComboBox<>();
        Stream.of(ImageTypes.values()).forEach(imageTypes::addItem);
        panel.add(new JLabel(tr("Show Image types: ")));
        panel.add(imageTypes, GBC.eol().anchor(GridBagConstraints.LINE_START));

        panel.add(new JSeparator(), GBC.eol().fill(GridBagConstraints.HORIZONTAL));
        final var objectFilter = new TrafficSignFilter();
        panel.add(new JLabel(tr("Object Detection Filters")),
            GBC.eol().anchor(GridBagConstraints.WEST).fill(GridBagConstraints.HORIZONTAL));
        this.destroyable.addListener(objectFilter);
        panel.add(objectFilter, GBC.eol().fill().anchor(GridBagConstraints.WEST));
        createLayout(panel, true, Arrays.asList(new SideButton(new UpdateAction()), new SideButton(new ResetAction())));

        // Add listeners for the shouldHidePredicate
        imageTypes
            .addItemListener(l -> this.shouldHidePredicate.imageTypes = (ImageTypes) imageTypes.getSelectedItem());

        // Add reset functions
        this.resetObjects.addListener(() -> imageTypes.setSelectedItem(ImageTypes.ALL));
        this.resetObjects.addListener(objectFilter::reset);

        // Set defaults for the shouldHidePredicate
        // This must be added last
        ResetListener setFields = () -> this.shouldHidePredicate.imageTypes = (ImageTypes) imageTypes.getSelectedItem();
        this.resetObjects.addListener(setFields);
        setFields.reset();
    }

    /**
     * Add user/organization filters
     *
     * @param panel The panel to add the filters to
     */
    private void addUserGroupFilters(JPanel panel) {
        final var userSearchPanel = new JPanel();
        userSearchPanel.setLayout(new FlowLayout(FlowLayout.LEFT));

        this.userLabel.setToolTipText(tr("Mapillary Users"));
        userSearchPanel.add(this.userLabel);
        userSearchPanel.add(this.users, GBC.eol());
        this.users.addItem(UserProfile.NONE);
        this.organizationLabel.setToolTipText(tr("Organizations"));
        userSearchPanel.add(this.organizationLabel);
        userSearchPanel.add(this.organizations);
        this.organizations.addItem(OrganizationRecord.NULL_RECORD);
        for (Component comp : Arrays.asList(this.userLabel, this.users, this.organizationLabel, this.organizations)) {
            comp.setEnabled(false);
        }
        this.users.setRenderer(new UserProfileListCellRenderer());
        this.organizations.setRenderer(new OrganizationListCellRenderer());
        panel.add(userSearchPanel, GBC.eol().anchor(GridBagConstraints.LINE_START));

        OrganizationRecord.addOrganizationListener(this);
        OrganizationRecord.getOrganizations().forEach(this::organizationAdded);
        UserProfile.addUserProfileListener(this);
        UserProfile.getUsers().forEach(this::userProfileAdded);

        this.organizations
            .addItemListener(l -> this.shouldHidePredicate.organization = (OrganizationRecord) l.getItem());
        this.users.addItemListener(l -> this.shouldHidePredicate.userProfile = (UserProfile) l.getItem());

        this.resetObjects.addListener(() -> organizations.setSelectedItem(OrganizationRecord.NULL_RECORD));
        ResetListener setListener = () -> this.shouldHidePredicate.organization = (OrganizationRecord) this.organizations
            .getSelectedItem();
        this.resetObjects.addListener(setListener);
        setListener.reset();
        this.resetObjects.addListener(() -> this.users.setSelectedItem(UserProfile.NONE));
        ResetListener userResetListener = () -> this.shouldHidePredicate.userProfile = (UserProfile) this.users
            .getSelectedItem();
        userResetListener.reset();
        this.resetObjects.addListener(userResetListener);
    }

    /**
     * Add time filters for pictures
     *
     * @param panel The panel to add the time filters to
     */
    private void addTimeFilters(JPanel panel) {
        // Time from panel
        final var fromPanel = new JPanel();
        fromPanel.setLayout(new FlowLayout(FlowLayout.LEFT));
        final var filterByDateCheckbox = new JCheckBox(tr("Not older than: "));
        fromPanel.add(filterByDateCheckbox);
        final var spinnerModel = new SpinnerNumberModel(1.0, 0, 10000, .1);
        final var spinner = new DisableShortcutsOnFocusGainedJSpinner(spinnerModel);
        // Set the editor such that we aren't zooming all over the place.
        spinner.setEnabled(false);
        fromPanel.add(spinner);

        final var time = new JComboBox<>(TIME_LIST);
        time.setEnabled(false);
        fromPanel.add(time);

        panel.add(fromPanel, GBC.eol().anchor(GridBagConstraints.LINE_START));
        // Time panel
        final var timePanel = new JPanel(new GridBagLayout());
        startDate = IDatePicker.getNewDatePicker();
        endDate = IDatePicker.getNewDatePicker();
        final Consumer<IDatePicker<?>> function = modified -> updateDates(startDate, endDate, modified);
        startDate.addEventHandler(function);
        endDate.addEventHandler(function);
        timePanel.add(new JLabel(tr("Start")), GBC.std().anchor(GridBagConstraints.LINE_START));
        timePanel.add(new JLabel(tr("End")), GBC.eol().anchor(GridBagConstraints.LINE_END));
        timePanel.add(startDate.getComponent(), GBC.std().anchor(GridBagConstraints.LINE_START));
        timePanel.add(endDate.getComponent(), GBC.eol().anchor(GridBagConstraints.LINE_END));
        final Dimension d = timePanel.getMinimumSize();
        d.width = (int) Math.ceil(d.width * 1.15);
        d.height = (int) Math.ceil(d.height * 1.15);
        timePanel.setMinimumSize(d);
        panel.add(timePanel, GBC.eol().anchor(GridBagConstraints.LINE_START));

        // Listeners
        filterByDateCheckbox.addItemListener(itemE -> {
            final boolean isSelected = filterByDateCheckbox.isSelected();
            spinner.setEnabled(isSelected);
            time.setEnabled(isSelected);
            if (isSelected) {
                this.startDate.setInstant(convertDateRangeBox(spinnerModel, time));
                this.shouldHidePredicate.startDateRefresh = this.startDate.getInstant();
            } else {
                this.startDate.reset();
                this.shouldHidePredicate.startDateRefresh = Instant.MIN;
            }
        });

        spinner.addChangeListener(l -> {
            this.startDate.setInstant(convertDateRangeBox(spinnerModel, time));
            this.shouldHidePredicate.startDateRefresh = this.startDate.getInstant();
        });
        time.addActionListener(l -> {
            this.startDate.setInstant(convertDateRangeBox(spinnerModel, time));
            this.shouldHidePredicate.startDateRefresh = this.startDate.getInstant();
        });
        filterByDateCheckbox.addChangeListener(l -> startDate.setInstant(convertDateRangeBox(spinnerModel, time)));

        endDate.addEventHandler(l -> this.shouldHidePredicate.endDateRefresh = l.getInstant());
        startDate.addEventHandler(l -> this.shouldHidePredicate.startDateRefresh = l.getInstant());
        filterByDateCheckbox
            .addItemListener(l -> this.shouldHidePredicate.timeFilter = l.getStateChange() == ItemEvent.SELECTED);
        time.addItemListener(l -> {
            if (time.getSelectedIndex() == 2) { // Days are the finest resolution we offer in the calendar
                spinnerModel.setStepSize(1);
                spinnerModel.setValue(Math.round(spinnerModel.getNumber().doubleValue()));
            } else {
                spinnerModel.setStepSize(0.1);
            }
        });

        this.resetObjects.addListener(() -> {
            filterByDateCheckbox.setSelected(false);
            filterByDateCheckbox.getItemListeners();
        });
        this.resetObjects.addListener(() -> time.setSelectedItem(TIME_LIST[0]));
        this.resetObjects.addListener(() -> spinnerModel.setValue(1));

        this.resetObjects.addListener(() -> {
            if (!endDate.getInstant().equals(Instant.MIN)) {
                endDate.reset();
            }
            if (!startDate.getInstant().equals(Instant.MIN)) {
                startDate.reset();
            }
        });

        ResetListener setFields = () -> {
            this.shouldHidePredicate.timeFilter = filterByDateCheckbox.isSelected();
            this.shouldHidePredicate.endDateRefresh = endDate.getInstant();
            this.shouldHidePredicate.startDateRefresh = startDate.getInstant();
        };
        this.resetObjects.addListener(setFields);
        setFields.reset();
    }

    /**
     * Set the start date for the filter
     *
     * @param start The start date
     */
    public void setStartDate(Instant start) {
        this.startDate.setInstant(start);
    }

    /**
     * Set the end date for the filter
     *
     * @param end The end date
     */
    public void setEndDate(Instant end) {
        this.endDate.setInstant(end);
    }

    /**
     * Set the organization to filter on
     *
     * @param organization The organization to filter on
     */
    public void setOrganization(String organization) {
        final var organizationRecord = OrganizationRecord.getOrganization(organization);
        this.organizations.setSelectedItem(organizationRecord);
    }

    @Nonnull
    private static Instant convertDateRangeBox(@Nonnull SpinnerNumberModel spinner,
        @Nonnull JComboBox<String> timeStep) {
        if (timeStep.isEnabled()) {
            final var current = LocalDate.now(ZoneOffset.UTC).atStartOfDay(ZoneOffset.UTC);
            final var type = (String) timeStep.getSelectedItem();
            final var start = spinner.getNumber();
            final var difference = new int[] { 0, 0, 0 }; // Year, Month, Day
            if (TIME_LIST[0].equals(type)) {
                difference[0] = start.intValue();
                difference[1] = (int) ((start.floatValue() - difference[0]) * 12);
                difference[2] = (int) (((start.floatValue() - difference[0]) * 12 - difference[1]) * 30);
            } else if (TIME_LIST[1].equals(type)) {
                difference[1] = start.intValue();
                difference[2] = (int) ((start.floatValue() - difference[1]) * 30);
            } else if (TIME_LIST[2].equals(type)) {
                difference[2] = start.intValue();
            }
            return current.minus(difference[0], ChronoUnit.YEARS).minus(difference[1], ChronoUnit.MONTHS)
                .minus(difference[2], ChronoUnit.DAYS).toInstant();
        }
        return Instant.MIN;
    }

    private static void updateDates(IDatePicker<?> startDate, IDatePicker<?> endDate, IDatePicker<?> modified) {
        final var start = startDate.getInstant();
        final var end = endDate.getInstant();
        if (Instant.MIN.equals(start) || Instant.MIN.equals(end)) {
            return;
        }
        if (startDate.equals(modified) && start.compareTo(end) > 0) {
            endDate.setInstant(start);
        } else if (endDate.equals(modified) && start.compareTo(end) > 0) {
            startDate.setInstant(end);
        }
    }

    /**
     * Get the instantiated instance of the dialog
     *
     * @return the unique instance of the class.
     */
    public static synchronized MapillaryFilterDialog getInstance() {
        if (instance != null) {
            return instance;
        }
        synchronized (MapillaryFilterDialog.class) {
            if (instance == null) {
                instance = new MapillaryFilterDialog();
            }
        }
        return instance;
    }

    /**
     * Check if the filter dialog has been created
     *
     * @return {@code true} if there is an already created instance
     */
    public static boolean hasInstance() {
        return instance != null;
    }

    /**
     * Resets the dialog to its default state.
     */
    public void reset() {
        this.resetObjects.fireEvent(ResetListener::reset);
        refresh();
    }

    /**
     * Applies the selected filter.
     */
    public synchronized void refresh() {
        // This predicate returns true if the image should be made invisible
        updateFilteredImages();
    }

    /**
     * Update filtered images
     */
    public void updateFilteredImages() {
        if (MapillaryLayer.hasInstance()) {
            MainApplication.worker.execute(() -> {
                final var readLock = MapillaryLayer.getInstance().getData().getReadLock();
                try {
                    readLock.lockInterruptibly();
                    this.updateFilteredImages(MapillaryLayer.getInstance().getData().getNodes());
                } catch (InterruptedException exception) {
                    Logging.error(exception);
                    Thread.currentThread().interrupt();
                } finally {
                    readLock.unlock();
                }
            });
        }
    }

    /**
     * Update filtered images
     *
     * @param nodeIterable The nodes to update
     * @param <N> The node type
     */
    private <N extends INode> void updateFilteredImages(Iterable<N> nodeIterable) {
        final ImageFilterPredicate shouldHide = this.shouldHidePredicate;
        shouldHide.updateLayerVisible();
        shouldHide.smartAdd = Boolean.TRUE.equals(MapillaryProperties.SMART_EDIT.get());
        Collection<IPrimitive> currentSelection = shouldHide.getCurrentSelection();
        for (N img : nodeIterable) {
            if (MapillaryImageUtils.isImage(img)) {
                img.setVisible(!shouldHide.test(img, currentSelection));
            }
        }
        GuiHelper.runInEDT(MapillaryLayer::invalidateInstance);
    }

    @Override
    public void finishedLoading(MVTTile tile) {
        updateFilteredImages(Utils.filteredCollection(tile.getData().getAllPrimitives(), VectorNode.class));
    }

    private static class ImageFilterPredicate implements Predicate<INode> {
        ImageTypes imageTypes;
        private boolean layerVisible;
        boolean timeFilter;
        Instant endDateRefresh;
        Instant startDateRefresh;
        OrganizationRecord organization;
        UserProfile userProfile;
        boolean smartAdd;

        public ImageFilterPredicate() {
            this.updateLayerVisible();
        }

        @Override
        public boolean test(INode img) {
            if (!layerVisible) {
                return true;
            }
            return test(img, getCurrentSelection());
        }

        Collection<IPrimitive> getCurrentSelection() {
            MainLayerManager layerManager = MainApplication.getLayerManager();
            return Stream
                .concat(layerManager.getLayersOfType(OsmDataLayer.class).stream().map(OsmDataLayer::getDataSet),
                    layerManager.getLayersOfType(PointObjectLayer.class).stream().map(PointObjectLayer::getData))
                .flatMap(ds -> ds.getAllSelected().stream()).collect(Collectors.toSet());
        }

        /**
         * Same as {@link #test(INode)}, but allows reuse of a selection collection (performance)
         *
         * @param img the image to test
         * @param currentSelection The currently selected detection(s)
         * @return {@code true} if the {@code img} should be filtered out
         */
        public boolean test(INode img, Collection<IPrimitive> currentSelection) {
            if (!layerVisible) {
                return true;
            }
            MainLayerManager layerManager = MainApplication.getLayerManager();
            if (this.smartAdd && MapillaryImageUtils.getKey(img) != 0
                && !layerManager.getLayersOfType(AbstractOsmDataLayer.class).isEmpty()) {
                Collection<Long> keys = currentSelection.stream().map(MapillaryUtils::getImagesFromDetections)
                    .flatMapToLong(LongStream::of).boxed().collect(Collectors.toSet());
                if (!keys.contains(MapillaryImageUtils.getKey(img))) {
                    return true;
                }
            }
            // Filter on time
            if (checkEndDate(img) || checkStartDate(img)
                || (this.imageTypes == ImageTypes.PANORAMIC && !MapillaryImageUtils.IS_PANORAMIC.test(img))
                || (this.imageTypes == ImageTypes.NON_PANORAMIC && MapillaryImageUtils.IS_PANORAMIC.test(img))) {
                return true;
            }
            if (MapillaryImageUtils.getKey(img) > 0 && MapillaryImageUtils.getSequenceKey(img) != null) {
                // Filter on organizations
                if (checkOrganization(img)) {
                    return true;
                }
                return checkUser(img);
            }
            return false;
        }

        /**
         * Update the layer visibility
         */
        final void updateLayerVisible() {
            this.layerVisible = MapillaryLayer.hasInstance() && MapillaryLayer.getInstance().isVisible();
        }

        /**
         * Check if the organization does not match
         *
         * @param img The image to check
         * @return {@code true} if the organization does not match
         */
        private boolean checkOrganization(INode img) {
            return this.organization != null && !OrganizationRecord.NULL_RECORD.equals(this.organization)
                && this.organization.id() != MapillaryImageUtils.getOrganization(img).id();
        }

        /**
         * Check if the user does not match
         *
         * @param img The image to check
         * @return {@code true} if the user does not match
         */
        private boolean checkUser(Tagged img) {
            return this.userProfile != null && !UserProfile.NONE.equals(this.userProfile)
                && this.userProfile.key() != MapillaryImageUtils.getUser(img).key();
        }

        /**
         * @param img The image to check
         * @return {@code true} if the start date is after the image date
         */
        private boolean checkStartDate(INode img) {
            if (Instant.MIN.equals(startDateRefresh)) {
                return false;
            }
            final var start = LocalDateTime.ofInstant(startDateRefresh, ZoneOffset.UTC).toLocalDate()
                .atStartOfDay(ZoneOffset.UTC).toInstant();
            final var imgDate = MapillaryImageUtils.getDate(img);
            return start.isAfter(imgDate);
        }

        /**
         * @param img The image to check
         * @return {@code true} if the end date is before the image date
         */
        private boolean checkEndDate(INode img) {
            if (Instant.MIN.equals(endDateRefresh)) {
                return false;
            }
            final var nextDate = LocalDateTime.ofInstant(endDateRefresh, ZoneOffset.UTC).toLocalDate()
                .atStartOfDay(ZoneOffset.UTC).plus(1, ChronoUnit.DAYS);
            final var end = nextDate.toInstant();
            final var imgDate = MapillaryImageUtils.getDate(img);
            return end.isBefore(imgDate);
        }
    }

    /**
     * Destroys the unique instance of the class.
     */
    public static synchronized void destroyInstance() {
        instance = null;
    }

    private static class UpdateAction extends AbstractAction {

        @Serial
        private static final long serialVersionUID = -7417238601979689863L;

        UpdateAction() {
            putValue(NAME, tr("Update"));
            new ImageProvider("dialogs", "refresh").getResource().attachImageIcon(this, true);
        }

        @Override
        public void actionPerformed(ActionEvent arg0) {
            MapillaryFilterDialog.getInstance().refresh();
        }
    }

    private static class ResetAction extends AbstractAction {
        @Serial
        private static final long serialVersionUID = 1178261778165525040L;

        ResetAction() {
            putValue(NAME, tr("Reset"));
            new ImageProvider("preferences", "reset").getResource().attachImageIcon(this, true);
        }

        @Override
        public void actionPerformed(ActionEvent arg0) {
            MapillaryFilterDialog.getInstance().reset();
        }
    }

    @Override
    public void destroy() {
        if (!destroyed) {
            super.destroy();
            this.destroyable.fireEvent(Destroyable::destroy);
            if (MainApplication.getMap() != null && Version.getInstance().getVersion() < 18686) {
                MainApplication.getMap().removeToggleDialog(this);
            }
            OrganizationRecord.removeOrganizationListener(this);
            UserProfile.removeUserProfileListener(this);
            destroyed = true;
        }
        destroyInstance();
    }

    @Override
    public void organizationAdded(OrganizationRecord organization) {
        if (organization != null && comboBoxDoesNotContainItem(organizations, organization)) {
            GuiHelper.runInEDT(() -> organizations.addItem(organization));
        }
        for (Component comp : Arrays.asList(organizationLabel, organizations)) {
            GuiHelper.runInEDT(() -> comp
                .setEnabled(organizations.getItemCount() > 1 || !OrganizationRecord.NULL_RECORD.equals(organization)));
        }
    }

    @Override
    public void userProfileAdded(UserProfile userProfile) {
        if (userProfile != null && comboBoxDoesNotContainItem(users, userProfile)) {
            GuiHelper.runInEDT(() -> {
                final var list = new ArrayList<>(users.getModel().asCollection());
                list.add(userProfile);
                list.sort(Comparator.comparing(UserProfile::username));
                final var model = users.getModel();
                model.removeAllElements();
                model.addAllElements(list);
            });
        }
        for (Component comp : Arrays.asList(userLabel, users)) {
            GuiHelper
                .runInEDT(() -> comp.setEnabled(users.getItemCount() > 1 || !UserProfile.NONE.equals(userProfile)));
        }
    }

    /**
     * Check if a combo box does <i>not</i> contain a specified item
     *
     * @param comboBox The combo box to look through
     * @param object The object to look for
     * @return {@code true} if the combobox did not contain the object
     * @param <T> The object type
     */
    private static <T> boolean comboBoxDoesNotContainItem(JComboBox<T> comboBox, T object) {
        for (var i = 0; i < comboBox.getItemCount(); i++) {
            if (comboBox.getItemAt(i).equals(object)) {
                return false;
            }
        }
        return true;
    }

    /**
     * Image types (pano, non-pano, and all)
     */
    private enum ImageTypes {
        ALL(marktr("All")), NON_PANORAMIC(marktr("Non-panoramic")), PANORAMIC(marktr("Panoramic"));

        private final String originalName;

        ImageTypes(String originalName) {
            this.originalName = originalName;
        }

        @Override
        public String toString() {
            return tr(this.originalName);
        }
    }
}
