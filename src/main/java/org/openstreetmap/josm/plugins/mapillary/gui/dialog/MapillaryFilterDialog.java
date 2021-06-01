// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.dialog;

import org.openstreetmap.josm.actions.ExpertToggleAction;
import org.openstreetmap.josm.actions.ExpertToggleAction.ExpertModeChangeListener;
import org.openstreetmap.josm.data.imagery.vectortile.mapbox.MVTTile;
import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.data.osm.IPrimitive;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.SideButton;
import org.openstreetmap.josm.gui.dialogs.ToggleDialog;
import org.openstreetmap.josm.gui.layer.AbstractOsmDataLayer;
import org.openstreetmap.josm.gui.layer.MainLayerManager;
import org.openstreetmap.josm.gui.layer.OsmDataLayer;
import org.openstreetmap.josm.gui.util.GuiHelper;
import org.openstreetmap.josm.gui.widgets.DisableShortcutsOnFocusGainedTextField;
import org.openstreetmap.josm.gui.widgets.JosmTextField;
import org.openstreetmap.josm.plugins.datepicker.IDatePicker;
import org.openstreetmap.josm.plugins.mapillary.cache.Caches;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.OrganizationRecord;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.OrganizationRecord.OrganizationRecordListener;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryFilterChooseSigns;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryPreferenceSetting;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.PointObjectLayer;
import org.openstreetmap.josm.plugins.mapillary.model.ImageDetection;
import org.openstreetmap.josm.plugins.mapillary.model.UserProfile;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryKeys;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;
import org.openstreetmap.josm.tools.Destroyable;
import org.openstreetmap.josm.tools.GBC;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.ListenerList;
import org.openstreetmap.josm.tools.Shortcut;

import javax.swing.AbstractAction;
import javax.swing.DefaultListCellRenderer;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.JSpinner;
import javax.swing.SpinnerNumberModel;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Image;
import java.awt.event.ActionEvent;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.ItemEvent;
import java.awt.event.KeyEvent;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.temporal.ChronoUnit;
import java.util.Arrays;
import java.util.Collection;
import java.util.function.Consumer;
import java.util.function.Predicate;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.openstreetmap.josm.tools.I18n.tr;

/**
 * ToggleDialog that lets you filter the images that are being shown.
 *
 * @author nokutu
 * @see MapillaryFilterChooseSigns
 */
public final class MapillaryFilterDialog extends ToggleDialog
  implements OrganizationRecordListener, MVTTile.TileListener {

  private static final long serialVersionUID = -4192029663670922103L;

  private static MapillaryFilterDialog instance;

  private static final String[] TIME_LIST = { tr("Years"), tr("Months"), tr("Days") };

  private static final long[] TIME_FACTOR = new long[] { 31_536_000_000L, // = 365 * 24 * 60 * 60 * 1000 = number of ms
                                                                          // in a year
    2_592_000_000L, // = 30 * 24 * 60 * 60 * 1000 = number of ms in a month
    86_400_000 // = 24 * 60 * 60 * 1000 = number of ms in a day
  };

  /** Reset objects (reset done in order added) */
  private final ListenerList<ResetListener> resetObjects = ListenerList.create();
  private final ListenerList<Destroyable> destroyable = ListenerList.create();

  private final JLabel organizationLabel = new JLabel(tr("Org"));
  final JComboBox<OrganizationRecord> organizations = new JComboBox<>();

  private boolean destroyed;

  private final transient ImageFilterPredicate shouldHidePredicate = new ImageFilterPredicate();

  private MapillaryFilterDialog() {
    super(tr("Mapillary filter"), "mapillary-filter", tr("Open Mapillary filter dialog"),
      Shortcut.registerShortcut("mapillary:filter:dialog", tr("Mapillary images Filter"), KeyEvent.CHAR_UNDEFINED,
        Shortcut.NONE),
      200, false, MapillaryPreferenceSetting.class);

    final JButton signChooser = new JButton(new SignChooserAction());
    final JCheckBox downloaded = new JCheckBox(tr("Downloaded images"));
    final JCheckBox onlySigns = new JCheckBox(tr("Only images with signs"));
    downloaded.addItemListener(l -> onlySigns.setEnabled(l.getStateChange() == ItemEvent.SELECTED));
    onlySigns.addItemListener(l -> signChooser.setEnabled(l.getStateChange() == ItemEvent.SELECTED));

    signChooser.setEnabled(false);
    final JPanel signChooserPanel = new JPanel();
    signChooserPanel.setLayout(new FlowLayout(FlowLayout.LEFT));
    signChooserPanel.add(signChooser);

    final JCheckBox imported = new JCheckBox(tr("Imported images"));
    imported.setSelected(true);
    downloaded.setSelected(true);

    final JPanel panel = new JPanel(new GridBagLayout());
    panel.add(new JLabel(tr("Picture Filters")), GBC.eol().anchor(GridBagConstraints.LINE_START));
    final JPanel imageLine = new JPanel();
    imageLine.add(downloaded, GBC.std().anchor(GridBagConstraints.LINE_START));
    imageLine.add(imported, GBC.eol());
    panel.add(imageLine, GBC.eol().anchor(GridBagConstraints.LINE_START));
    this.addTimeFilters(panel);
    this.addUserGroupFilters(panel);
    this.addCameraMakeModelFilters(panel);
    this.addImageQualityFilters(panel);
    final JPanel signs = new JPanel();
    signs.add(onlySigns, GBC.std().anchor(GridBagConstraints.LINE_START));
    signs.add(signChooserPanel, GBC.eol().anchor(GridBagConstraints.LINE_START));
    panel.add(signs, GBC.eol().anchor(GridBagConstraints.LINE_START));
    final JCheckBox onlyPano = new JCheckBox(tr("Panorama only"));
    panel.add(onlyPano, GBC.eol().anchor(GridBagConstraints.LINE_START));

    panel.add(new JSeparator(), GBC.eol().fill(GridBagConstraints.HORIZONTAL));
    final TrafficSignFilter objectFilter = new TrafficSignFilter();
    panel.add(new JLabel(tr("Object Detection Filters")),
      GBC.eol().anchor(GridBagConstraints.WEST).fill(GridBagConstraints.HORIZONTAL));
    this.destroyable.addListener(objectFilter);
    panel.add(objectFilter, GBC.eol().fill().anchor(GridBagConstraints.WEST));
    createLayout(panel, true, Arrays.asList(new SideButton(new UpdateAction()), new SideButton(new ResetAction())));

    // Add listeners for the shouldHidePredicate
    imported
      .addItemListener(l -> this.shouldHidePredicate.importedIsSelected = l.getStateChange() == ItemEvent.SELECTED);
    downloaded
      .addItemListener(l -> this.shouldHidePredicate.downloadedIsSelected = l.getStateChange() == ItemEvent.SELECTED);
    onlyPano
      .addItemListener(l -> this.shouldHidePredicate.onlyPanoIsSelected = l.getStateChange() == ItemEvent.SELECTED);
    onlySigns
      .addItemListener(l -> this.shouldHidePredicate.onlySignsIsSelected = l.getStateChange() == ItemEvent.SELECTED);

    // Add reset functions
    this.resetObjects.addListener(() -> imported.setSelected(true));
    this.resetObjects.addListener(() -> downloaded.setSelected(true));
    this.resetObjects.addListener(() -> onlySigns.setEnabled(true));
    this.resetObjects.addListener(() -> onlySigns.setSelected(false));
    this.resetObjects.addListener(() -> onlyPano.setSelected(false));
    this.resetObjects.addListener(() -> signChooser.setEnabled(false));
    this.resetObjects.addListener(objectFilter::reset);
    this.resetObjects.addListener(MapillaryFilterChooseSigns::reset);

    // Set defaults for the shouldHidePredicate
    // This must be added last
    ResetListener setFields = () -> {
      this.shouldHidePredicate.importedIsSelected = imported.isSelected();
      this.shouldHidePredicate.downloadedIsSelected = downloaded.isSelected();
      this.shouldHidePredicate.onlyPanoIsSelected = onlyPano.isSelected();
      this.shouldHidePredicate.onlySignsIsSelected = onlySigns.isSelected();
    };
    this.resetObjects.addListener(setFields);
    setFields.reset();
  }

  /**
   * Add camera make/model filters
   *
   * @param panel The panel to add the filters to
   */
  private void addCameraMakeModelFilters(JPanel panel) {
    JLabel cameraMakeLabel = new JLabel(tr("Camera Make"));
    panel.add(cameraMakeLabel, GBC.std().anchor(GridBagConstraints.LINE_START));
    final JosmTextField cameraMake = new JosmTextField();
    panel.add(cameraMake, GBC.std().fill(GridBagConstraints.HORIZONTAL).anchor(GridBagConstraints.LINE_START));
    JLabel cameraModelLabel = new JLabel("Model");
    panel.add(cameraModelLabel, GBC.std().anchor(GridBagConstraints.LINE_START));
    final JosmTextField cameraModel = new JosmTextField();
    panel.add(cameraModel, GBC.eol().fill(GridBagConstraints.HORIZONTAL).anchor(GridBagConstraints.LINE_START));

    cameraMake.addFocusListener(new FocusListener() {
      @Override
      public void focusGained(FocusEvent e) {
        // Do nothing
      }

      @Override
      public void focusLost(FocusEvent e) {
        shouldHidePredicate.cameraMake = cameraMake.getText();
      }
    });

    cameraModel.addFocusListener(new FocusListener() {
      @Override
      public void focusGained(FocusEvent e) {
        // Do nothing
      }

      @Override
      public void focusLost(FocusEvent e) {
        shouldHidePredicate.cameraModel = cameraModel.getText();
      }
    });
    this.resetObjects.addListener(() -> cameraMake.setText(""));
    this.resetObjects.addListener(() -> cameraModel.setText(""));

    ResetListener setFields = () -> {
      this.shouldHidePredicate.cameraMake = cameraMake.getText();
      this.shouldHidePredicate.cameraModel = cameraModel.getText();
    };
    this.resetObjects.addListener(setFields);
    setFields.reset();

    ExpertToggleAction.addVisibilitySwitcher(cameraMakeLabel);
    ExpertToggleAction.addVisibilitySwitcher(cameraMake);
    ExpertToggleAction.addVisibilitySwitcher(cameraModelLabel);
    ExpertToggleAction.addVisibilitySwitcher(cameraModel);
    ExpertModeChangeListener expertModeChangeListener = l -> setFields.reset();
    ExpertToggleAction.addExpertModeChangeListener(expertModeChangeListener);
    this.destroyable.addListener(() -> ExpertToggleAction.removeExpertModeChangeListener(expertModeChangeListener));
    this.destroyable.addListener(() -> {
      ExpertToggleAction.removeVisibilitySwitcher(cameraMakeLabel);
      ExpertToggleAction.removeVisibilitySwitcher(cameraMake);
      ExpertToggleAction.removeVisibilitySwitcher(cameraModelLabel);
      ExpertToggleAction.removeVisibilitySwitcher(cameraModel);
    });
  }

  /**
   * Add image quality filters
   *
   * @param panel The panel to add the filters to
   */
  private void addImageQualityFilters(JPanel panel) {
    final JCheckBox qualityCheck = new JCheckBox(tr("Image Quality"));
    final SpinnerNumberModel spinnerQualityModel = new SpinnerNumberModel(3, 1, 5, 1);
    final JSpinner spinnerQuality = new JSpinner(spinnerQualityModel);
    qualityCheck.addItemListener(l -> spinnerQuality.setEnabled(l.getStateChange() == ItemEvent.SELECTED));
    spinnerQuality.setEnabled(qualityCheck.isSelected());
    panel.add(qualityCheck, GBC.std().anchor(GridBagConstraints.LINE_START));
    panel.add(spinnerQuality, GBC.eol());

    spinnerQualityModel
      .addChangeListener(l -> this.shouldHidePredicate.qualityScore = spinnerQualityModel.getNumber().intValue());
    this.resetObjects.addListener(() -> spinnerQualityModel.setValue(3));
    this.resetObjects.addListener(() -> qualityCheck.setSelected(false));
    this.resetObjects.addListener(() -> spinnerQuality.setEnabled(false));
    ResetListener setFields = () -> this.shouldHidePredicate.qualityScore = Integer.MIN_VALUE;
    this.resetObjects.addListener(setFields);
    setFields.reset();

    ExpertToggleAction.addVisibilitySwitcher(qualityCheck);
    ExpertToggleAction.addVisibilitySwitcher(spinnerQuality);
    ExpertModeChangeListener expertModeChangeListener = l -> setFields.reset();
    ExpertToggleAction.addExpertModeChangeListener(expertModeChangeListener);
    this.destroyable.addListener(() -> ExpertToggleAction.removeExpertModeChangeListener(expertModeChangeListener));
    this.destroyable.addListener(() -> {
      ExpertToggleAction.removeVisibilitySwitcher(qualityCheck);
      ExpertToggleAction.removeVisibilitySwitcher(spinnerQuality);
    });

  }

  /**
   * Add user/organization filters
   *
   * @param panel The panel to add the filters to
   */
  private void addUserGroupFilters(JPanel panel) {
    final JPanel userSearchPanel = new JPanel();
    userSearchPanel.setLayout(new FlowLayout(FlowLayout.LEFT));

    final JosmTextField user = new DisableShortcutsOnFocusGainedTextField(10);
    user.addActionListener(new UpdateAction());
    userSearchPanel.add(new JLabel(tr("User")));
    userSearchPanel.add(user, GBC.eol());
    organizationLabel.setToolTipText(tr("Organizations"));
    userSearchPanel.add(organizationLabel);
    userSearchPanel.add(this.organizations);
    organizations.addItem(OrganizationRecord.NULL_RECORD);
    for (Component comp : Arrays.asList(organizationLabel, organizations)) {
      comp.setEnabled(false);
    }
    organizations.setRenderer(new DefaultListCellRenderer() {
      private static final long serialVersionUID = -1650696801628131389L;

      @Override
      public Component getListCellRendererComponent(JList<?> list, Object value, int index, boolean isSelected,
        boolean cellHasFocus) {
        JLabel comp = (JLabel) super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
        if (value instanceof OrganizationRecord) {
          OrganizationRecord organization = (OrganizationRecord) value;
          if (organization.getNiceName() != null && !organization.getNiceName().isEmpty()) {
            comp.setText(organization.getNiceName());
          } else {
            comp.setText(organization.getKey());
          }
          if (organization.getAvatar() != null) {
            final ImageProvider.ImageSizes size = ImageProvider.ImageSizes.DEFAULT;
            final Image scaledImage = organization.getAvatar().getImage().getScaledInstance(size.getAdjustedWidth(),
              size.getAdjustedHeight(), Image.SCALE_SMOOTH);
            comp.setIcon(new ImageIcon(scaledImage));
          }
        }
        return comp;
      }

    });
    panel.add(userSearchPanel, GBC.eol().anchor(GridBagConstraints.LINE_START));

    OrganizationRecord.addOrganizationListener(this);
    OrganizationRecord.getOrganizations().forEach(this::organizationAdded);

    // Listeners
    user.addFocusListener(new FocusListener() {
      @Override
      public void focusGained(FocusEvent e) {
        // Do nothing
      }

      @Override
      public void focusLost(FocusEvent e) {
        shouldHidePredicate.user = user.getText();
      }
    });
    this.organizations.addItemListener(l -> this.shouldHidePredicate.organization = (OrganizationRecord) l.getItem());

    this.resetObjects.addListener(() -> user.setText(""));
    this.resetObjects.addListener(() -> organizations.setSelectedItem(OrganizationRecord.NULL_RECORD));
    ResetListener setListener = () -> {
      this.shouldHidePredicate.user = user.getText();
      this.shouldHidePredicate.organization = (OrganizationRecord) this.organizations.getSelectedItem();
    };
    this.resetObjects.addListener(setListener);
    setListener.reset();
  }

  /**
   * Add time filters for pictures
   *
   * @param panel The panel to add the time filters to
   */
  private void addTimeFilters(JPanel panel) {
    // Time from panel
    final JPanel fromPanel = new JPanel();
    fromPanel.setLayout(new FlowLayout(FlowLayout.LEFT));
    final JCheckBox filterByDateCheckbox = new JCheckBox(tr("Not older than: "));
    fromPanel.add(filterByDateCheckbox);
    final SpinnerNumberModel spinnerModel = new SpinnerNumberModel(1.0, 0, 10000, .1);
    final JSpinner spinner = new JSpinner(spinnerModel);
    spinner.setEnabled(false);
    fromPanel.add(spinner);

    final JComboBox<String> time = new JComboBox<>(TIME_LIST);
    time.setEnabled(false);
    fromPanel.add(time);

    panel.add(fromPanel, GBC.eol().anchor(GridBagConstraints.LINE_START));
    // Time panel
    final JPanel timePanel = new JPanel(new GridBagLayout());
    final IDatePicker<JComponent> startDate = IDatePicker.getNewDatePicker();
    final IDatePicker<JComponent> endDate = IDatePicker.getNewDatePicker();
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
      spinner.setEnabled(filterByDateCheckbox.isSelected());
      time.setEnabled(filterByDateCheckbox.isSelected());
    });

    spinner.addChangeListener(l -> startDate.setInstant(convertDateRangeBox(spinnerModel, time)));
    time.addActionListener(l -> startDate.setInstant(convertDateRangeBox(spinnerModel, time)));
    filterByDateCheckbox.addChangeListener(l -> startDate.setInstant(convertDateRangeBox(spinnerModel, time)));

    endDate.addEventHandler(l -> this.shouldHidePredicate.endDateRefresh = l.getInstant());
    startDate.addEventHandler(l -> this.shouldHidePredicate.startDateRefresh = l.getInstant());
    filterByDateCheckbox
      .addItemListener(l -> this.shouldHidePredicate.timeFilter = l.getStateChange() == ItemEvent.SELECTED);
    spinnerModel.addChangeListener(l -> this.shouldHidePredicate.dateRange = spinnerModel.getNumber());
    time.addItemListener(l -> this.shouldHidePredicate.time = (String) l.getItem());

    this.resetObjects.addListener(() -> {
      filterByDateCheckbox.setSelected(false);
      filterByDateCheckbox.getItemListeners();
    });
    this.resetObjects.addListener(() -> time.setSelectedItem(TIME_LIST[0]));
    this.resetObjects.addListener(() -> spinnerModel.setValue(1));

    this.resetObjects.addListener(() -> {
      if (endDate != null && startDate != null) {
        endDate.reset();
        startDate.reset();
      }
    });

    ResetListener setFields = () -> {
      this.shouldHidePredicate.timeFilter = filterByDateCheckbox.isSelected();
      this.shouldHidePredicate.endDateRefresh = endDate.getInstant();
      this.shouldHidePredicate.startDateRefresh = startDate.getInstant();
      this.shouldHidePredicate.dateRange = spinnerModel.getNumber();
      this.shouldHidePredicate.time = (String) time.getSelectedItem();
    };
    this.resetObjects.addListener(setFields);
    setFields.reset();
  }

  private static Instant convertDateRangeBox(SpinnerNumberModel spinner, JComboBox<String> timeStep) {
    if (timeStep.isEnabled()) {
      ZonedDateTime current = LocalDate.now(ZoneOffset.UTC).atStartOfDay(ZoneOffset.UTC);
      String type = (String) timeStep.getSelectedItem();
      Number start = spinner.getNumber();
      int[] difference = new int[] { 0, 0, 0 }; // Year, Month, Day
      if (TIME_LIST[0].equals(type)) {
        difference[0] = start.intValue();
        difference[1] = (int) ((start.floatValue() - difference[0]) * 12);
        difference[2] = (int) (((start.floatValue() - difference[0]) * 12 - difference[1]) * 30);
      } else if (TIME_LIST[1].equals(type)) {
        difference[1] = start.intValue();
        difference[2] = (int) ((start.floatValue() - difference[1]) * 30);
      } else if (TIME_LIST[2].contentEquals(type)) {
        difference[2] = start.intValue();
      }
      return current.minus(difference[0], ChronoUnit.YEARS).minus(difference[1], ChronoUnit.MONTHS)
        .minus(difference[2], ChronoUnit.DAYS).toInstant();
    }
    return null;
  }

  private static void updateDates(IDatePicker<?> startDate, IDatePicker<?> endDate, IDatePicker<?> modified) {
    Instant start = startDate.getInstant();
    Instant end = endDate.getInstant();
    if (start == null || end == null)
      return;
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
    if (instance == null)
      instance = new MapillaryFilterDialog();
    return instance;
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
    this.shouldHidePredicate.updateLayerVisible();
    this.shouldHidePredicate.smartAdd = Boolean.TRUE.equals(MapillaryProperties.SMART_EDIT.get());
    updateFilteredImages();
  }

  /**
   * Update filtered images
   */
  public void updateFilteredImages() {
    if (MapillaryLayer.hasInstance()) {
      updateFilteredImages(MapillaryLayer.getInstance().getData().getNodes().parallelStream());
    }
  }

  /**
   * Update filtered images
   *
   * @param nodeStream The nodes to update
   * @param <N> The node type
   */
  private <N extends INode> void updateFilteredImages(Stream<N> nodeStream) {
    Predicate<INode> shouldHide = this.getShouldHidePredicate();
    if (System.getSecurityManager() != null) {
      // Ensure that we aren't initializing the cache in a secure context -- this fails, and throws exceptions.
      // See JOSM #20951
      Caches.UserProfileCache.getInstance();
    }
    nodeStream.filter(MapillaryImageUtils.IS_IMAGE).forEach(img -> img.setVisible(!shouldHide.test(img)));
    MapillaryLayer.invalidateInstance();
  }

  /**
   * Get the current predicate for filtering images
   *
   * @return The image filtering predicate
   */
  public Predicate<INode> getShouldHidePredicate() {
    return this.shouldHidePredicate;
  }

  @Override
  public void finishedLoading(MVTTile tile) {
    updateFilteredImages(
      tile.getData().getAllPrimitives().parallelStream().filter(INode.class::isInstance).map(INode.class::cast));
  }

  private static class ImageFilterPredicate implements Predicate<INode> {
    String cameraModel;
    String cameraMake;
    String time;
    Number dateRange;
    private boolean layerVisible;
    boolean importedIsSelected;
    boolean downloadedIsSelected;
    boolean timeFilter;
    boolean onlySignsIsSelected;
    boolean onlyPanoIsSelected;
    Instant endDateRefresh;
    Instant startDateRefresh;
    OrganizationRecord organization;
    boolean smartAdd;
    String user;
    int qualityScore = Integer.MIN_VALUE;

    public ImageFilterPredicate() {
      this.updateLayerVisible();
    }

    @Override
    public boolean test(INode img) {
      if (!layerVisible) {
        return true;
      }
      MainLayerManager layerManager = MainApplication.getLayerManager();
      if (this.smartAdd && img.hasKey(MapillaryKeys.KEY)
        && !layerManager.getLayersOfType(AbstractOsmDataLayer.class).isEmpty()) {
        Collection<IPrimitive> currentSelection = Stream
          .concat(layerManager.getLayersOfType(OsmDataLayer.class).stream().map(OsmDataLayer::getDataSet),
            layerManager.getLayersOfType(PointObjectLayer.class).stream().map(PointObjectLayer::getData))
          .flatMap(ds -> ds.getAllSelected().stream()).collect(Collectors.toSet());
        Collection<String> keys = currentSelection.stream().map(MapillaryUtils::getImagesFromDetections)
          .flatMap(Collection::stream).collect(Collectors.toSet());
        if (!keys.contains(img.get(MapillaryKeys.KEY))) {
          return true;
        }
      }
      if ((this.timeFilter && checkValidTime(img)) || (this.endDateRefresh != null && checkEndDate(img))
        || (this.startDateRefresh != null && checkStartDate(img))
        || (!this.importedIsSelected && img.hasKey(MapillaryImageUtils.IMPORTED_KEY))
        || (this.onlyPanoIsSelected && !MapillaryImageUtils.IS_PANORAMIC.test(img))
        || (this.cameraMake != null && !this.cameraMake.trim().isEmpty()
          && !this.cameraMake.equals(img.get("camera_make")))
        || (this.cameraModel != null && !this.cameraModel.trim().isEmpty()
          && !this.cameraModel.equals(img.get("camera_model")))
        || (this.qualityScore != Integer.MIN_VALUE && (MapillaryImageUtils.getQuality(img) < this.qualityScore
          // The following line is to ensure that any images that *don't* have a quality score are shown when low
          // quality is OK.
          || (this.qualityScore < 3 && MapillaryImageUtils.getQuality(img) == Integer.MIN_VALUE)))) {
        return true;
      }
      if (!"".equals(MapillaryImageUtils.getKey(img))) {
        if (!this.downloadedIsSelected) {
          return true;
        }
        if (this.onlySignsIsSelected && (ImageDetection.getDetections(img.get(MapillaryKeys.KEY), false).isEmpty()
          || !checkSigns(ImageDetection.getDetections(img.get(MapillaryKeys.KEY), false)))) {
          return true;
        }
        UserProfile userProfile = Caches.UserProfileCache.getInstance().get(img.get(MapillaryKeys.USER_KEY));
        if (!"".equals(this.user) && (userProfile == null || !this.user.equals(userProfile.getUsername()))) {
          return true;
        }
        if (!OrganizationRecord.NULL_RECORD.equals(this.organization) && MapillaryImageUtils.getSequenceKey(img) != null
          && !this.organization.getKey().equals(MapillaryImageUtils.getOrganization(img).getKey())) {
          return true;
        }
      }
      return false;
    }

    /**
     * Update the layer visibility
     */
    void updateLayerVisible() {
      this.layerVisible = MapillaryLayer.hasInstance() && MapillaryLayer.getInstance().isVisible();
    }

    private boolean checkValidTime(INode img) {
      final long currentTime = Instant.now().toEpochMilli();
      for (int i = 0; i < 3; i++) {
        if (TIME_LIST[i].equals(time)
          && MapillaryImageUtils.getDate(img).toEpochMilli() < currentTime - dateRange.doubleValue() * TIME_FACTOR[i]) {
          return true;
        }
      }
      return false;
    }

    /**
     * @param img The image to check
     * @return {@code true} if the start date is after the image date
     */
    private boolean checkStartDate(INode img) {
      final Instant start = LocalDateTime.ofInstant(startDateRefresh, ZoneOffset.UTC).toLocalDate()
        .atStartOfDay(ZoneOffset.UTC).toInstant();
      final Instant imgDate = MapillaryImageUtils.getDate(img);
      return start.isAfter(imgDate);
    }

    /**
     * @param img The image to check
     * @return {@code true} if the end date is before the image date
     */
    private boolean checkEndDate(INode img) {
      final ZonedDateTime nextDate = LocalDateTime.ofInstant(endDateRefresh, ZoneOffset.UTC).toLocalDate()
        .atStartOfDay(ZoneOffset.UTC).plus(1, ChronoUnit.DAYS);
      final Instant end = nextDate.toInstant();
      final Instant imgDate = MapillaryImageUtils.getDate(img);
      return end.isBefore(imgDate);
    }

    /**
     * Checks if the image fulfills the sign conditions.
     *
     * @param imageDetections The {@code Collection<ImageDetection<?>>} object that is going to be
     *        checked.
     * @return {@code true} if it fulfills the conditions; {@code false}
     *         otherwise.
     */
    private static boolean checkSigns(Collection<ImageDetection<?>> imageDetections) {
      final String[] signTags = MapillaryFilterChooseSigns.getSignTags();
      for (int i = 0; i < signTags.length; i++) {
        if (checkSign(imageDetections, MapillaryFilterChooseSigns.getInstance().signCheckboxes[i], signTags[i])) {
          return true;
        }
      }
      return false;
    }

    private static boolean checkSign(Collection<ImageDetection<?>> detections, JCheckBox signCheckBox, String signTag) {
      boolean contains = false;
      final Pattern pattern = Pattern.compile(signTag);
      for (ImageDetection<?> detection : detections) {
        if (pattern.matcher(detection.getValue().getKey()).find()) {
          contains = true;
          break;
        }
      }
      return contains == signCheckBox.isSelected() && contains;
    }
  }

  /**
   * Destroys the unique instance of the class.
   */
  public static synchronized void destroyInstance() {
    instance = null;
  }

  private static class UpdateAction extends AbstractAction {

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

  /**
   * Opens a new window where you can specifically filter signs.
   *
   * @author nokutu
   */
  private static class SignChooserAction extends AbstractAction {

    private static final long serialVersionUID = 8706299665735930148L;

    SignChooserAction() {
      putValue(NAME, tr("Choose signs"));
    }

    @Override
    public void actionPerformed(ActionEvent arg0) {
      final JOptionPane pane = new JOptionPane(MapillaryFilterChooseSigns.getInstance(), JOptionPane.PLAIN_MESSAGE,
        JOptionPane.OK_CANCEL_OPTION);
      JDialog dlg = pane.createDialog(MainApplication.getMainFrame(), tr("Choose signs"));
      dlg.setVisible(true);
      Object value = pane.getValue();
      if (value != null && (int) value == JOptionPane.OK_OPTION) {
        MapillaryFilterDialog.getInstance().refresh();
      }
      dlg.dispose();
    }
  }

  @Override
  public void destroy() {
    if (!destroyed) {
      super.destroy();
      this.destroyable.fireEvent(Destroyable::destroy);
      if (MainApplication.getMap() != null)
        MainApplication.getMap().removeToggleDialog(this);
      OrganizationRecord.removeOrganizationListener(this); // TODO uncomment when API for orgs is available
      destroyed = true;
    }
    destroyInstance();
  }

  @Override
  public void organizationAdded(OrganizationRecord organization) {
    boolean add = true;
    for (int i = 0; i < organizations.getItemCount(); i++) {
      if (organizations.getItemAt(i).equals(organization)) {
        add = false;
        break;
      }
    }
    if (add) {
      GuiHelper.runInEDT(() -> organizations.addItem(organization));
    }
    for (Component comp : Arrays.asList(organizationLabel, organizations)) {
      GuiHelper.runInEDT(
        () -> comp.setEnabled(organizations.getItemCount() > 1 || organization != OrganizationRecord.NULL_RECORD));
    }
  }
}
