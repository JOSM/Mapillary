// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.dialog;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Predicate;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.swing.AbstractAction;
import javax.swing.DefaultListCellRenderer;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.JSpinner;
import javax.swing.JTextField;
import javax.swing.SpinnerNumberModel;

import org.openstreetmap.josm.data.osm.OsmPrimitive;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.SideButton;
import org.openstreetmap.josm.gui.dialogs.ToggleDialog;
import org.openstreetmap.josm.gui.layer.AbstractOsmDataLayer;
import org.openstreetmap.josm.gui.layer.MainLayerManager;
import org.openstreetmap.josm.gui.layer.OsmDataLayer;
import org.openstreetmap.josm.gui.util.GuiHelper;
import org.openstreetmap.josm.gui.widgets.DisableShortcutsOnFocusGainedTextField;
import org.openstreetmap.josm.plugins.datepicker.IDatePicker;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryDataListener;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImportedImage;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.OrganizationRecord;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.OrganizationRecord.OrganizationRecordListener;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryFilterChooseSigns;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryPreferenceSetting;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.PointObjectLayer;
import org.openstreetmap.josm.plugins.mapillary.model.ImageDetection;
import org.openstreetmap.josm.plugins.mapillary.model.UserProfile;
import org.openstreetmap.josm.plugins.mapillary.oauth.MapillaryLoginListener;
import org.openstreetmap.josm.plugins.mapillary.oauth.MapillaryUser;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;
import org.openstreetmap.josm.tools.GBC;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.Shortcut;

/**
 * ToggleDialog that lets you filter the images that are being shown.
 *
 * @author nokutu
 * @see MapillaryFilterChooseSigns
 */
public final class MapillaryFilterDialog extends ToggleDialog
  implements MapillaryDataListener, MapillaryLoginListener, OrganizationRecordListener {

  private static final long serialVersionUID = -4192029663670922103L;

  private static MapillaryFilterDialog instance;

  private static final String[] TIME_LIST = { tr("Years"), tr("Months"), tr("Days") };

  private static final long[] TIME_FACTOR = new long[] { 31_536_000_000L, // = 365 * 24 * 60 * 60 * 1000 = number of ms
                                                                          // in a year
    2_592_000_000L, // = 30 * 24 * 60 * 60 * 1000 = number of ms in a month
    86_400_000 // = 24 * 60 * 60 * 1000 = number of ms in a day
  };

  private final JCheckBox filterByDateCheckbox;
  /**
   * Spinner to choose the range of dates.
   */
  private final SpinnerNumberModel spinnerModel;

  private final JCheckBox imported = new JCheckBox(tr("Imported images"));
  private final JCheckBox onlyPano = new JCheckBox(tr("Panorama only"));
  private final JComboBox<String> time;
  private final JTextField user;

  final JButton signChooser = new JButton(new SignChooserAction());
  final JCheckBox downloaded = new JCheckBox(new DownloadCheckBoxAction());
  final JCheckBox onlySigns = new JCheckBox(new OnlySignsAction());

  final JComboBox<OrganizationRecord> organizations = new JComboBox<>();

  private final IDatePicker<?> startDate;
  private final IDatePicker<?> endDate;

  private boolean destroyed;

  private final JLabel organizationLabel;
  private final TrafficSignFilter objectFilter;

  private Predicate<MapillaryAbstractImage> shouldHidePredicate;

  private MapillaryFilterDialog() {
    super(tr("Mapillary filter"), "mapillary-filter", tr("Open Mapillary filter dialog"),
      Shortcut.registerShortcut("mapillary:filterdialog", tr("Mapillary images Filter"), KeyEvent.CHAR_UNDEFINED,
        Shortcut.NONE),
      200, false, MapillaryPreferenceSetting.class);
    MapillaryUser.addListener(this);

    this.signChooser.setEnabled(false);
    JPanel signChooserPanel = new JPanel();
    signChooserPanel.setLayout(new FlowLayout(FlowLayout.LEFT));
    signChooserPanel.add(this.signChooser);

    JPanel fromPanel = new JPanel();
    fromPanel.setLayout(new FlowLayout(FlowLayout.LEFT));
    filterByDateCheckbox = new JCheckBox(tr("Not older than: "));
    fromPanel.add(filterByDateCheckbox);
    this.spinnerModel = new SpinnerNumberModel(1.0, 0, 10000, .1);
    JSpinner spinner = new JSpinner(spinnerModel);
    spinner.setEnabled(false);
    fromPanel.add(spinner);
    time = new JComboBox<>(TIME_LIST);
    time.setEnabled(false);
    fromPanel.add(this.time);

    JPanel timePanel;
    startDate = IDatePicker.getNewDatePicker();
    endDate = IDatePicker.getNewDatePicker();
    Consumer<IDatePicker<?>> function = this::updateDates;
    startDate.addEventHandler(function);
    endDate.addEventHandler(function);
    timePanel = new JPanel(new GridBagLayout());
    timePanel.add(new JLabel(tr("Start")), GBC.std().anchor(GridBagConstraints.LINE_START));
    timePanel.add(new JLabel(tr("End")), GBC.eol().anchor(GridBagConstraints.LINE_END));
    timePanel.add(startDate.getComponent(), GBC.std().anchor(GridBagConstraints.LINE_START));
    timePanel.add(endDate.getComponent(), GBC.eol().anchor(GridBagConstraints.LINE_END));
    Dimension d = timePanel.getMinimumSize();
    d.width = (int) Math.ceil(d.width * 1.15);
    d.height = (int) Math.ceil(d.height * 1.15);
    timePanel.setMinimumSize(d);

    filterByDateCheckbox.addItemListener(itemE -> {
      spinner.setEnabled(filterByDateCheckbox.isSelected());
      time.setEnabled(filterByDateCheckbox.isSelected());
    });

    spinner.addChangeListener(l -> startDate.setDate(convertDateRangeBox(spinnerModel, time)));
    time.addActionListener(l -> startDate.setDate(convertDateRangeBox(spinnerModel, time)));
    filterByDateCheckbox.addChangeListener(l -> startDate.setDate(convertDateRangeBox(spinnerModel, time)));

    JPanel userSearchPanel = new JPanel();
    userSearchPanel.setLayout(new FlowLayout(FlowLayout.LEFT));
    this.user = new DisableShortcutsOnFocusGainedTextField(10);
    this.user.addActionListener(new UpdateAction());
    userSearchPanel.add(new JLabel(tr("User")));
    userSearchPanel.add(this.user, GBC.eol());
    organizationLabel = new JLabel(tr("Org"));
    organizationLabel.setToolTipText(tr("Organizations"));
    userSearchPanel.add(organizationLabel);
    userSearchPanel.add(this.organizations);
    organizations.addItem(OrganizationRecord.NULL_RECORD);
    for (Component comp : Arrays.asList(organizationLabel, organizations)) {
      comp.setEnabled(false);
      comp.setVisible(false);
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
            comp.setIcon(organization.getAvatar());
          }
        }
        return comp;
      }

    });
    if (MapillaryUser.getUsername() != null) {
      onLogin(null);
    }

    // OrganizationRecord.addOrganizationListener(this); // TODO uncomment when API for orgs is available
    OrganizationRecord.getOrganizations().forEach(this::organizationAdded);

    this.imported.setSelected(true);
    this.downloaded.setSelected(true);

    JPanel panel = new JPanel(new GridBagLayout());
    JPanel imageLine = new JPanel();
    imageLine.add(this.downloaded, GBC.std().anchor(GridBagConstraints.LINE_START));
    imageLine.add(this.imported, GBC.eol());
    panel.add(imageLine, GBC.eol().anchor(GridBagConstraints.LINE_START));
    panel.add(fromPanel, GBC.eol().anchor(GridBagConstraints.LINE_START));
    panel.add(timePanel, GBC.eol().anchor(GridBagConstraints.LINE_START));
    panel.add(userSearchPanel, GBC.eol().anchor(GridBagConstraints.LINE_START));
    JPanel signs = new JPanel();
    signs.add(this.onlySigns, GBC.std().anchor(GridBagConstraints.LINE_START));
    signs.add(signChooserPanel, GBC.eol().anchor(GridBagConstraints.LINE_START));
    panel.add(signs, GBC.eol().anchor(GridBagConstraints.LINE_START));
    panel.add(onlyPano, GBC.eol().anchor(GridBagConstraints.LINE_START));

    panel.add(new JSeparator(), GBC.eol().fill(GridBagConstraints.HORIZONTAL));
    objectFilter = new TrafficSignFilter();
    panel.add(objectFilter, GBC.eol().fill().anchor(GridBagConstraints.WEST));
    createLayout(panel, true, Arrays.asList(new SideButton(new UpdateAction()), new SideButton(new ResetAction())));
  }

  private static LocalDate convertDateRangeBox(SpinnerNumberModel spinner, JComboBox<String> timeStep) {
    if (timeStep.isEnabled()) {
      LocalDate current = LocalDate.now(ZoneId.systemDefault());
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
      LocalDate year = current.minusYears(difference[0]);
      LocalDate month = year.minusMonths(difference[1]);
      return month.minusDays(difference[2]);
    }
    return null;
  }

  private void updateDates(IDatePicker<?> modified) {
    LocalDate start = startDate.getDate();
    LocalDate end = endDate.getDate();
    if (start == null || end == null)
      return;
    if (startDate.equals(modified) && start.compareTo(end) > 0) {
      endDate.setDate(start);
    } else if (endDate.equals(modified) && start.compareTo(end) > 0) {
      startDate.setDate(end);
    }
  }

  /**
   * @return the unique instance of the class.
   */
  public static synchronized MapillaryFilterDialog getInstance() {
    if (instance == null)
      instance = new MapillaryFilterDialog();
    return instance;
  }

  @Override
  public void imagesAdded() {
    refresh();
  }

  @Override
  public void selectedImageChanged(MapillaryAbstractImage oldImage, MapillaryAbstractImage newImage) {
    // Do nothing when image selection changed
  }

  /**
   * Resets the dialog to its default state.
   */
  public void reset() {
    this.filterByDateCheckbox.setSelected(false);
    this.imported.setSelected(true);
    this.downloaded.setSelected(true);
    this.onlySigns.setEnabled(true);
    this.onlySigns.setSelected(false);
    this.onlyPano.setSelected(false);
    this.user.setText("");
    this.time.setSelectedItem(TIME_LIST[0]);
    this.signChooser.setEnabled(false);
    this.spinnerModel.setValue(1);
    this.objectFilter.reset();
    if (this.endDate != null && this.startDate != null) {
      this.endDate.reset();
      this.startDate.reset();
    }
    MapillaryFilterChooseSigns.reset();
    organizations.setSelectedItem(OrganizationRecord.NULL_RECORD);
    refresh();
  }

  /**
   * Applies the selected filter.
   */
  public synchronized void refresh() {
    // This predicate returns true if the image should be made invisible
    this.shouldHidePredicate = new ImageFilterPredicate(this.imported, this.downloaded, this.filterByDateCheckbox,
      this.onlySigns, this.onlyPano, this.endDate, this.startDate, this.spinnerModel, this.time, this.organizations,
      this.user);

    updateFilteredImages();
  }

  /**
   * Update filtered images
   */
  public void updateFilteredImages() {
    if (MapillaryLayer.hasInstance()) {
      Predicate<MapillaryAbstractImage> shouldHide = this.getShouldHidePredicate();
      if (shouldHide != null) {
        MapillaryLayer.getInstance().getData().getImages().parallelStream()
          .forEach(img -> img.setVisible(!shouldHide.test(img)));
        MapillaryLayer.invalidateInstance();
      }
    }
  }

  /**
   * Get the current predicate for filtering images
   *
   * @return The image filtering predicate
   */
  public Predicate<MapillaryAbstractImage> getShouldHidePredicate() {
    return this.shouldHidePredicate;
  }

  private static class ImageFilterPredicate implements Predicate<MapillaryAbstractImage> {
    private final boolean layerVisible = MapillaryLayer.hasInstance() && MapillaryLayer.getInstance().isVisible();
    private final boolean importedIsSelected;
    private final boolean downloadedIsSelected;
    private final boolean timeFilter;
    private final boolean onlySignsIsSelected;
    private final boolean onlyPanoIsSelected;
    private final LocalDate endDateRefresh;
    private final LocalDate startDateRefresh;
    private final OrganizationRecord organization;
    private final boolean smartAdd;
    private final String user;
    private JComboBox<String> time;
    private SpinnerNumberModel spinnerModel;
    private IDatePicker<?> startDate;
    private IDatePicker<?> endDate;

    public ImageFilterPredicate(JCheckBox imported, JCheckBox downloaded, JCheckBox filterByDateCheckbox,
      JCheckBox onlySigns, JCheckBox onlyPano, IDatePicker<?> endDate, IDatePicker<?> startDate,
      SpinnerNumberModel spinnerModel, JComboBox<String> time, JComboBox<OrganizationRecord> organizations,
      JTextField user) {
      this.importedIsSelected = imported.isSelected();
      this.downloadedIsSelected = downloaded.isSelected();
      this.timeFilter = filterByDateCheckbox.isSelected();
      this.onlySignsIsSelected = onlySigns.isSelected();
      this.onlyPanoIsSelected = onlyPano.isSelected();
      this.endDateRefresh = endDate == null ? null : endDate.getDate();
      this.startDateRefresh = startDate == null ? convertDateRangeBox(spinnerModel, time) : startDate.getDate();
      this.organization = (OrganizationRecord) organizations.getSelectedItem();
      this.smartAdd = Boolean.TRUE.equals(MapillaryProperties.SMART_EDIT.get());
      this.user = user.getText();
      this.time = time;
      this.spinnerModel = spinnerModel;
      this.startDate = startDate;
      this.endDate = endDate;
    }

    @Override
    public boolean test(MapillaryAbstractImage img) {
      if (!layerVisible) {
        return true;
      }
      MainLayerManager layerManager = MainApplication.getLayerManager();
      if (smartAdd && img instanceof MapillaryImage
        && !layerManager.getLayersOfType(AbstractOsmDataLayer.class).isEmpty()) {
        Collection<OsmPrimitive> currentSelection = Stream
          .concat(layerManager.getLayersOfType(OsmDataLayer.class).stream().map(OsmDataLayer::getDataSet),
            layerManager.getLayersOfType(PointObjectLayer.class).stream().map(PointObjectLayer::getDataSet))
          .flatMap(ds -> ds.getAllSelected().stream()).collect(Collectors.toSet());
        Collection<String> keys = currentSelection.stream().map(MapillaryUtils::getImagesFromDetections)
          .flatMap(Collection::stream).collect(Collectors.toSet());
        if (!keys.contains(((MapillaryImage) img).getKey())) {
          return true;
        }
      }
      if (timeFilter && checkValidTime(img)) {
        return true;
      }
      if (endDateRefresh != null && checkEndDate(img)) {
        return true;
      }
      if (startDateRefresh != null && checkStartDate(img)) {
        return true;
      }
      if (!importedIsSelected && img instanceof MapillaryImportedImage) {
        return true;
      }
      if (onlyPanoIsSelected && !img.isPanorama()) {
        return true;
      }
      if (img instanceof MapillaryImage) {
        if (!downloadedIsSelected) {
          return true;
        }
        if (onlySignsIsSelected
          && (((MapillaryImage) img).getDetections().isEmpty() || !checkSigns((MapillaryImage) img))) {
          return true;
        }
        UserProfile userProfile = ((MapillaryImage) img).getUser();
        if (!"".equals(this.user) && (userProfile == null || !this.user.equals(userProfile.getUsername()))) {
          return true;
        }
        if (!OrganizationRecord.NULL_RECORD.equals(organization) && img.getSequence() != null
          && !((MapillaryImage) img).getSequence().getOrganization().getKey().equals(organization.getKey())) {
          return true;
        }
      }
      return false;
    }

    private boolean checkValidTime(MapillaryAbstractImage img) {
      final long currentTime = currentTime();
      for (int i = 0; i < 3; i++) {
        if (TIME_LIST[i].equals(time.getSelectedItem())
          && img.getCapturedAt() < currentTime - spinnerModel.getNumber().doubleValue() * TIME_FACTOR[i]) {
          return true;
        }
      }
      return false;
    }

    /**
     * @param img The image to check
     * @return {@code true} if the start date is after the image date
     */
    private boolean checkStartDate(MapillaryAbstractImage img) {
      LocalDate start = startDate.getDate();
      LocalDate imgDate = LocalDate.parse(img.getDate("yyyy-MM-dd"));
      return start.isAfter(imgDate);
    }

    /**
     * @param img The image to check
     * @return {@code true} if the end date is before the image date
     */
    private boolean checkEndDate(MapillaryAbstractImage img) {
      LocalDate end = endDate.getDate();
      LocalDate imgDate = LocalDate.parse(img.getDate("yyyy-MM-dd"));
      return end.isBefore(imgDate);
    }

    /**
     * Checks if the image fulfills the sign conditions.
     *
     * @param img The {@link MapillaryAbstractImage} object that is going to be
     *        checked.
     * @return {@code true} if it fulfills the conditions; {@code false}
     *         otherwise.
     */
    private static boolean checkSigns(MapillaryImage img) {
      for (int i = 0; i < MapillaryFilterChooseSigns.SIGN_TAGS.length; i++) {
        if (checkSign(img, MapillaryFilterChooseSigns.getInstance().signCheckboxes[i],
          MapillaryFilterChooseSigns.SIGN_TAGS[i]))
          return true;
      }
      return false;
    }

    private static boolean checkSign(MapillaryImage img, JCheckBox signCheckBox, String signTag) {
      boolean contains = false;
      for (ImageDetection<?> detection : img.getDetections()) {
        if (Pattern.compile(signTag).matcher(detection.getValue().getKey()).find()) {
          contains = true;
        }
      }
      return contains == signCheckBox.isSelected() && contains;
    }

    private static long currentTime() {
      Calendar cal = Calendar.getInstance();
      return cal.getTimeInMillis();
    }
  }

  /**
   * Destroys the unique instance of the class.
   */
  public static synchronized void destroyInstance() {
    instance = null;
  }

  private class DownloadCheckBoxAction extends AbstractAction {

    private static final long serialVersionUID = 4672634002899519496L;

    DownloadCheckBoxAction() {
      putValue(NAME, tr("Downloaded images"));
    }

    @Override
    public void actionPerformed(ActionEvent arg0) {
      onlySigns.setEnabled(downloaded.isSelected());
    }
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

  private class OnlySignsAction extends AbstractAction {

    private static final long serialVersionUID = -2937440338019185723L;

    OnlySignsAction() {
      putValue(NAME, tr("Only images with signs"));
    }

    @Override
    public void actionPerformed(ActionEvent arg0) {
      signChooser.setEnabled(onlySigns.isSelected());
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
      objectFilter.destroy();
      if (MainApplication.getMap() != null)
        MainApplication.getMap().removeToggleDialog(this);
      // OrganizationRecord.removeOrganizationListener(this); // TODO uncomment when API for orgs is available
      MapillaryUser.removeListener(this);
      destroyed = true;
    }
    destroyInstance();
  }

  @Override
  public void onLogin(String username) {
    List<OrganizationRecord> userOrganizations = MapillaryUser.getOrganizations();
    userOrganizations.forEach(this::organizationAdded);
  }

  @Override
  public void onLogout() {
    // TODO Only remove user organizations, not all organizations
    organizations.removeAllItems();
    organizationAdded(OrganizationRecord.NULL_RECORD);
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
      GuiHelper.runInEDTAndWait(() -> organizations.addItem(organization));
    }
    for (Component comp : Arrays.asList(organizationLabel, organizations)) {
      GuiHelper.runInEDT(() -> comp.setEnabled(organizations.getItemCount() > 1));
      GuiHelper.runInEDT(() -> comp.setVisible(organizations.getItemCount() > 1));
    }
  }
}
