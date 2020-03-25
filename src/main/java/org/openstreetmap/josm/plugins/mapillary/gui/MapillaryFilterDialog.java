// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.time.LocalDate;
import java.util.Arrays;
import java.util.Calendar;
import java.util.function.Predicate;
import java.util.regex.Pattern;

import javax.swing.AbstractAction;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.JTextField;
import javax.swing.SpinnerNumberModel;

import javafx.event.EventType;
import javafx.scene.control.DatePicker;

import org.openstreetmap.josm.actions.ExpertToggleAction;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.SideButton;
import org.openstreetmap.josm.gui.dialogs.ToggleDialog;
import org.openstreetmap.josm.gui.widgets.DisableShortcutsOnFocusGainedTextField;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryDataListener;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImportedImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.gui.dialog.JavaFxWrapper;
import org.openstreetmap.josm.plugins.mapillary.model.ImageDetection;
import org.openstreetmap.josm.plugins.mapillary.model.UserProfile;
import org.openstreetmap.josm.plugins.mapillary.utils.LocalDateConverter;
import org.openstreetmap.josm.tools.GBC;
import org.openstreetmap.josm.tools.ImageProvider;

/**
 * ToggleDialog that lets you filter the images that are being shown.
 *
 * @author nokutu
 * @see MapillaryFilterChooseSigns
 */
public final class MapillaryFilterDialog extends ToggleDialog implements MapillaryDataListener {

  private static final long serialVersionUID = -4192029663670922103L;

  private static MapillaryFilterDialog instance;

  private static final String[] TIME_LIST = {tr("Years"), tr("Months"), tr("Days")};

  private static final long[] TIME_FACTOR = new long[]{
    31_536_000_000L, // = 365 * 24 * 60 * 60 * 1000 = number of ms in a year
    2_592_000_000L, // = 30 * 24 * 60 * 60 * 1000 = number of ms in a month
    86_400_000 // = 24 * 60 * 60 * 1000 = number of ms in a day
  };

  private final JCheckBox filterByDateCheckbox;
  /**
   * Spinner to choose the range of dates.
   */
  private final SpinnerNumberModel spinnerModel;

  private final JCheckBox imported = new JCheckBox(tr("Imported images"));
  private final JCheckBox downloaded = new JCheckBox(new DownloadCheckBoxAction());
  private final JCheckBox onlySigns = new JCheckBox(new OnlySignsAction());
  private final JComboBox<String> time;
  private final JTextField user;

  private final JButton signChooser = new JButton(new SignChooserAction());

  private final JavaFxWrapper<DatePicker> startDate;
  private final JavaFxWrapper<DatePicker> endDate;

  private MapillaryFilterDialog() {
    super(tr("Mapillary filter"), "mapillary-filter", tr("Open Mapillary filter dialog"), null, 200,
        false, MapillaryPreferenceSetting.class);

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

    // TODO Replace if #18747 is taken
    startDate = new JavaFxWrapper<>(DatePicker.class);
    endDate = new JavaFxWrapper<>(DatePicker.class);
    JPanel timePanel = new JPanel(new GridBagLayout());
    timePanel.add(new JLabel(tr("Start")), GBC.std());
    timePanel.add(new JLabel(tr("End")), GBC.eol());
    timePanel.add(startDate, GBC.std());
    timePanel.add(endDate, GBC.eol());
    Dimension d = timePanel.getMinimumSize();
    d.width = Double.valueOf(Math.ceil(d.width * 1.15)).intValue();
    d.height = Double.valueOf(Math.ceil(d.height * 1.15)).intValue(); // TODO check
    timePanel.setMinimumSize(d);

    startDate.getNode().addEventHandler(EventType.ROOT, e -> updateDates(startDate));
    endDate.getNode().addEventHandler(EventType.ROOT, e -> updateDates(endDate));
    endDate.getNode().setConverter(new LocalDateConverter());
    startDate.getNode().setConverter(endDate.getNode().getConverter());
    ExpertToggleAction.addVisibilitySwitcher(timePanel);

    filterByDateCheckbox.addItemListener(itemE -> {
      spinner.setEnabled(filterByDateCheckbox.isSelected());
      time.setEnabled(filterByDateCheckbox.isSelected());
    });

    JPanel userSearchPanel = new JPanel();
    userSearchPanel.setLayout(new FlowLayout(FlowLayout.LEFT));
    this.user = new DisableShortcutsOnFocusGainedTextField(10);
    this.user.addActionListener(new UpdateAction());
    userSearchPanel.add(new JLabel(tr("User")));
    userSearchPanel.add(this.user);

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

    createLayout(panel, true, Arrays.asList(new SideButton(new UpdateAction()), new SideButton(new ResetAction())));
  }

  private void updateDates(JavaFxWrapper<?> modified) {
    LocalDate start = startDate.getNode().getValue();
    LocalDate end = endDate.getNode().getValue();
    if (start == null || end == null)
      return;
    if (modified == startDate) {
      if (start.compareTo(end) > 0) {
        endDate.getNode().setValue(start);
      }
    } else if (modified == endDate) {
      if (start.compareTo(end) > 0) {
        startDate.getNode().setValue(end);
      }
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
    this.endDate.getNode().setValue(null);
    this.imported.setSelected(true);
    this.filterByDateCheckbox.setSelected(false);
    this.downloaded.setSelected(true);
    this.onlySigns.setEnabled(true);
    this.onlySigns.setSelected(false);
    this.user.setText("");
    this.time.setSelectedItem(TIME_LIST[0]);
    this.signChooser.setEnabled(false);
    this.spinnerModel.setValue(1);
    this.startDate.getNode().setValue(null);
    refresh();
  }

  /**
   * Applies the selected filter.
   */
  public synchronized void refresh() {
    final boolean layerVisible = MapillaryLayer.hasInstance() && MapillaryLayer.getInstance().isVisible();
    final boolean imported = this.imported.isSelected();
    final boolean downloaded = this.downloaded.isSelected();
    final boolean timeFilter = filterByDateCheckbox.isSelected();
    final boolean onlySigns = this.onlySigns.isSelected();
    final LocalDate endDate = this.endDate.getNode().getValue();
    final LocalDate startDate = this.startDate.getNode().getValue();

    // This predicate returns true if the image should be made invisible
    Predicate<MapillaryAbstractImage> shouldHide =
      img -> {
        if (!layerVisible) {
          return true;
        }
        if (timeFilter && checkValidTime(img)) {
          return true;
        }
        if (endDate != null && checkEndDate(img)) {
          return true;
        }
        if (startDate != null && checkStartDate(img)) {
          return true;
        }
        if (!imported && img instanceof MapillaryImportedImage) {
          return true;
        }
        if (img instanceof MapillaryImage) {
          if (!downloaded) {
            return true;
          }
          if (onlySigns && (((MapillaryImage) img).getDetections().isEmpty() || !checkSigns((MapillaryImage) img))) {
            return true;
          }
          UserProfile userProfile = ((MapillaryImage) img).getUser();
          if (!"".equals(user.getText()) && (userProfile == null || !user.getText().equals(userProfile.getUsername()))) {
            return true;
          }
        }
        return false;
      };

    if (MapillaryLayer.hasInstance()) {
      MapillaryLayer.getInstance().getData().getImages().parallelStream().forEach(img -> img.setVisible(!shouldHide.test(img)));
    }

    MapillaryLayer.invalidateInstance();
  }

  private boolean checkValidTime(MapillaryAbstractImage img) {
    final long currentTime = currentTime();
    for (int i = 0; i < 3; i++) {
      if (TIME_LIST[i].equals(time.getSelectedItem()) &&
        img.getCapturedAt() < currentTime - spinnerModel.getNumber().doubleValue() * TIME_FACTOR[i]) {
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
    LocalDate start = this.startDate.getNode().getValue();
    LocalDate imgDate = LocalDate.parse(img.getDate("yyyy-MM-dd"));
    return start.isAfter(imgDate);
  }

  /**
   * @param img The image to check
   * @return {@code true} if the end date is before the image date
   */
  private boolean checkEndDate(MapillaryAbstractImage img) {
    LocalDate end = this.endDate.getNode().getValue();
    LocalDate imgDate = LocalDate.parse(img.getDate("yyyy-MM-dd"));
    return end.isBefore(imgDate);
  }

  /**
   * Checks if the image fulfills the sign conditions.
   *
   * @param img The {@link MapillaryAbstractImage} object that is going to be
   * checked.
   *
   * @return {@code true} if it fulfills the conditions; {@code false}
   * otherwise.
   */
  private static boolean checkSigns(MapillaryImage img) {
    for (int i = 0; i < MapillaryFilterChooseSigns.SIGN_TAGS.length; i++) {
      if (checkSign(img, MapillaryFilterChooseSigns.getInstance().signCheckboxes[i], MapillaryFilterChooseSigns.SIGN_TAGS[i]))
        return true;
    }
    return false;
  }

  private static boolean checkSign(MapillaryImage img, JCheckBox signCheckBox, String signTag) {
    boolean contains = false;
    for (ImageDetection detection : img.getDetections()) {
      if (Pattern.compile(signTag).matcher(detection.getValue()).find()) {
        contains = true;
      }
    }
    return contains == signCheckBox.isSelected() && contains;
  }

  private static long currentTime() {
    Calendar cal = Calendar.getInstance();
    return cal.getTimeInMillis();
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
      final JOptionPane pane = new JOptionPane(
        MapillaryFilterChooseSigns.getInstance(),
        JOptionPane.PLAIN_MESSAGE,
        JOptionPane.OK_CANCEL_OPTION
      );
      JDialog dlg = pane.createDialog(MainApplication.getMainFrame(), tr("Choose signs"));
      dlg.setVisible(true);
      if ((int) pane.getValue() == JOptionPane.OK_OPTION)
        MapillaryFilterDialog.getInstance().refresh();
      dlg.dispose();
    }
  }
}
