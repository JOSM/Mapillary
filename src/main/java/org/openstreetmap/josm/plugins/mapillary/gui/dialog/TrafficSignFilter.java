// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.dialog;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ItemEvent;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.JSpinner;
import javax.swing.SpinnerNumberModel;
import javax.swing.SwingUtilities;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import com.drew.lang.Charsets;
import org.apache.commons.io.IOUtils;

import org.openstreetmap.josm.data.osm.Filter;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.widgets.FilterField;
import org.openstreetmap.josm.plugins.datepicker.IDatePicker;
import org.openstreetmap.josm.plugins.mapillary.gui.ImageCheckBoxButton;
import org.openstreetmap.josm.tools.Destroyable;
import org.openstreetmap.josm.tools.GBC;
import org.openstreetmap.josm.tools.I18n;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.Logging;
import org.openstreetmap.josm.tools.ResourceProvider;

/**
 * This filters traffic signs
 *
 * @author Taylor Smock
 */
public class TrafficSignFilter extends JPanel implements Destroyable {
  private static final long serialVersionUID = 1177890183422385423L;
  private boolean destroyed;
  private final List<ImageCheckBoxButton> buttons;
  private boolean showRelevant;
  private final FilterField filterField;
  private final SpinnerNumberModel showMaxNumberModel;
  private int detectionPage;
  private final JCheckBox toggleVisibleCheckbox;

  public TrafficSignFilter() {
    setLayout(new GridBagLayout());

    /* First/last seen */
    createFirstLastSeen(this, "first");
    createFirstLastSeen(this, "last");
    /* End first/last seen */
    JPanel layers = new JPanel();
    layers.setLayout(new FlowLayout(FlowLayout.LEFT));
    layers.add(new JLabel(I18n.tr("Layer")));
    for (String[] layer : Arrays.asList(
      new String[] { "trafficsigns", I18n.marktr("Traffic Signs") },
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
    toggleVisibleCheckbox.addItemListener(l -> toggleVisible(l.getStateChange() == ItemEvent.SELECTED));
    add(showRelevantObjs, GBC.std().anchor(GridBagConstraints.WEST));
    add(toggleVisibleCheckbox, GBC.std().anchor(GridBagConstraints.CENTER));
    add(pagination, GBC.eol().anchor(GridBagConstraints.EAST));

    buttons = new ArrayList<>();
    addButtons();
    /* End filter signs */
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

  private void updateShown(SpinnerNumberModel model) {
    buttons.parallelStream().forEach(i -> SwingUtilities.invokeLater(() -> i.setVisible(false)));
    buttons.stream().filter(i -> i.isFiltered(filterField.getText())).skip(
      detectionPage * model.getNumber().longValue()).limit(model.getNumber().longValue())
      .forEach(i -> SwingUtilities.invokeLater(() -> i.setVisible(true)));
    long notSelected = buttons.parallelStream().filter(Component::isVisible).filter(i -> !i.isSelected()).count();
    long selected = buttons.parallelStream().filter(Component::isVisible).filter(ImageCheckBoxButton::isSelected)
      .count();
    toggleVisibleCheckbox.setSelected(notSelected < selected);
    toggleVisibleCheckbox.invalidate();
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

  private static void updateDates(
    String position, IDatePicker<?> modified, IDatePicker<?> firstSeen, IDatePicker<?> lastSeen) {
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
      .filter(f -> f.text.isEmpty())
      .collect(Collectors.toList());
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

  private void showRelevantObjects(boolean showRelevant) {
    this.showRelevant = showRelevant;
    filterButtons(filterField.getText());
    updateShown(showMaxNumberModel);
  }

  private void toggleVisible(boolean check) {
    if (SwingUtilities.isEventDispatchThread()) {
      MainApplication.worker.execute(() -> toggleVisible(check));
      return;
    }
    MapillaryExpertFilterDialog.getInstance().getFilterModel().pauseUpdates();
    List<Future<?>> futures = buttons.stream().filter(ImageCheckBoxButton::isVisible)
      .map(b -> b.setSelected(check)).filter(Objects::nonNull).collect(Collectors.toList());

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
    getIcons(scrollPane, "signs");
    getIcons(scrollPane, "objects");
    add(scrollPane, GBC.eol().fill().anchor(GridBagConstraints.WEST));
  }

  private void filterButtons(String expr) {
    SwingUtilities.invokeLater(
      () -> buttons.stream().forEach(b -> b.setVisible(b.isFiltered(expr) && (!showRelevant || b.isRelevant()))));
  }

  public void getIcons(JComponent panel, String type) {
    if (SwingUtilities.isEventDispatchThread()) {
      MainApplication.worker.execute(() -> getIcons(panel, type));
      return;
    }
    String directory = "mapillary_sprite_source/package_" + type;
    try {
      List<String> files = IOUtils.readLines(ResourceProvider.getResourceAsStream("/images/" + directory),
        Charsets.UTF_8.name());
      Collections.sort(files);
      for (String file : files) {
        try {
          SwingUtilities.invokeAndWait(() -> {
            ImageCheckBoxButton button = new ImageCheckBoxButton(directory, file);
            buttons.add(button);
            panel.add(button, GBC.eol().fill(GridBagConstraints.HORIZONTAL).anchor(GridBagConstraints.WEST));
          });
        } catch (InvocationTargetException e) {
          Logging.error(e);
        } catch (InterruptedException e) {
          Logging.error(e);
          Thread.currentThread().interrupt();
        }
      }
      SwingUtilities.invokeLater(() -> panel.add(new JSeparator(), GBC.eol()));
      Stream.of(showMaxNumberModel.getListeners(ChangeListener.class))
        .forEach(i -> SwingUtilities.invokeLater(() -> i.stateChanged(new ChangeEvent(this))));
    } catch (IOException e) {
      Logging.error(e);
    }
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
    List<Future<?>> futures = buttons.stream().map(b -> b.setSelected(false)).collect(Collectors.toList());
    filterField.setText("");
    showMaxNumberModel.setValue(100);
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
}
