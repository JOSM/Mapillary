// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.dialog;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ItemEvent;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
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
import javafx.event.ActionEvent;
import javafx.scene.control.DatePicker;
import org.apache.commons.io.IOUtils;

import org.openstreetmap.josm.data.osm.Filter;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.widgets.FilterField;
import org.openstreetmap.josm.plugins.javafx.gui.JavaFxWrapper;
import org.openstreetmap.josm.plugins.mapillary.gui.ImageCheckBoxButton;
import org.openstreetmap.josm.plugins.mapillary.utils.LocalDateConverter;
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
  public static final MapillaryFilterTableModel FILTER_TABLE_MODEL = ImageCheckBoxButton.FILTER_TABLE_MODEL;
  private boolean destroyed;
  private final List<ImageCheckBoxButton> buttons;
  private boolean showRelevant;
  private final FilterField filterField;
  private SpinnerNumberModel showMaxModel;
  private int detectionPage;
  private JCheckBox toggleVisible;

  public TrafficSignFilter() {
    setLayout(new GridBagLayout());

    /* First/last seen */
    createFirstLastSeen(this, "first");
    createFirstLastSeen(this, "last");
    /* End first/last seen */

    /* Filter minimum detections */
    add(new JLabel(I18n.tr("Minimum object detections")));
    JSpinner minDetections = new JSpinner();
    minDetections.setModel(new SpinnerNumberModel(0, 0, 100, 1));
    add(minDetections, GBC.eol());
    Filter minDetectionFilter = FILTER_TABLE_MODEL.getFilters().parallelStream()
      .filter(p -> p.text.contains("min_detections")).findFirst().orElseGet(() -> {
        Filter filter = new Filter();
        FILTER_TABLE_MODEL.addFilter(filter);
        return filter;
      });
    minDetections.addChangeListener(l -> updateMinDetectionFilter(minDetections, minDetectionFilter));
    /* End filter minimum detections */

    /* Filter signs */
    filterField = new FilterField().filter(this::filterButtons);
    filterField.setToolTipText(I18n.tr("Filter Mapillary Detections"));
    add(filterField, GBC.eol().fill(GridBagConstraints.HORIZONTAL));
    toggleVisible = new JCheckBox(I18n.tr("Select Visible"));
    JCheckBox showRelevantObjs = new JCheckBox(I18n.tr("Show Relevant"));
    JPanel pagination = new JPanel(new GridBagLayout());
    JButton previousButtonPagination = new JButton(ImageProvider.get("svpLeft"));
    JButton nextButtonPagination = new JButton(ImageProvider.get("svpRight"));
    nextButtonPagination.addActionListener(l -> updateDetectionPage(1));
    previousButtonPagination.addActionListener(l -> updateDetectionPage(-1));
    pagination.add(previousButtonPagination, GBC.std().anchor(GridBagConstraints.EAST));
    JSpinner showMax = new JSpinner();
    showMax.setMaximumSize(new Dimension(60, showMax.getMaximumSize().height));
    showMaxModel = new SpinnerNumberModel(100, 10, Integer.MAX_VALUE, 10);
    showMax.setModel(showMaxModel);
    showMaxModel.addChangeListener(l -> updateShown(showMaxModel));
    showMax.addChangeListener(l -> updateShown(showMaxModel));
    pagination.add(showMax, GBC.std().anchor(GridBagConstraints.CENTER));
    pagination.add(nextButtonPagination, GBC.std().anchor(GridBagConstraints.WEST));
    showRelevantObjs.addItemListener(l -> showRelevantObjects(l.getStateChange() == ItemEvent.SELECTED));
    toggleVisible.addItemListener(l -> toggleVisible(l.getStateChange() == ItemEvent.SELECTED));
    add(showRelevantObjs, GBC.std().anchor(GridBagConstraints.WEST));
    add(toggleVisible, GBC.std().anchor(GridBagConstraints.CENTER));
    add(pagination, GBC.eol().anchor(GridBagConstraints.EAST));

    buttons = new ArrayList<>();
    addButtons();
    /* End filter signs */
  }

  private void updateDetectionPage(int difference) {
    detectionPage += difference;
    if (detectionPage < 0) {
      detectionPage = 0;
    }
    long visible = buttons.parallelStream().filter(b -> b.isFiltered(filterField.getText())).count();
    while (detectionPage * showMaxModel.getNumber().intValue() > visible) {
      detectionPage--;
    }
    updateShown(showMaxModel);
  }

  private void updateShown(SpinnerNumberModel model) {
    buttons.parallelStream().forEach(i -> SwingUtilities.invokeLater(() -> i.setVisible(false)));
    buttons.stream().filter(i -> i.isFiltered(filterField.getText())).skip(
      detectionPage * model.getNumber().longValue()
    ).limit(model.getNumber().longValue()
    ).forEach(i -> SwingUtilities.invokeLater(() -> i.setVisible(true)));
    long notSelected = buttons.parallelStream().filter(Component::isVisible).filter(i -> !i.isSelected()).count();
    long selected = buttons.parallelStream().filter(Component::isVisible).filter(ImageCheckBoxButton::isSelected)
      .count();
    toggleVisible.setSelected(notSelected < selected);
    toggleVisible.invalidate();
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

    JavaFxWrapper<DatePicker> firstSeenPicker = new JavaFxWrapper<>(DatePicker.class);
    JavaFxWrapper<DatePicker> lastSeenPicker = new JavaFxWrapper<>(DatePicker.class);
    firstSeen.add(firstSeenPicker, GBC.eol());
    lastSeen.add(lastSeenPicker, GBC.eol());
    firstSeenPicker.getNode().addEventHandler(
      ActionEvent.ACTION,
      t -> updateDates(firstLast, firstSeenPicker.getNode(), firstSeenPicker.getNode(), lastSeenPicker.getNode())
    );
    lastSeenPicker.getNode().addEventHandler(
      ActionEvent.ACTION,
      t -> updateDates(firstLast, lastSeenPicker.getNode(), firstSeenPicker.getNode(), lastSeenPicker.getNode())
    );

    firstSeenPicker.getNode().setConverter(new LocalDateConverter());
    lastSeenPicker.getNode().setConverter(firstSeenPicker.getNode().getConverter());

    // This is required to ensure that the dialogs don't collapse to nothing
    SwingUtilities.invokeLater(() -> {
      Dimension d = firstSeenPicker.getPreferredSize();
      d.width = (int) Math.ceil(Math.max(d.width * 1.15, firstSeenPicker.getNode().getWidth()));
      d.height = (int) Math.ceil(Math.max(d.height * 1.15, firstSeenPicker.getNode().getHeight()));
      firstSeenPicker.setMinimumSize(d);
      lastSeenPicker.setMinimumSize(d);
    });
  }

  private static void updateDates(String position, DatePicker modified, DatePicker firstSeen, DatePicker lastSeen) {
    LocalDate start = firstSeen.getValue();
    LocalDate end = lastSeen.getValue();
    if (start != null && end != null) {
      if (firstSeen.equals(modified) && start.compareTo(end) > 0) {
        lastSeen.setValue(start);
      } else if (lastSeen.equals(modified) && start.compareTo(end) > 0) {
        lastSeen.setValue(end);
      }
    }
    Filter dateFilter = FILTER_TABLE_MODEL.getFilters().parallelStream()
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
    List<Filter> toRemove = FILTER_TABLE_MODEL.getFilters().parallelStream().filter(f -> f.text.isEmpty())
      .collect(Collectors.toList());
    for (Filter f : toRemove) {
      int index = FILTER_TABLE_MODEL.getFilters().indexOf(f);
      FILTER_TABLE_MODEL.removeFilter(index);
    }
  }

  private static void updateMinDetectionFilter(JSpinner minDetections, Filter filter) {
    if (minDetections.getModel() instanceof SpinnerNumberModel) {
      SpinnerNumberModel model = (SpinnerNumberModel) minDetections.getModel();
      filter.enable = !Integer.valueOf(0).equals(model.getNumber());
      if (filter.enable && model.getNumber() != null) {
        filter.text = "detections_num < " + model.getNumber();
      }
      doFilterAddRemoveWork(filter);
    }
  }

  private static void doFilterAddRemoveWork(Filter filter) {
    int index = FILTER_TABLE_MODEL.getFilters().indexOf(filter);
    if (index < 0 && filter.enable && !filter.text.isEmpty()) {
      FILTER_TABLE_MODEL.addFilter(filter);
    } else if (filter.enable) {
      FILTER_TABLE_MODEL.fireTableRowsUpdated(index, index);
    } else {
      FILTER_TABLE_MODEL.removeFilter(index);
    }

  }

  private void showRelevantObjects(boolean showRelevant) {
    this.showRelevant = showRelevant;
    filterButtons(filterField.getText());
    updateShown(showMaxModel);
  }

  private void toggleVisible(boolean check) {
    if (SwingUtilities.isEventDispatchThread()) {
      MainApplication.worker.execute(() -> toggleVisible(check));
      return;
    }
    FILTER_TABLE_MODEL.pauseUpdates();
    List<Future<?>> futures = buttons.parallelStream().filter(ImageCheckBoxButton::isVisible)
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
    FILTER_TABLE_MODEL.resumeUpdates();
  }

  private void addButtons() {
    JPanel scrollPane = new JPanel(new GridBagLayout());
    getIcons(scrollPane, "signs");
    getIcons(scrollPane, "objects");
    add(scrollPane, GBC.eol().fill().anchor(GridBagConstraints.WEST));
  }

  private void filterButtons(String expr) {
    SwingUtilities.invokeLater(
      () -> buttons.stream().forEach(b -> b.setVisible(b.isFiltered(expr) && (!showRelevant || b.isRelevant())))
    );
  }

  public void getIcons(JComponent panel, String type) {
    if (SwingUtilities.isEventDispatchThread()) {
      MainApplication.worker.submit(() -> getIcons(panel, type));
      return;
    }
    String directory = "mapillary_sprite_source/package_" + type;
    try {
      List<String> files = IOUtils.readLines(ResourceProvider.getResourceAsStream("/images/" + directory),
          Charsets.UTF_8);
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
      Stream.of(showMaxModel.getListeners(ChangeListener.class))
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
    buttons.forEach(b -> b.setSelected(false));
    filterField.setText("");
    showMaxModel.setValue(100);
    while (!FILTER_TABLE_MODEL.getFilters().isEmpty()) {
      FILTER_TABLE_MODEL.removeFilter(0);
    }
    Stream.of(getComponents()).forEach(this::resetSubPanels);
  }

  private void resetSubPanels(Component component) {
    if (component instanceof JavaFxWrapper && ((JavaFxWrapper<?>) component).getNode() instanceof DatePicker) {
      ((DatePicker) ((JavaFxWrapper<?>) component).getNode()).setValue(null);
    } else if (component instanceof JSpinner && ((JSpinner) component).getModel() instanceof SpinnerNumberModel
      && !((JSpinner) component).getModel().equals(showMaxModel)) {
      ((SpinnerNumberModel) ((JSpinner) component).getModel()).setValue(0);
    }
    if (component instanceof JComponent) {
      Stream.of(((JComponent) component).getComponents()).forEach(this::resetSubPanels);
    }
  }
}
