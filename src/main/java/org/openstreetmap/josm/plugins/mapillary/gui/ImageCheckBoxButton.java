// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Future;
import java.util.stream.Stream;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;

import org.openstreetmap.josm.data.osm.Filter;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.util.GuiHelper;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.ObjectDetections;
import org.openstreetmap.josm.plugins.mapillary.gui.dialog.MapillaryExpertFilterDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.PointObjectLayer;
import org.openstreetmap.josm.tools.Destroyable;
import org.openstreetmap.josm.tools.GBC;

/**
 * @author Taylor Smock
 */
public class ImageCheckBoxButton extends JPanel implements Destroyable, TableModelListener {
  private static final long serialVersionUID = 3659377445718790107L;
  private final transient Filter filter;
  private final JCheckBox jcheckbox;
  private final String detection;
  private final String[] splitName;
  private final ObjectDetections[] detections;

  public ImageCheckBoxButton(ImageIcon icon, String detection, ObjectDetections... detections) {
    super(new GridBagLayout());
    this.detection = detection;
    this.detections = detections;
    MapillaryExpertFilterDialog.getInstance().getFilterModel().addTableModelListener(this);
    splitName = detection.split("--", -1);
    JButton image = new JButton();
    image.setIcon(icon);
    add(image, GBC.std().anchor(GridBagConstraints.WEST));
    String name = splitName[splitName.length - 1].replace(".svg", "");
    if (name.matches("g[0-9]+")) {
      name = splitName[splitName.length - 2];
    }

    name = name.replace("-", " ");
    String filterText = "value:\"" + detection + "\"";
    filter = MapillaryExpertFilterDialog.getInstance().getFilterModel().getFilters().parallelStream()
      .filter(f -> f.text.equals(filterText)).findAny().orElse(makeNewFilter());
    filter.text = filterText;

    jcheckbox = new JCheckBox(name);
    add(jcheckbox, GBC.eol().fill());
    image.addActionListener(l -> updateCheckBox(jcheckbox, filter));
    jcheckbox.addActionListener(l -> updateFilters(jcheckbox, filter));

    image.setToolTipText(detection);
    jcheckbox.setToolTipText(image.getToolTipText());

    tableChanged(null);
  }

  private static Filter makeNewFilter() {
    Filter filter = new Filter();
    filter.hiding = true;
    return filter;
  }

  private static void updateCheckBox(JCheckBox jcheckbox, Filter filter) {
    jcheckbox.setSelected(!jcheckbox.isSelected());
    updateFilters(jcheckbox, filter);
  }

  private static void updateFilters(JCheckBox jcheckbox, Filter filter) {
    int index = MapillaryExpertFilterDialog.getInstance().getFilterModel().getFilters().indexOf(filter);
    if (jcheckbox.isSelected()) {
      filter.enable = true;
    } else if (!jcheckbox.isSelected()) {
      filter.enable = false;
      MapillaryExpertFilterDialog.getInstance().getFilterModel().removeFilter(index);
    }
    if (index < 0 && filter.enable) {
      MapillaryExpertFilterDialog.getInstance().getFilterModel().addFilter(filter);
    }
  }

  @Override
  public void destroy() {
    MapillaryExpertFilterDialog.getInstance().getFilterModel().removeTableModelListener(this);
  }

  @Override
  public void tableChanged(TableModelEvent e) {
    if (filter == null) {
      return;
    }
    final int index = MapillaryExpertFilterDialog.getInstance().getFilterModel().getFilters().indexOf(filter);
    if (index < 0) {
      filter.enable = false;
    }
    if (e != null && (e.getFirstRow() > index || e.getLastRow() < index) && e.getType() != TableModelEvent.DELETE) {
      return;
    }
    if (SwingUtilities.isEventDispatchThread()) {
      jcheckbox.setSelected(filter.enable);
    } else {
      // Invoke in GUI, but don't wait for it (may be blocked)
      GuiHelper.runInEDT(() -> jcheckbox.setSelected(filter.enable));
    }
  }

  /**
   * Get the name of the detection
   *
   * @return The detection name
   */
  public String getDetectionName() {
    return this.detection;
  }

  /**
   * Get the detections for this button
   *
   * @return The detections
   */
  public ObjectDetections[] getDetections() {
    return this.detections;
  }

  /**
   * @param searchString The string
   * @return true if the button shouldn't be visible
   */
  public boolean isFiltered(String searchString) {
    String[] searchSplit = searchString.split(" ", -1);
    return Stream.of(splitName).parallel()
      .anyMatch(n -> Stream.of(searchSplit).parallel().anyMatch(s -> s.contains(n) || n.contains(s)));
  }

  /**
   * @param selected Set the checkbox state to the selected boolean
   * @return A future to indicate if the call finished
   */
  public Future<?> setSelected(boolean selected) {
    CompletableFuture<?> completableFuture = new CompletableFuture<>();
    GuiHelper.runInEDT(() -> {
      jcheckbox.setSelected(selected);
      updateFilters(jcheckbox, filter);
      completableFuture.complete(null);
    });
    return completableFuture;
  }

  @Override
  public void setVisible(boolean visible) {
    super.setVisible(visible);
  }

  @Override
  public boolean isVisible() {
    return super.isVisible();
  }

  /**
   * @return {@code true} if selected (see {@link JCheckBox#isSelected}).
   */
  public boolean isSelected() {
    return jcheckbox.isSelected();
  }

  /**
   * @return {@code true} if a point object layer has it. Or if there are no point object layers.
   */
  public boolean isRelevant() {
    List<PointObjectLayer> layers = MainApplication.getLayerManager().getLayersOfType(PointObjectLayer.class);
    return layers.isEmpty()
      || layers.parallelStream().map(PointObjectLayer::getDataSet).flatMap(ds -> ds.allPrimitives().parallelStream())
        .filter(p -> p.hasKey("value")).map(p -> p.get("value")).anyMatch(p -> p.contains(detection));
  }
}
