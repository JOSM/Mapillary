// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.lang.reflect.InvocationTargetException;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.stream.Stream;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;

import org.openstreetmap.josm.data.osm.Filter;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.plugins.mapillary.gui.dialog.MapillaryExpertFilterDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.PointObjectLayer;
import org.openstreetmap.josm.tools.Destroyable;
import org.openstreetmap.josm.tools.GBC;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.Logging;

/**
 * @author Taylor Smock
 */
public class ImageCheckBoxButton extends JPanel implements Destroyable, TableModelListener {
  private static final long serialVersionUID = 3659377445718790107L;
  private final transient Filter filter;
  private final JCheckBox jcheckbox;
  private final String imageName;
  private final String[] splitName;

  public ImageCheckBoxButton(String directory, String imageName) {
    super(new GridBagLayout());
    this.imageName = imageName;
    MapillaryExpertFilterDialog.getInstance().getFilterModel().addTableModelListener(this);
    ImageProvider provider = new ImageProvider(directory, imageName);
    splitName = imageName.split("--", -1);
    JButton image = new JButton();
    CompletableFuture<Void> future = provider.getAsync(i -> SwingUtilities.invokeLater(() -> image.setIcon(i)));
    MainApplication.worker.execute(() -> {
      try {
        future.get();
      } catch (InterruptedException e) {
        Logging.error(e);
        Thread.currentThread().interrupt();
      } catch (ExecutionException e) {
        Logging.error(e);
        Logging.error(e.getCause());
      }
    });
    add(image, GBC.std().anchor(GridBagConstraints.WEST));
    String name = splitName[splitName.length - 1].replace(".svg", "");
    if (name.matches("g[0-9]+")) {
      name = splitName[splitName.length - 2];
    }

    name = name.replace("-", " ");
    String filterText = "value=\"" + imageName.replace(".svg", "") + "\"";
    filter = MapillaryExpertFilterDialog.getInstance().getFilterModel().getFilters().parallelStream()
      .filter(f -> f.text.equals(filterText)).findAny()
        .orElse(makeNewFilter());
    filter.text = filterText;

    jcheckbox = new JCheckBox(name);
    add(jcheckbox, GBC.eol().fill());
    image.addActionListener(l -> updateCheckBox(jcheckbox, filter));
    jcheckbox.addActionListener(l -> updateFilters(jcheckbox, filter));

    image.setToolTipText(imageName.replace(".svg", ""));
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
      try {
        SwingUtilities.invokeAndWait(() -> jcheckbox.setSelected(filter.enable));
      } catch (InvocationTargetException e1) {
        Logging.error(e1);
      } catch (InterruptedException e1) {
        Logging.error(e1);
        Thread.currentThread().interrupt();
      }
    }
  }

  /**
   * Get the name of the detection
   *
   * @return The detection name
   */
  public String getDetectionName() {
    return this.imageName;
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
    if (SwingUtilities.isEventDispatchThread()) {
      return MainApplication.worker.submit(() -> setSelected(selected));
    }
    try {
      SwingUtilities.invokeAndWait(() -> jcheckbox.setSelected(selected));
    } catch (InvocationTargetException e) {
      Logging.error(e);
    } catch (InterruptedException e) {
      Logging.error(e);
      Thread.currentThread().interrupt();
    } finally {
      updateFilters(jcheckbox, filter);
    }
    return null;
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
    return layers.isEmpty() || layers.parallelStream().map(PointObjectLayer::getDataSet).flatMap(
      ds -> ds.allPrimitives().parallelStream()
    ).filter(p -> p.hasKey("value")).map(p -> p.get("value")).anyMatch(imageName::contains);
  }
}
