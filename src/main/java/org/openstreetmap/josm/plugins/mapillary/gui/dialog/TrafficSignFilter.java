// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.dialog;

import static org.openstreetmap.josm.tools.I18n.marktr;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ItemEvent;
import java.awt.event.KeyEvent;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.stream.Collectors;

import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.SwingUtilities;

import com.drew.lang.Charsets;
import org.apache.commons.io.IOUtils;

import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.dialogs.ToggleDialog;
import org.openstreetmap.josm.gui.widgets.FilterField;
import org.openstreetmap.josm.gui.widgets.VerticallyScrollablePanel;
import org.openstreetmap.josm.plugins.mapillary.gui.ImageCheckBoxButton;
import org.openstreetmap.josm.tools.GBC;
import org.openstreetmap.josm.tools.I18n;
import org.openstreetmap.josm.tools.Logging;
import org.openstreetmap.josm.tools.ResourceProvider;
import org.openstreetmap.josm.tools.Shortcut;

/**
 * This filters traffic signs
 *
 * @author Taylor Smock
 */
public class TrafficSignFilter extends ToggleDialog {
  private static final long serialVersionUID = 1177890183422385423L;
  private static final String DIALOG_NAME = marktr("Mapillary Traffic Sign Filter");
  private boolean destroyed;
  private final JPanel panel;
  private final List<ImageCheckBoxButton> buttons;

  public TrafficSignFilter() {
    super(DIALOG_NAME, "mapillary-filter", DIALOG_NAME,
        Shortcut.registerShortcut("mapillary:filter", DIALOG_NAME, KeyEvent.CHAR_UNDEFINED, Shortcut.NONE), 200, true);

    panel = new JPanel(new GridBagLayout());

    FilterField filterField = new FilterField().filter(this::filterButtons);
    filterField.setToolTipText(I18n.tr("Filter Mapillary Detections"));
    panel.add(filterField, GBC.std().fill(GridBagConstraints.HORIZONTAL));
    JCheckBox toggleVisible = new JCheckBox(I18n.tr("Select Visible"));
    toggleVisible.addItemListener(l -> toggleVisible(l.getStateChange() == ItemEvent.SELECTED));
    panel.add(toggleVisible, GBC.eol().anchor(GridBagConstraints.EAST));

    buttons = new ArrayList<>();
    addButtons();
    super.createLayout(panel, false, Collections.emptyList());
  }

  private void toggleVisible(boolean check) {
    if (SwingUtilities.isEventDispatchThread()) {
      MainApplication.worker.execute(() -> toggleVisible(check));
      return;
    }
    ImageCheckBoxButton.FILTER_TABLE_MODEL.pauseUpdates();
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
    ImageCheckBoxButton.FILTER_TABLE_MODEL.resumeUpdates();
  }

  private void addButtons() {
    VerticallyScrollablePanel scrollPane = new VerticallyScrollablePanel(new GridBagLayout());
    buttons.addAll(getIcons(scrollPane, "signs"));
    scrollPane.getScrollableTracksViewportWidth();
    scrollPane.add(new JSeparator(), GBC.eol());
    scrollPane.getScrollableTracksViewportWidth();
    buttons.addAll(getIcons(scrollPane, "objects"));
    panel.add(scrollPane.getVerticalScrollPane(), GBC.eol().fill().anchor(GridBagConstraints.WEST));
  }

  private void filterButtons(String expr) {
    SwingUtilities.invokeLater(() -> buttons.stream().forEach(b -> b.setVisible(b.isFiltered(expr))));
  }

  public static List<ImageCheckBoxButton> getIcons(JComponent panel, String type) {
    String directory = "mapillary_sprite_source/package_" + type;
    List<ImageCheckBoxButton> buttons = new ArrayList<>();
    try {
      List<String> files = IOUtils.readLines(ResourceProvider.getResourceAsStream("/images/" + directory),
          Charsets.UTF_8);
      Collections.sort(files);
      for (String file : files) {
        ImageCheckBoxButton button = new ImageCheckBoxButton(directory, file);
        buttons.add(button);
        panel.add(button, GBC.eol().fill(GridBagConstraints.HORIZONTAL).anchor(GridBagConstraints.WEST));
      }
    } catch (IOException e) {
      Logging.error(e);
    }
    return buttons;
  }

  @Override
  public void destroy() {
    if (!destroyed) {
      super.destroy();
      MainApplication.getMap().removeToggleDialog(this);
      buttons.forEach(ImageCheckBoxButton::destroy);
      destroyed = true;
    }
  }
}
