// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.dialog;

import static org.openstreetmap.josm.tools.I18n.marktr;

import java.awt.event.KeyEvent;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JPanel;

import com.drew.lang.Charsets;
import org.apache.commons.io.IOUtils;

import org.openstreetmap.josm.data.osm.Filter;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.dialogs.FilterTableModel;
import org.openstreetmap.josm.gui.dialogs.ToggleDialog;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.Logging;
import org.openstreetmap.josm.tools.Pair;
import org.openstreetmap.josm.tools.ResourceProvider;
import org.openstreetmap.josm.tools.Shortcut;

/**
 * This filters traffic signs
 *
 * @author Taylor Smock
 */
public class TrafficSignFilter extends ToggleDialog {
  private static final String NAME = marktr("Mapillary Traffic Sign Filter");
  private boolean destroyed;
  public TrafficSignFilter() {
    super(NAME, "mapillary-filter", NAME,
        Shortcut.registerShortcut("mapillary:filter", NAME, KeyEvent.CHAR_UNDEFINED, Shortcut.NONE),
        30, true);
    JPanel panel = new JPanel();
    getIcons(panel, "signs");
    getIcons(panel, "objects");
    super.createLayout(panel, true, Collections.emptyList());
  }

  public static void getIcons(JPanel panel, String type) {
    String directory = "mapillary_sprite_source/package_" + type;
    try {
      List<String> files = IOUtils.readLines(ResourceProvider.getResourceAsStream("/images/" + directory),
          Charsets.UTF_8);
      Collections.sort(files);
      List<Pair<String[], JComponent>> components = new ArrayList<>();
      for (String file : files) {
        Pair<String[], JComponent> pair = new Pair<>(file.split("--", -1), new JPanel());
        components.add(pair);
        JButton image = new JButton(ImageProvider.get(directory, file));
        pair.b.add(image);
        JCheckBox jcheckbox = new JCheckBox(pair.a[pair.a.length - 1]);
        pair.b.add(jcheckbox);
        image.addActionListener(l -> updateCheckBox(jcheckbox, file));
        jcheckbox.addActionListener(l -> updateFilters(jcheckbox, file));
      }
    } catch (IOException e) {
      Logging.error(e);
    }
  }

  private static void updateCheckBox(JCheckBox jcheckbox, String name) {
    jcheckbox.setSelected(!jcheckbox.isSelected());
    updateFilters(jcheckbox, name);
  }

  private static void updateFilters(JCheckBox jcheckbox, String name) {
    FilterTableModel model = MainApplication.getMap().filterDialog.getFilterModel();
    Filter filter = model.getFilters().parallelStream().filter(f -> f.text.equals(name)).findAny().orElse(new Filter());
    filter.text = "value=\"" + name.replace(".svg", "") + "\"";
    filter.enable = true;
    filter.hiding = true;
    int index = model.getFilters().indexOf(filter);
    if (jcheckbox.isSelected() && index < 0) {
      model.addFilter(filter);
    } else if (!jcheckbox.isSelected() && index >= 0) {
      model.removeFilter(index);
    }
  }

  @Override
  public void destroy() {
    if (!destroyed) {
      super.destroy();
      MainApplication.getMap().removeToggleDialog(this);
      destroyed = true;
    }
  }
}
