// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui;

import static org.openstreetmap.josm.tools.I18n.marktr;
import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.GridBagLayout;
import java.awt.event.KeyEvent;
import java.util.Collections;

import javax.swing.JButton;
import javax.swing.JPanel;

import org.openstreetmap.josm.gui.dialogs.ToggleDialog;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.tools.GBC;
import org.openstreetmap.josm.tools.Shortcut;

/**
 * A dialog for Mapillary Point objects
 * <p>
 * See https://blog.mapillary.com/update/2020/01/16/how-to-use-mapillary-point-data-in-openstreetmap-id.html
 *
 * @author Taylor Smock
 */
public class MapillaryPointObjectsDialog extends ToggleDialog {
  private static final long serialVersionUID = 464122697849239035L;
  private static final String NAME = marktr("Mapillary Point Objects");
  private static final String TOOLTIP = marktr("Point Objects");
  private static final String IMAGE = "mapillary-logo";

  public MapillaryPointObjectsDialog() {
    super(
      tr(NAME), IMAGE, tr(TOOLTIP), Shortcut.registerShortcut("mapillary:pointobjects", TOOLTIP, KeyEvent.CHAR_UNDEFINED, Shortcut.NONE), 30
    );
    JPanel panel = new JPanel(new GridBagLayout());
    JButton enabled = new JButton(tr("Enabled"));
    panel.add(enabled, GBC.eol());
    enabled.addChangeListener(e -> MapillaryProperties.POINT_FEATURES.put(enabled.isSelected()));

    createLayout(panel, true, Collections.emptyList());
  }
}
