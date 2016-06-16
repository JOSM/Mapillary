// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.PluginState;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;

import static org.openstreetmap.josm.tools.I18n.tr;

/**
 * Dialog shown when a set of images is uploaded.
 *
 * @author nokutu
 *
 */
public class NotLoggedInDialog extends JPanel {

  private static final long serialVersionUID = -2180924089016137840L;

  /**
   * Main constructor.
   */
  public NotLoggedInDialog() {
    this.setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
    JLabel text = new JLabel(tr("You are not loged in, please log in to Mapillary in the preferences"));
    text.setAlignmentX(Component.CENTER_ALIGNMENT);
    this.add(text);
  }
}
