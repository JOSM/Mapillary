// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui;

import java.awt.Component;

import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;

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
