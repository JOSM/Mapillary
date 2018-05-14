// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui;

import static org.openstreetmap.josm.tools.I18n.tr;

import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.SpinnerModel;
import javax.swing.SpinnerNumberModel;

/**
 * Dialog to set the walk mode options.
 *
 * @author nokutu
 *
 */
public class MapillaryWalkDialog extends JPanel {

  private static final long serialVersionUID = -6258767312211941358L;

  /** Spin containing the interval value. */
  public final SpinnerModel spin;
  /** Whether it must wait for the picture to be downloaded */
  public final JCheckBox waitForPicture;
  /** Whether the view must follow the selected image. */
  public final JCheckBox followSelection;
  /** Go forward or backwards */
  public final JCheckBox goForward;

  /**
   * Main constructor
   */
  public MapillaryWalkDialog() {
    final JPanel interval = new JPanel();
    this.spin = new SpinnerNumberModel(2000, 500, 10000, 500);
    interval.add(new JLabel("Interval (miliseconds): "));
    interval.add(new JSpinner(this.spin));
    add(interval);

    this.waitForPicture = new JCheckBox(tr("Wait for full quality pictures"));
    this.waitForPicture.setSelected(true);
    add(this.waitForPicture);

    this.followSelection = new JCheckBox(tr("Follow selected image"));
    this.followSelection.setSelected(true);
    add(this.followSelection);

    this.goForward = new JCheckBox(tr("Go forward"));
    this.goForward.setSelected(true);
    add(this.goForward);
  }
}
