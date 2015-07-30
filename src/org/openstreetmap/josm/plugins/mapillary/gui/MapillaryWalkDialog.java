package org.openstreetmap.josm.plugins.mapillary.gui;

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
  public SpinnerModel spin;
  /** Whether it must wait for the picture to be downloaded */
  public JCheckBox waitForPicture;
  /** Whether the view must follow the selected image. */
  public JCheckBox followSelection;

  /**
   * Main constructor
   */
  public MapillaryWalkDialog() {
    JPanel interval = new JPanel();
    spin = new SpinnerNumberModel(2000, 500, 10000, 500);
    interval.add(new JLabel("Interval (miliseconds): "));
    interval.add(new JSpinner(spin));
    add(interval);

    waitForPicture = new JCheckBox("Wait for full quality pictures");
    waitForPicture.setSelected(true);
    add(waitForPicture);

    followSelection = new JCheckBox("Follow selected image");
    followSelection.setSelected(true);
    add(followSelection);
  }
}
