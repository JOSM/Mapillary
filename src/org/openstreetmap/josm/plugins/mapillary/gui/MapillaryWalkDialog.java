package org.openstreetmap.josm.plugins.mapillary.gui;

import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.SpinnerModel;
import javax.swing.SpinnerNumberModel;

/**
 * Dialog to set the walk mode options.
 * @author nokutu
 *
 */
public class MapillaryWalkDialog extends JPanel {
  
  private static final long serialVersionUID = -6258767312211941358L;
  
  /** Spin containing the interval value. */
  public SpinnerModel spin;
  /** Whether it must wait for the picture to be downloaded */
  public JCheckBox waitForPicture;

  /**
   * Main constructor
   */
  public MapillaryWalkDialog() {
    JPanel interval = new JPanel();
    spin = new SpinnerNumberModel(3000, 100, 15000, 100);
    interval.add(new JLabel("Interval (miliseconds): "));
    interval.add(new JSpinner(spin));
    add(interval);
    
    waitForPicture = new JCheckBox("Wait for the picture to be downloaded");
    waitForPicture.setSelected(true);
    add(waitForPicture);
  }
}
