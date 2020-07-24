// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.boilerplate;

import javax.swing.JSpinner;
import javax.swing.SpinnerNumberModel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

/**
 * Spinner for double values.
 */
public class DoubleSpinner extends JSpinner {

  private static final double STEP_RATIO = 0.1;

  private SpinnerNumberModel model;

  public DoubleSpinner(double min, double max) {
    super();
    // Model setup
    model = new SpinnerNumberModel(0.0, min, max, 0.1);
    this.setModel(model);

    // Step recalculation
    this.addChangeListener(new ChangeListener() {
      @Override
      public void stateChanged(ChangeEvent e) {
        Double value = getDouble();
        // Steps are sensitive to the current magnitude of the value
        long magnitude = Math.round(Math.log10(value));
        double stepSize = Math.max(STEP_RATIO * Math.pow(10, magnitude), 0.001);
        model.setStepSize(stepSize);
      }
    });
  }

  /**
   * Returns the current value as a Double
   */
  public Double getDouble() {
    return (Double) getValue();
  }

}
