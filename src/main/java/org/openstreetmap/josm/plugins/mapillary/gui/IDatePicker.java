// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui;

import java.time.LocalDate;
import java.util.function.Consumer;

import javax.swing.JComponent;

import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.tools.Logging;

/**
 * @author Taylor Smock
 */
public interface IDatePicker<T extends JComponent> {
  void setDate(LocalDate date);

  LocalDate getDate();

  T getComponent();

  void reset();

  void addEventHandler(Consumer<IDatePicker<?>> function);

  public static IDatePicker<? extends JComponent> getNewDatePicker() {
    boolean fx = false;
    boolean useFx = MapillaryProperties.JAVA_FX.get();
    if (useFx) {
      try {
        new DatePickerFx();
        fx = true;
      } catch (UnsupportedClassVersionError e) {
        Logging.error(e);
      }
    }
    if (fx) {
      return new DatePickerFx();
    }
    return new DatePickerSwing();
  }
}
