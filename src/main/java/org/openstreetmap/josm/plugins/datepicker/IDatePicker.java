// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.datepicker;

import java.time.Instant;
import java.time.LocalDate;
import java.util.function.Consumer;

import javax.swing.JComponent;

import org.openstreetmap.josm.plugins.datepicker.impl.DatePickerSwing;
import org.openstreetmap.josm.spi.preferences.Config;
import org.openstreetmap.josm.tools.Logging;

/**
 * This is a generic interface for date pickers. Use
 * {@link IDatePicker#getNewDatePicker} to get a new date picker. It uses the
 * {@code "datepicker.classname"} setting to determine the appropriate class.
 *
 * @author Taylor Smock
 */
public interface IDatePicker<T extends JComponent> {
  /**
   * Set the date
   *
   * @param date The date to set
   */
  void setInstant(Instant date);

  /**
   * Get the date
   *
   * @return The date
   */
  Instant getInstant();

  /**
   * Get the component
   *
   * @return The component to be added to a UI
   */
  T getComponent();

  /**
   * Reset the component
   */
  void reset();

  /**
   * Add an event handler for when the date changes
   *
   * @param function The function to call when the date changes
   */
  void addEventHandler(Consumer<IDatePicker<?>> function);

  /**
   * Get a new date picker. The implementation may change. Only rely on this
   * interface.
   *
   * @return A new date picker
   */
  @SuppressWarnings("unchecked")
  static IDatePicker<JComponent> getNewDatePicker() {
    String datePicker = Config.getPref().get("datepicker.classname",
      "org.openstreetmap.josm.plugins.datepicker.impl.DatePickerJDatePicker");
    try {
      Class<?> cls = Class.forName(datePicker);
      Object instance = cls.getConstructor().newInstance();
      if (instance instanceof IDatePicker) {
        return (IDatePicker<JComponent>) instance;
      }
    } catch (ReflectiveOperationException e) {
      Logging.error(e);
    }
    // Fall back to basic text entry
    IDatePicker<?> rDatePicker = new DatePickerSwing();
    return (IDatePicker<JComponent>) rDatePicker;
  }
}
