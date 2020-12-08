// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.datepicker.impl;

import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.function.Consumer;

import org.openstreetmap.josm.gui.widgets.JosmTextField;
import org.openstreetmap.josm.plugins.datepicker.IDatePicker;

/**
 * This is a fall back date picker. It is used when the set date picker fails to
 * load, for what ever reason. Do not directly use this implementation.
 *
 * @author Taylor Smock
 */
public class DatePickerSwing implements IDatePicker<JosmTextField> {
  private LocalDate date;
  private final JosmTextField component = new JosmTextField("", 12);

  public DatePickerSwing() {
    super();
    component.setToolTipText("YYYY-MM-DD");
  }

  @Override
  public void setDate(LocalDate date) {
    this.date = date;
    if (date != null) {
      component.setText(date.format(DateTimeFormatter.ISO_DATE));
    } else {
      component.setText("");
    }
  }

  @Override
  public LocalDate getDate() {
    return date;
  }

  @Override
  public JosmTextField getComponent() {
    return component;
  }

  @Override
  public void reset() {
    component.setText("");
  }

  @Override
  public void addEventHandler(Consumer<IDatePicker<?>> function) {
    component.addFocusListener(new FocusListener() {

      @Override
      public void focusGained(FocusEvent e) {
        // Do nothing
      }

      @Override
      public void focusLost(FocusEvent e) {
        setDate(null);
        function.accept(DatePickerSwing.this);
      }
    });
  }

}
