// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui;

import java.time.LocalDate;
import java.util.function.Consumer;

import javax.swing.JComponent;

/**
 * @author Taylor Smock
 */
public interface IDatePicker<T extends JComponent> {
  void setDate(LocalDate date);

  LocalDate getDate();

  T getComponent();

  void reset();

  void addEventHandler(Consumer<IDatePicker<?>> function);
}
