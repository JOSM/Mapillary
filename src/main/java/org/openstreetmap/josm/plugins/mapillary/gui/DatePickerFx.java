// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui;

import java.awt.Dimension;
import java.time.LocalDate;
import java.util.concurrent.ExecutionException;
import java.util.function.Consumer;

import javafx.event.ActionEvent;
import javafx.scene.control.DatePicker;
import javafx.util.StringConverter;

import org.openstreetmap.josm.gui.util.GuiHelper;
import org.openstreetmap.josm.plugins.javafx.gui.JavaFxWrapper;
import org.openstreetmap.josm.plugins.mapillary.utils.LocalDateConverter;

/**
 *
 */
public class DatePickerFx extends JavaFxWrapper<DatePicker> implements IDatePicker<DatePickerFx> {
  private static final long serialVersionUID = -7646082837904944120L;

  protected static class LocalDateConverterJavaFX extends StringConverter<LocalDate> {
    private static final LocalDateConverter CONVERTER = new LocalDateConverter();

    @Override
    public String toString(LocalDate date) {
      return CONVERTER.toString(date);
    }

    @Override
    public LocalDate fromString(String date) {
      return CONVERTER.fromString(date);
    }

  }

  public DatePickerFx() throws ExecutionException {
    super(DatePicker.class);
    this.getNode().setConverter(new LocalDateConverterJavaFX());
    GuiHelper.runInEDT(() -> {
      Dimension d = getPreferredSize();
      d.width = (int) Math.ceil(Math.max(d.width * 1.15, getNode().getWidth()));
      d.height = (int) Math.ceil(Math.max(d.height * 1.15, getNode().getHeight()));
      getComponent().setMinimumSize(d);
      getComponent().setMinimumSize(d);
    });

  }

  @Override
  public void setDate(LocalDate date) {
    this.getNode().setValue(date);
  }

  @Override
  public LocalDate getDate() {
    return this.getNode().getValue();
  }

  @Override
  public DatePickerFx getComponent() {
    return this;
  }

  @Override
  public void addEventHandler(Consumer<IDatePicker<?>> function) {
    this.getNode().addEventHandler(ActionEvent.ACTION, i -> function.accept(this));
  }

  @Override
  public void reset() {
    this.getNode().setValue(null);
  }
}
