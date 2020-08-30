// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.datepicker.impl;

import java.time.LocalDate;
import java.time.temporal.ChronoField;
import java.util.Properties;
import java.util.function.Consumer;

import org.jdatepicker.impl.DateComponentFormatter;
import org.jdatepicker.impl.JDatePanelImpl;
import org.jdatepicker.impl.JDatePickerImpl;
import org.jdatepicker.impl.UtilDateModel;
import org.openstreetmap.josm.plugins.datepicker.IDatePicker;

import static org.openstreetmap.josm.tools.I18n.tr;

/**
 * An implementation of the IDatePicker interface. Do NOT directly use. This may
 * be removed at any time.
 *
 * @author Taylor Smock
 *
 */
public class DatePickerJDatePicker implements IDatePicker<JDatePickerImpl> {
    final static Properties PROPERTIES = new Properties();
    private boolean reset = true;
    static {
        PROPERTIES.put("text.today", tr("Today"));
        PROPERTIES.put("text.month", tr("Month"));
        PROPERTIES.put("text.year", tr("Year"));
        PROPERTIES.put("text.clear", tr("Clear"));
    }

    private JDatePickerImpl datePicker;

    public DatePickerJDatePicker() {
        UtilDateModel model = new UtilDateModel();
        JDatePanelImpl datePanel = new JDatePanelImpl(model, PROPERTIES);
        this.datePicker = new JDatePickerImpl(datePanel, new DateComponentFormatter());
        this.datePicker.setTextEditable(true);
        this.datePicker.setShowYearButtons(true);
    }

    @Override
    public void setDate(LocalDate date) {
        if (date != null) {
            this.datePicker.getModel().setDate(date.get(ChronoField.YEAR_OF_ERA), date.getMonthValue(),
                    date.getDayOfMonth());
        } else
            reset();
    }

    @Override
    public LocalDate getDate() {
        return this.datePicker.getJFormattedTextField().getText().trim().isEmpty() ? null
                : LocalDate.of(this.datePicker.getModel().getYear(), this.datePicker.getModel().getMonth(),
                        this.datePicker.getModel().getDay());
    }

    @Override
    public JDatePickerImpl getComponent() {
        return this.datePicker;
    }

    @Override
    public void reset() {
        this.datePicker.getJFormattedTextField().setText("");
    }

    @Override
    public void addEventHandler(Consumer<IDatePicker<?>> function) {
        this.datePicker.addActionListener(action -> function.accept(this));
    }

}
