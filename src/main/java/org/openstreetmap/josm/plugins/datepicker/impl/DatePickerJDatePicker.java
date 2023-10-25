// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.datepicker.impl;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.time.Instant;
import java.util.Calendar;
import java.util.Properties;
import java.util.function.Consumer;

import jakarta.annotation.Nonnull;
import org.jdatepicker.impl.DateComponentFormatter;
import org.jdatepicker.impl.JDatePanelImpl;
import org.jdatepicker.impl.JDatePickerImpl;
import org.jdatepicker.impl.UtilCalendarModel;
import org.openstreetmap.josm.plugins.datepicker.IDatePicker;

/**
 * An implementation of the IDatePicker interface. Do NOT directly use. This may
 * be removed at any time.
 *
 * @author Taylor Smock
 */
public class DatePickerJDatePicker implements IDatePicker<JDatePickerImpl> {
    static final Properties PROPERTIES = new Properties();
    static {
        PROPERTIES.put("text.today", tr("Today"));
        PROPERTIES.put("text.month", tr("Month"));
        PROPERTIES.put("text.year", tr("Year"));
        PROPERTIES.put("text.clear", tr("Clear"));
    }

    private final JDatePickerImpl datePicker;
    private final UtilCalendarModel model;

    /** Create a new {@link DatePickerJDatePicker} component */
    public DatePickerJDatePicker() {
        model = new UtilCalendarModel();
        JDatePanelImpl datePanel = new JDatePanelImpl(model, PROPERTIES);
        this.datePicker = new JDatePickerImpl(datePanel, new DateComponentFormatter());
        this.datePicker.setTextEditable(true);
        this.datePicker.setShowYearButtons(true);
    }

    @Override
    public void setInstant(@Nonnull Instant date) {
        if (Instant.MIN.isBefore(date)) {
            Calendar cal = Calendar.getInstance();
            cal.setTimeInMillis(date.toEpochMilli());
            model.setValue(cal);
        } else {
            reset();
        }
    }

    @Nonnull
    @Override
    public Instant getInstant() {
        Calendar value = model.getValue();
        if (value == null)
            return Instant.MIN;
        return value.toInstant();
    }

    @Nonnull
    @Override
    public JDatePickerImpl getComponent() {
        return this.datePicker;
    }

    @Override
    public void reset() {
        this.datePicker.getJFormattedTextField().setText("");
        model.setValue(null);
    }

    @Override
    public void addEventHandler(@Nonnull Consumer<IDatePicker<?>> function) {
        this.datePicker.addActionListener(action -> function.accept(this));
    }

}
