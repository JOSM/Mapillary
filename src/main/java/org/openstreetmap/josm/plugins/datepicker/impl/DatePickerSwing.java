// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.datepicker.impl;

import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.time.temporal.TemporalAccessor;
import java.util.function.Consumer;

import org.openstreetmap.josm.gui.widgets.JosmTextField;
import org.openstreetmap.josm.plugins.datepicker.IDatePicker;
import org.openstreetmap.josm.tools.Logging;
import org.openstreetmap.josm.tools.Utils;

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
        if (date == null) {
            try {
                TemporalAccessor temporal = DateTimeFormatter.ISO_DATE.parse(component.getText());
                this.date = LocalDate.from(temporal);
            } catch (DateTimeParseException e) {
                if (!Utils.removeWhiteSpaces(component.getText()).isEmpty()) {
                    Logging.trace(e);
                    component.setText("");
                }
            }
        } else {
            this.date = date;
            component.setText(date.format(DateTimeFormatter.ISO_DATE));
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
