// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.datepicker.impl;

import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.time.Instant;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.function.Consumer;

import jakarta.annotation.Nonnull;
import org.openstreetmap.josm.gui.widgets.DisableShortcutsOnFocusGainedTextField;
import org.openstreetmap.josm.gui.widgets.JosmTextField;
import org.openstreetmap.josm.plugins.datepicker.IDatePicker;

/**
 * This is a fallback date picker. It is used when the set date picker fails to
 * load, for what ever reason. Do not directly use this implementation.
 *
 * @author Taylor Smock
 */
public class DatePickerSwing implements IDatePicker<JosmTextField> {
    private Instant date;
    private final JosmTextField component = new DisableShortcutsOnFocusGainedTextField("", 12);

    /** Create a new {@link DatePickerSwing} component */
    public DatePickerSwing() {
        super();
        component.setToolTipText("YYYY-MM-DD");
    }

    @Override
    public void setInstant(@Nonnull Instant date) {
        this.date = date;
        if (Instant.MIN.isBefore(date)) {
            this.component
                .setText(DateTimeFormatter.ISO_DATE.format(ZonedDateTime.ofInstant(this.date, ZoneOffset.UTC)));
        } else {
            component.setText("");
        }
    }

    @Nonnull
    @Override
    public Instant getInstant() {
        return this.date;
    }

    @Nonnull
    @Override
    public JosmTextField getComponent() {
        return component;
    }

    @Override
    public void reset() {
        component.setText("");
    }

    @Override
    public void addEventHandler(@Nonnull Consumer<IDatePicker<?>> function) {
        component.addFocusListener(new FocusListener() {

            @Override
            public void focusGained(FocusEvent e) {
                // Do nothing
            }

            @Override
            public void focusLost(FocusEvent e) {
                setInstant(Instant.MIN);
                function.accept(DatePickerSwing.this);
            }
        });
    }

}
