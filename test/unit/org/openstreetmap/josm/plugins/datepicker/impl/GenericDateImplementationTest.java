// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.datepicker.impl;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneOffset;
import java.util.Calendar;
import java.util.Date;
import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.openstreetmap.josm.plugins.datepicker.IDatePicker;

/**
 * Test method for date implementations.
 * The {@link #getDatePickers} method should be updated when new date pickers are implemented.
 */
class GenericDateImplementationTest {
    static Stream<Arguments> getDatePickers() {
        Calendar minCalendar = Calendar.getInstance();
        minCalendar.setTime(new Date(Long.MIN_VALUE));
        Calendar maxCalendar = Calendar.getInstance();
        maxCalendar.setTime(new Date(Long.MAX_VALUE));
        LocalDate zero = LocalDate.of(1, 1, 1);
        LocalDate dec = LocalDate.of(1, 12, 31);
        LocalDate current = LocalDate.now(ZoneOffset.UTC);
        // Don't use LocalDate.MAX and LocalDate.MIN due to conversions between Date and LocalDate
        LocalDate max = LocalDate.of(maxCalendar.get(Calendar.YEAR), maxCalendar.get(Calendar.MONTH),
            maxCalendar.get(Calendar.DAY_OF_MONTH));
        LocalDate min = LocalDate.of(minCalendar.get(Calendar.YEAR), minCalendar.get(Calendar.MONTH),
            minCalendar.get(Calendar.DAY_OF_MONTH));
        return Stream.of(new DatePickerJDatePicker(), new DatePickerSwing())
            .flatMap(datePicker -> Stream.of(Arguments.of(datePicker, min), Arguments.of(datePicker, max),
                Arguments.of(datePicker, dec), Arguments.of(datePicker, current), Arguments.of(datePicker, zero),
                Arguments.of(datePicker, null)));
    }

    @MethodSource("getDatePickers")
    @ParameterizedTest
    void testSetGetDate(final IDatePicker<?> datePicker, final LocalDate expected) {
        // This check does not test for set text listeners.
        final Instant expectedInstant = expected != null ? Instant.from(expected.atStartOfDay(ZoneOffset.UTC))
            : Instant.EPOCH;
        assertDoesNotThrow(() -> datePicker.setInstant(expectedInstant));
        assertEquals(expectedInstant, datePicker.getInstant());
    }
}
