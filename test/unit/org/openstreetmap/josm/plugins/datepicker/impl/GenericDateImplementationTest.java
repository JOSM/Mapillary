// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.datepicker.impl;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneOffset;
import java.time.temporal.ChronoUnit;
import java.util.stream.Stream;

import org.openstreetmap.josm.plugins.datepicker.IDatePicker;
import org.openstreetmap.josm.testutils.JOSMTestRules;

import org.junit.jupiter.api.extension.RegisterExtension;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * Test method for date implementations.
 * The {@link #getDatePickers} method should be updated when new date pickers are implemented.
 */
class GenericDateImplementationTest {
  @RegisterExtension
  static JOSMTestRules rules = new JOSMTestRules();

  static Stream<Arguments> getDatePickers() {
    Instant zero = LocalDate.of(1, 1, 1).atStartOfDay(ZoneOffset.UTC).toInstant();
    Instant dec = LocalDate.of(1, 12, 31).atStartOfDay(ZoneOffset.UTC).toInstant();
    Instant current = Instant.now();
    // Don't use LocalDate.MAX and LocalDate.MIN due to conversions between Date and LocalDate
    Instant max = Instant.MAX;
    Instant min = Instant.MIN;
    return Stream.of(new DatePickerJDatePicker(), new DatePickerSwing())
      .flatMap(datePicker -> Stream.of(Arguments.of(datePicker, min), Arguments.of(datePicker, max),
        Arguments.of(datePicker, dec), Arguments.of(datePicker, current), Arguments.of(datePicker, zero),
        Arguments.of(datePicker, null)));
  }

  @MethodSource("getDatePickers")
  @ParameterizedTest
  void testSetGetDate(final IDatePicker<?> datePicker, final Instant expected) {
    // This check does not test for set text listeners.
    final Instant expectedInstant = expected != null ? expected : Instant.EPOCH;
    assertDoesNotThrow(() -> datePicker.setInstant(expectedInstant));
    assertEquals(expectedInstant.truncatedTo(ChronoUnit.DAYS), datePicker.getInstant());
  }
}
