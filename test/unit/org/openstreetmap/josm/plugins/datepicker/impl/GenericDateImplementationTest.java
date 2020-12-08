// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.datepicker.impl;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.time.LocalDate;
import java.util.Calendar;
import java.util.Date;
import java.util.stream.Stream;

import javax.swing.JFormattedTextField;

import mockit.Invocation;
import mockit.Mock;
import mockit.MockUp;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.extension.RegisterExtension;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import org.openstreetmap.josm.plugins.datepicker.IDatePicker;
import org.openstreetmap.josm.testutils.JOSMTestRules;

/**
 *
 */
class GenericDateImplementationTest {

  static class JFormattedTextFieldMock extends MockUp<JFormattedTextField> {
    private String text = null;

    @Mock
    public String getText() {
      return this.text;
    }

    @Mock
    public void setText(Invocation inv, String text) {
      this.text = text;
      inv.proceed(text);
    }

    public void setText(String text) {
      this.text = text;

    }
  }

  @RegisterExtension
  static JOSMTestRules rules = new JOSMTestRules();
  JFormattedTextFieldMock jformattedTextFieldMock;

  @BeforeEach
  void setUpEach() {
    this.jformattedTextFieldMock = new JFormattedTextFieldMock();
  }

  static Stream<Arguments> getDatePickers() {
    Calendar minCalendar = Calendar.getInstance();
    minCalendar.setTime(new Date(Long.MIN_VALUE));
    Calendar maxCalendar = Calendar.getInstance();
    maxCalendar.setTime(new Date(Long.MAX_VALUE));
    LocalDate zero = LocalDate.of(1, 1, 1);
    LocalDate dec = LocalDate.of(1, 12, 31);
    LocalDate current = LocalDate.now();
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
  void testSetGetDate(IDatePicker<?> datePicker, LocalDate expected) {
    // This check does not test for set text listeners.
    assertDoesNotThrow(() -> datePicker.setDate(expected));
    assertEquals(expected, datePicker.getDate());
  }
}
