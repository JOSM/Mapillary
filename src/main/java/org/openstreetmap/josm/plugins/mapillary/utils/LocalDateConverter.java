// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

import javafx.util.StringConverter;

import org.openstreetmap.josm.tools.date.DateUtils;

/**
 *
 */
public class LocalDateConverter extends StringConverter<LocalDate> {

  @Override
  public String toString(LocalDate date) {
    DateTimeFormatter dateFormatter = DateTimeFormatter.ofPattern(getDateFormat());
    return date == null ? "" : dateFormatter.format(date);
  }

  @Override
  public LocalDate fromString(String date) {
    DateTimeFormatter dateFormatter = DateTimeFormatter.ofPattern(getDateFormat());
    return date == null || date.trim().isEmpty() ? null : LocalDate.parse(date, dateFormatter);
  }

  /**
   * @return The date format for Mapillary UI
   */
  public static String getDateFormat() {
    return DateUtils.PROP_ISO_DATES.get() ? "yyyy-MM-dd" : "dd/MM/yyyy";
  }
}
