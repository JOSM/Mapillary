// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

import org.openstreetmap.josm.tools.date.DateUtils;

/**
 *
 */
public class LocalDateConverter {

  public String toString(LocalDate date) {
    DateTimeFormatter dateFormatter = DateTimeFormatter.ofPattern(getDateFormat());
    return date == null ? "" : dateFormatter.format(date);
  }

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
