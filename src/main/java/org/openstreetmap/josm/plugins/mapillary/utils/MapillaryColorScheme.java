// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import java.awt.Color;

import javax.swing.JComponent;

public final class MapillaryColorScheme {
  /**
   * Color for unselected images
   */
  public static final Color SEQ_UNSELECTED = Color.decode("#05CB63");
  /**
   * Color for the camera angle indicator of images in unselected sequences
   */
  public static final Color SEQ_UNSELECTED_CA = Color.decode("#187A45");
  /**
   * Color for the marker of images in a selected sequence
   */
  public static final Color SEQ_SELECTED = Color.decode("#00BCFF");
  /**
   * Color for the camera angle indicator of images in selected sequences
   */
  public static final Color SEQ_SELECTED_CA = Color.decode("#356685");
  /**
   * Color for the marker of currently selected images
   */
  public static final Color SEQ_HIGHLIGHTED = new Color(0xf5811a);
  /**
   * Color for the camera angle indicator of the currently selected images
   */
  public static final Color SEQ_HIGHLIGHTED_CA = new Color(0xf5b81a);

  public static final Color SEQ_PRIVATE_SELECTED = new Color(0xdd9aff);
  public static final Color SEQ_PRIVATE_SELECTED_CA = new Color(0xe4aeff);
  public static final Color SEQ_PRIVATE_UNSELECTED = new Color(0xa900ff);
  public static final Color SEQ_PRIVATE_UNSELECTED_CA = new Color(0xba33ff);
  public static final Color SEQ_PRIVATE_HIGHLIGHTED = SEQ_HIGHLIGHTED;
  public static final Color SEQ_PRIVATE_HIGHLIGHTED_CA = SEQ_HIGHLIGHTED_CA;

  public static final Color SEQ_IMPORTED_SELECTED = new Color(0xdddddd);
  public static final Color SEQ_IMPORTED_SELECTED_CA = SEQ_IMPORTED_SELECTED.brighter();
  public static final Color SEQ_IMPORTED_UNSELECTED = new Color(0x999999);
  public static final Color SEQ_IMPORTED_UNSELECTED_CA = SEQ_IMPORTED_UNSELECTED.brighter();
  public static final Color SEQ_IMPORTED_HIGHLIGHTED = new Color(0xbb2222);
  public static final Color SEQ_IMPORTED_HIGHLIGHTED_CA = SEQ_IMPORTED_HIGHLIGHTED.brighter();

  public static final Color MAPILLARY_GREEN = new Color(0x35af6d);
  public static final Color TOOLBAR_DARK_GREY = new Color(0x242528);

  public static final Color IMAGEDETECTION_TRAFFICSIGN = new Color(0xffc01b);
  public static final Color IMAGEDETECTION_UNKNOWN = new Color(0x33bb44);

  private MapillaryColorScheme() {
    // Private constructor to avoid instantiation
  }

  /**
   * Styles the given components as default panels (currently only the background is set to white)
   * @param components the components to style as default panels (e.g. checkboxes also, that's why
   *   not only JPanels are accepted)
   */
  public static void styleAsDefaultPanel(JComponent... components) {
    if (components != null && components.length >= 1) {
      for (JComponent component : components) {
        component.setBackground(Color.WHITE);
      }
    }
  }
}
