// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;

import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComponent;

public final class MapillaryColorScheme {
  public static final Color MAPILLARY_GREEN = new Color(0x35af6d);
  public static final Color TOOLBAR_DARK_GREY = new Color(0x242528);

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

  public static final class MapillaryButton extends JButton {
    private static final long serialVersionUID = 3824559897922944415L;
    public MapillaryButton(final String text, final Action action) {
      super(action);
      setText(text);
      setForeground(Color.WHITE);
      setBorder(BorderFactory.createEmptyBorder(7, 10, 7, 10));
    }

    @Override
    protected void paintComponent(final Graphics g) {
      if (getModel().isPressed()) {
        g.setColor(MAPILLARY_GREEN.darker().darker());
      } else if (getModel().isRollover()) {
        g.setColor(MAPILLARY_GREEN.darker());
      } else {
        g.setColor(MAPILLARY_GREEN);
      }
      ((Graphics2D) g).setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
      ((Graphics2D) g).fillRoundRect(0, 0, getWidth(), getHeight(), 3, 3);
      super.paintComponent(g);
    }

    @Override
    public boolean isContentAreaFilled() {
      return false;
    }
  }
}
