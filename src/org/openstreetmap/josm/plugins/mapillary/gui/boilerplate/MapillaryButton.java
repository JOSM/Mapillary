// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.boilerplate;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;

import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.JButton;

import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryColorScheme;

public class MapillaryButton extends JButton {
  private static final long serialVersionUID = 2619282066157874464L;

  public MapillaryButton(final Action action) {
    this(action, false);
  }

  public MapillaryButton(final Action action, boolean slim) {
    super(action);
    setForeground(Color.WHITE);
    setBorder(slim ? BorderFactory.createEmptyBorder(3, 4, 3, 4) : BorderFactory.createEmptyBorder(7, 10, 7, 10));
  }

  @Override
  protected void paintComponent(final Graphics g) {
    if (!isEnabled()) {
      g.setColor(MapillaryColorScheme.TOOLBAR_DARK_GREY);
    } else if (getModel().isPressed()) {
      g.setColor(MapillaryColorScheme.MAPILLARY_GREEN.darker().darker());
    } else if (getModel().isRollover()) {
      g.setColor(MapillaryColorScheme.MAPILLARY_GREEN.darker());
    } else {
      g.setColor(MapillaryColorScheme.MAPILLARY_GREEN);
    }
    ((Graphics2D) g).setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
    g.fillRoundRect(0, 0, getWidth(), getHeight(), 3, 3);
    super.paintComponent(g);
  }

  @Override
  public boolean isContentAreaFilled() {
    return false;
  }
}
