// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui;

import java.awt.Component;

import javax.swing.Icon;
import javax.swing.JTree;
import javax.swing.tree.DefaultTreeCellRenderer;

import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.tools.ImageProvider;

/**
 * Renders an item in a {@link JTree} that represents a {@link MapillaryAbstractImage}.
 */
public class MapillaryImageTreeCellRenderer extends DefaultTreeCellRenderer {
  private static final long serialVersionUID = 5359276673450659572L;

  private static final Icon ICON = new ImageProvider("mapicon").setMaxSize(16).get();

  @Override
  public Component getTreeCellRendererComponent(
    JTree tree, Object value, boolean sel, boolean expanded, boolean leaf, int row, boolean hasFocus
  ) {
    super.getTreeCellRendererComponent(tree, value.toString(), sel, expanded, leaf, row, hasFocus);
    setIcon(ICON);
    return this;
  }
}
