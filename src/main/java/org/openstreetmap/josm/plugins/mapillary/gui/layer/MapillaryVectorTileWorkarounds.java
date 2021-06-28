package org.openstreetmap.josm.plugins.mapillary.gui.layer;

import java.util.Arrays;
import java.util.Collection;
import java.util.stream.Stream;

import org.openstreetmap.josm.data.osm.INode;

/**
 * This interface literally exists to work around unexpected behavior in Mapillary's vector tiles
 */
public interface MapillaryVectorTileWorkarounds {
  /**
   * Set the selected nodes. This is needed since Mapillary tile entities only have unique ids within a tile (this is
   * arguable a spec violation,
   * depending upon how it is read).
   *
   * @see #getSelected()
   * @param <N> The node type
   * @param nodes The nodes that are actually selected.
   */
  <N extends INode> void setSelected(Collection<N> nodes);

  /**
   * Set the selected nodes. This is needed since Mapillary tile entities only have unique ids within a tile (this is
   * arguable a spec violation,
   * depending upon how it is read).
   *
   * @see #getSelected()
   * @param <N> The node type
   * @param nodes The nodes that are actually selected.
   */
  default <N extends INode> void setSelected(N... nodes) {
    this.setSelected(Arrays.asList(nodes));
  }

  /**
   * Get the selected nodes.
   *
   * @see #setSelected for an explanation why this is necessary.
   * @return The selected nodes.
   */
  Stream<INode> getSelected();
}
