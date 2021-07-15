package org.openstreetmap.josm.plugins.mapillary.gui.layer;

import java.util.Arrays;
import java.util.Collection;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.data.vector.VectorDataSet;
import org.openstreetmap.josm.data.vector.VectorNode;

/**
 * This interface literally exists to work around unexpected behavior in Mapillary's vector tiles
 */
public interface MapillaryVectorTileWorkarounds {
  /** The pattern used for ids */
  static final Pattern NUMBER_PATTERN = Pattern.compile("^[0-9]+$");

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
    this.setSelected(Arrays.asList(nodes.clone()));
  }

  /**
   * Get the selected nodes. {@link #setSelected} explains why this is necessary.
   *
   * @see #setSelected
   * @return The selected nodes.
   */
  Stream<INode> getSelected();

  /**
   * Set the node ids (map features/images have integer ids, sequences have a string id)
   *
   * @param idKey The key for the id
   * @param nodeStream The nodes to update
   */
  default void setNodeIds(final String idKey, final Stream<VectorNode> nodeStream) {
    nodeStream.filter(n -> n.hasKey(idKey)).forEach(node -> {
      final String id = node.get(idKey);
      if (NUMBER_PATTERN.matcher(id).matches()) {
        VectorDataSet dataSet = node.getDataSet();
        // Force reindexing
        if (dataSet != null && dataSet.containsNode(node)) {
          dataSet.removePrimitive(node);
          node.setOsmId(Long.parseLong(id), 1);
          dataSet.addPrimitive(node);
        }
      }
    });
  }
}
