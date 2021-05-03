package org.openstreetmap.josm.plugins.mapillary.data.mapillary;

import org.openstreetmap.josm.data.osm.event.IDataSelectionListener;
import org.openstreetmap.josm.data.vector.VectorDataSet;
import org.openstreetmap.josm.data.vector.VectorNode;
import org.openstreetmap.josm.data.vector.VectorPrimitive;
import org.openstreetmap.josm.data.vector.VectorRelation;
import org.openstreetmap.josm.data.vector.VectorWay;

/**
 * A convenient listener for selection events in vector data
 */
public interface VectorDataSelectionListener
  extends IDataSelectionListener<VectorPrimitive, VectorNode, VectorWay, VectorRelation, VectorDataSet> {
}
