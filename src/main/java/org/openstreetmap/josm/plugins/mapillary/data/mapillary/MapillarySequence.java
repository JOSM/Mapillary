// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.data.mapillary;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import org.openstreetmap.josm.data.osm.BBox;
import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.data.osm.IPrimitive;
import org.openstreetmap.josm.data.osm.IWay;
import org.openstreetmap.josm.data.osm.OsmPrimitiveType;
import org.openstreetmap.josm.data.osm.UniqueIdGenerator;
import org.openstreetmap.josm.data.osm.visitor.PrimitiveVisitor;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillarySequenceUtils;

/**
 * A mapillary sequence of images
 *
 * @author Taylor Smock
 */
public class MapillarySequence extends MapillaryPrimitive implements IWay<MapillaryNode> {
    private static final UniqueIdGenerator MAPILLARY_SEQUENCE_GENERATOR = new UniqueIdGenerator();
    private static final MapillaryNode[] EMPTY_NODES = new MapillaryNode[0];
    private MapillaryNode[] nodes = EMPTY_NODES;

    /**
     * Create an empty sequence
     *
     * @param key The sequence key (or id)
     */
    public MapillarySequence(final String key) {
        this.setKeys(Collections.singletonMap(MapillarySequenceUtils.KEY, key));
    }

    @Override
    public UniqueIdGenerator getIdGenerator() {
        return MAPILLARY_SEQUENCE_GENERATOR;
    }

    @Override
    public void accept(PrimitiveVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public BBox getBBox() {
        final BBox tBBox = new BBox();
        this.getNodes().forEach(tBBox::add);
        return tBBox.toImmutable();
    }

    @Override
    public List<? extends IPrimitive> getReferrers(boolean allowWithoutDataset) {
        return Collections.emptyList();
    }

    @Override
    public int getNodesCount() {
        return this.nodes.length;
    }

    @Override
    public MapillaryNode getNode(int index) {
        return this.nodes[index];
    }

    @Override
    public List<MapillaryNode> getNodes() {
        return Collections.unmodifiableList(Arrays.asList(this.nodes));
    }

    @Override
    public List<Long> getNodeIds() {
        return Arrays.stream(this.nodes).map(MapillaryNode::getId).collect(Collectors.toList());
    }

    @Override
    public long getNodeId(int idx) {
        return this.nodes[idx].getId();
    }

    @Override
    public void setNodes(List<MapillaryNode> nodes) {
        if (this.nodes.length > 0) {
            throw new IllegalStateException("MapillarySequence nodes should not change");
        }

        this.nodes = nodes.toArray(EMPTY_NODES);
        nodes.forEach(node -> node.setReferrer(this));
    }

    @Override
    public boolean isClosed() {
        return false;
    }

    @Override
    public MapillaryNode firstNode() {
        return this.nodes[0];
    }

    @Override
    public MapillaryNode lastNode() {
        return this.nodes[this.nodes.length - 1];
    }

    @Override
    public boolean isFirstLastNode(INode n) {
        return this.firstNode().equals(n) || this.lastNode().equals(n);
    }

    @Override
    public boolean isInnerNode(INode n) {
        return !this.isFirstLastNode(n);
    }

    @Override
    public OsmPrimitiveType getType() {
        return OsmPrimitiveType.WAY;
    }

    @Override
    public boolean equals(Object other) {
        if (other instanceof IWay) {
            return Objects.equals(MapillarySequenceUtils.getKey(this), MapillarySequenceUtils.getKey((IWay<?>) other));
        }
        return false;
    }

    @Override
    public int hashCode() {
        return MapillarySequenceUtils.getKey(this).hashCode();
    }
}
