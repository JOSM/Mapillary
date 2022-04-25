package org.openstreetmap.josm.plugins.mapillary.data.mapillary;

import java.util.Collections;
import java.util.List;

import org.openstreetmap.josm.data.coor.EastNorth;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.data.osm.BBox;
import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.data.osm.IPrimitive;
import org.openstreetmap.josm.data.osm.OsmPrimitiveType;
import org.openstreetmap.josm.data.osm.UniqueIdGenerator;
import org.openstreetmap.josm.data.osm.visitor.PrimitiveVisitor;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;

public class MapillaryNode extends MapillaryPrimitive implements INode, IPrimitive {
    private static final UniqueIdGenerator MAPILLARY_NODE_GENERATOR = new UniqueIdGenerator();
    private double lat;
    private double lon;
    private MapillarySequence referrer;

    public MapillaryNode() {
        // Do nothing
    }

    public MapillaryNode(MapillaryNode clone) {
        this.lat = clone.lat;
        this.lon = clone.lon;
        // DO NOT CLONE THE REFERRER -- we _specifically_ only allow the referrer to be set _once_.
    }

    @Override
    public double lon() {
        return this.lon;
    }

    @Override
    public double lat() {
        return this.lat;
    }

    @Override
    public UniqueIdGenerator getIdGenerator() {
        return MAPILLARY_NODE_GENERATOR;
    }

    @Override
    public LatLon getCoor() {
        return new LatLon(this.lat, this.lon);
    }

    @Override
    public void setCoor(LatLon coor) {
        this.lat = coor.lat();
        this.lon = coor.lon();
    }

    @Override
    public void setEastNorth(EastNorth eastNorth) {
        throw new UnsupportedOperationException("Not yet implemented");
    }

    @Override
    public boolean isReferredByWays(int n) {
        if (n == 1 && this.referrer != null) {
            return true;
        }
        return false;
    }

    @Override
    public void accept(PrimitiveVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public BBox getBBox() {
        return new BBox(this).toImmutable();
    }

    @Override
    public List<? extends IPrimitive> getReferrers(boolean allowWithoutDataset) {
        return this.referrer == null ? Collections.emptyList() : Collections.singletonList(this.referrer);
    }

    void setReferrer(MapillarySequence referrer) {
        if (this.referrer != null) {
            throw new IllegalStateException("MapillaryNode should only have a sequence set once");
        }
        this.referrer = referrer;
    }

    @Override
    public OsmPrimitiveType getType() {
        return OsmPrimitiveType.NODE;
    }

    @Override
    public boolean equals(Object other) {
        if (other instanceof INode) {
            return MapillaryImageUtils.getKey(this) == MapillaryImageUtils.getKey((INode) other);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Long.hashCode(MapillaryImageUtils.getKey(this));
    }

    public MapillarySequence getSequence() {
        return this.referrer;
    }
}
