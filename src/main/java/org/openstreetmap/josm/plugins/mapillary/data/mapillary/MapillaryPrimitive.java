// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.data.mapillary;

import java.util.Map;

import org.openstreetmap.josm.data.osm.AbstractPrimitive;
import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.data.osm.IPrimitive;
import org.openstreetmap.josm.data.osm.IRelation;
import org.openstreetmap.josm.data.osm.IWay;
import org.openstreetmap.josm.data.osm.NameFormatter;
import org.openstreetmap.josm.data.osm.OsmData;
import org.openstreetmap.josm.data.osm.visitor.PrimitiveVisitor;
import org.openstreetmap.josm.gui.mappaint.StyleCache;

abstract class MapillaryPrimitive extends AbstractPrimitive {

    private boolean highlighted;
    private StyleCache styleCache;
    private boolean styleCacheIsUpToDate;

    @Override
    protected void keysChangedImpl(Map<String, String> originalKeys) {
        this.clearCachedStyle();
        this.getReferrers().forEach(IPrimitive::clearCachedStyle);
    }

    @Override
    public void visitReferrers(PrimitiveVisitor visitor) {
        for (IPrimitive obj : this.getReferrers()) {
            if (obj instanceof INode) {
                visitor.visit((INode) obj);
            } else if (obj instanceof IWay) {
                visitor.visit((IWay<?>) obj);
            } else if (obj instanceof IRelation) {
                visitor.visit((IRelation<?>) obj);
            }
        }
    }

    @Override
    public String getDisplayName(NameFormatter formatter) {
        return null;
    }

    @Override
    public void setHighlighted(boolean highlighted) {
        this.highlighted = highlighted;
    }

    @Override
    public boolean isHighlighted() {
        return this.highlighted;
    }

    @Override
    public boolean isTagged() {
        return !this.getKeys().isEmpty();
    }

    @Override
    public boolean isAnnotated() {
        return false;
    }

    @Override
    public boolean hasDirectionKeys() {
        return false;
    }

    @Override
    public boolean reversedDirection() {
        return false;
    }

    @Override
    public OsmData<?, ?, ?, ?> getDataSet() {
        return null;
    }

    @Override
    public int compareTo(IPrimitive o) {
        throw new UnsupportedOperationException("Not yet implemented");
    }

    @Override
    public StyleCache getCachedStyle() {
        return this.styleCache;
    }

    @Override
    public void setCachedStyle(StyleCache mappaintStyle) {
        this.styleCache = mappaintStyle;
    }

    @Override
    public boolean isCachedStyleUpToDate() {
        return this.styleCacheIsUpToDate;
    }

    @Override
    public void declareCachedStyleUpToDate() {
        this.styleCacheIsUpToDate = true;
    }
}
