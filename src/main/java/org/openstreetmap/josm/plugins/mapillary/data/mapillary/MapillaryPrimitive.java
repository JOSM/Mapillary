// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.data.mapillary;

import java.util.Map;
import java.util.HashMap;

import org.openstreetmap.josm.data.osm.AbstractPrimitive;
import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.data.osm.IPrimitive;
import org.openstreetmap.josm.data.osm.IRelation;
import org.openstreetmap.josm.data.osm.IWay;
import org.openstreetmap.josm.data.osm.NameFormatter;
import org.openstreetmap.josm.data.osm.OsmData;
import org.openstreetmap.josm.data.osm.visitor.PrimitiveVisitor;
import org.openstreetmap.josm.gui.mappaint.ElemStyles;
import org.openstreetmap.josm.gui.mappaint.StyleCache;

abstract class MapillaryPrimitive extends AbstractPrimitive {

    private boolean highlighted;
    private final Map<ElemStyles, StyleCache> styleCaches = new HashMap<>();

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

    // Methods required for the JOSM mappaint style engine.
    @Override
    public StyleCache getCachedStyle(ElemStyles elemStyles) {
        return styleCaches.get(elemStyles);
    }

    @Override
    public void setCachedStyle(ElemStyles elemStyles, StyleCache mappaintStyle) {
        this.styleCaches.put(elemStyles, mappaintStyle);
    }

    @Override
    public boolean isCachedStyleUpToDate(ElemStyles elemStyles) {
        // This primitive type does not have a dataSet to sync with, so we can't use a cache index.
        // Invalidation is handled manually by calling clearCachedStyle().
        return styleCaches.get(elemStyles) != null;
    }

    @Override
    public void declareCachedStyleUpToDate(ElemStyles elemStyles) {
        // This primitive type does not have a dataSet to sync with, so there is nothing to do here.
    }

    @Override
    public void clearCachedStyle(){
        this.styleCaches.clear();
    };
}
