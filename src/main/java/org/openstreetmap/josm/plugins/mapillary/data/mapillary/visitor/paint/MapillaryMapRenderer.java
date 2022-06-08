package org.openstreetmap.josm.plugins.mapillary.data.mapillary.visitor.paint;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.Graphics2D;
import java.util.Collections;
import java.util.stream.Stream;

import org.openstreetmap.josm.data.osm.visitor.paint.StyledMapRenderer;
import org.openstreetmap.josm.data.vector.VectorDataSet;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.NavigatableComponent;
import org.openstreetmap.josm.gui.layer.LayerManager;
import org.openstreetmap.josm.gui.layer.imagery.MVTLayer;
import org.openstreetmap.josm.gui.mappaint.ElemStyles;
import org.openstreetmap.josm.gui.mappaint.mapcss.MapCSSStyleSource;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.PointObjectLayer;

public class MapillaryMapRenderer extends StyledMapRenderer {
    private static final MapCSSStyleSource styleSource = new MapCSSStyleSource(
        "resource://mapcss/MapillaryImagery.mapcss", tr("Mapillary"), tr("Mapillary style sheet"));
    private static final ElemStyles styles = new ElemStyles(Collections.singletonList(styleSource));

    /**
     * Constructs a new {@code StyledMapRenderer}.
     *
     * @param g the graphics context. Must not be null.
     * @param nc the map viewport. Must not be null.
     * @throws IllegalArgumentException if {@code g} is null
     * @throws IllegalArgumentException if {@code nc} is null
     */
    public MapillaryMapRenderer(Graphics2D g, NavigatableComponent nc) {
        super(g, nc, false);
        if (!styleSource.isLoaded()) {
            styleSource.loadStyleSource();
        }
        this.setStyles(styles);
    }

    /**
     * Called to clear the style cache when highlights or selections change
     */
    public static void selectionOrHighlightChanged() {
        styles.clearCached();
        LayerManager lm = MainApplication.getLayerManager();
        Stream
            .concat(lm.getLayersOfType(MapillaryLayer.class).stream(),
                lm.getLayersOfType(PointObjectLayer.class).stream())
            .map(MVTLayer::getData).forEach(VectorDataSet::clearMappaintCache);
    }
}
