package org.openstreetmap.josm.plugins.mapillary;

import java.util.stream.Stream;

import org.openstreetmap.josm.data.vector.VectorDataSet;
import org.openstreetmap.josm.gui.layer.LayerManager;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.VectorDataSelectionListener;
import org.openstreetmap.josm.plugins.mapillary.gui.imageinfo.ImageInfoPanel;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.tools.Destroyable;

/**
 * A listener that looks for new Mapillary Layers, and adds appropriate listeners
 *
 * @author Taylor Smock
 */
public class MapillaryLayerListener implements Destroyable, LayerManager.LayerChangeListener {
    private final LayerManager layerManager;

    public MapillaryLayerListener(LayerManager layerManager) {
        this.layerManager = layerManager;
        this.layerManager.addAndFireLayerChangeListener(this);
    }

    @Override
    public void layerAdded(LayerManager.LayerAddEvent e) {
        if (e.getAddedLayer() instanceof MapillaryLayer) {
            VectorDataSet dataSet = ((MapillaryLayer) e.getAddedLayer()).getData();
            dataSet.addSelectionListener((VectorDataSelectionListener) ImageInfoPanel.getInstance());
            Stream.of(MapillaryPlugin.getMapillaryDataListeners()).forEach(dataSet::addSelectionListener);
        }
    }

    @Override
    public void layerRemoving(LayerManager.LayerRemoveEvent e) {
        if (e.getRemovedLayer() instanceof MapillaryLayer) {
            VectorDataSet dataSet = ((MapillaryLayer) e.getRemovedLayer()).getData();
            dataSet.removeSelectionListener((VectorDataSelectionListener) ImageInfoPanel.getInstance());
            Stream.of(MapillaryPlugin.getMapillaryDataListeners()).forEach(dataSet::removeSelectionListener);
        }
    }

    @Override
    public void layerOrderChanged(LayerManager.LayerOrderChangeEvent e) {
        // Don't care
    }

    @Override
    public void destroy() {
        this.layerManager.removeAndFireLayerChangeListener(this);
    }

}
