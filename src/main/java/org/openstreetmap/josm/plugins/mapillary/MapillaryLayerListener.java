package org.openstreetmap.josm.plugins.mapillary;

import java.util.Collections;
import java.util.stream.Stream;

import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.data.vector.VectorDataSet;
import org.openstreetmap.josm.data.vector.VectorNode;
import org.openstreetmap.josm.data.vector.VectorPrimitive;
import org.openstreetmap.josm.data.vector.VectorRelation;
import org.openstreetmap.josm.data.vector.VectorWay;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.MapFrame;
import org.openstreetmap.josm.gui.layer.LayerManager;
import org.openstreetmap.josm.gui.layer.geoimage.GeoImageLayer;
import org.openstreetmap.josm.gui.layer.geoimage.ImageViewerDialog;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.VectorDataSelectionListener;
import org.openstreetmap.josm.plugins.mapillary.gui.imageinfo.ImageInfoPanel;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.geoimage.MapillaryImageEntry;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
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
            dataSet.addSelectionListener(ImageDialogListener.getInstance());
            Stream.of(MapillaryPlugin.getMapillaryDataListeners()).forEach(dataSet::addSelectionListener);
        }
    }

    @Override
    public void layerRemoving(LayerManager.LayerRemoveEvent e) {
        if (e.getRemovedLayer() instanceof MapillaryLayer) {
            VectorDataSet dataSet = ((MapillaryLayer) e.getRemovedLayer()).getData();
            dataSet.removeSelectionListener((VectorDataSelectionListener) ImageInfoPanel.getInstance());
            dataSet.removeSelectionListener(ImageDialogListener.getInstance());
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

    /**
     * A class that listens for dataset updates and updates the image viewer
     */
    private static class ImageDialogListener implements VectorDataSelectionListener {
        private static ImageDialogListener instance;

        public static ImageDialogListener getInstance() {
            if (instance == null) {
                instance = new ImageDialogListener();
            }
            return instance;
        }

        @Override
        public void selectionChanged(
            SelectionChangeEvent<VectorPrimitive, VectorNode, VectorWay, VectorRelation, VectorDataSet> event) {
            ensureImageViewerDialogEnabled();
            event.getAdded().stream().filter(MapillaryImageUtils::isImage).filter(INode.class::isInstance)
                .map(INode.class::cast).findFirst().map(MapillaryImageEntry::getCachedEntry)
                .ifPresent(ImageViewerDialog.getInstance()::displayImage);
        }

        /**
         * Ensure that the image viewer dialog is created
         */
        private static void ensureImageViewerDialogEnabled() {
            MapFrame map = MainApplication.getMap();
            if (map != null && map.getToggleDialog(ImageViewerDialog.class) == null) {
                // GeoImageLayer should do all the setup when hookUpMapView is called.
                final GeoImageLayer geoImageLayer = new GeoImageLayer(Collections.emptyList(), null);
                geoImageLayer.hookUpMapView();
                geoImageLayer.destroy();
            }
        }
    }
}
