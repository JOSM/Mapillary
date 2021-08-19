// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.actions;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.event.ActionEvent;

import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.data.osm.event.IDataSelectionListener;
import org.openstreetmap.josm.data.vector.VectorDataSet;
import org.openstreetmap.josm.data.vector.VectorNode;
import org.openstreetmap.josm.data.vector.VectorPrimitive;
import org.openstreetmap.josm.data.vector.VectorRelation;
import org.openstreetmap.josm.data.vector.VectorWay;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.VectorDataSelectionListener;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.ImageProvider.ImageSizes;

/**
 * Zooms to the currently selected image.
 *
 * @author nokutu
 */
public class MapillaryZoomAction extends JosmAction implements VectorDataSelectionListener {

    private static final long serialVersionUID = -6050566219765623059L;

    /**
     * Main constructor.
     */
    public MapillaryZoomAction() {
        super(tr("Zoom to selected image"), new ImageProvider(MapillaryPlugin.LOGO).setSize(ImageSizes.DEFAULT),
            tr("Zoom to the currently selected Mapillary image"), null, false, "mapillaryZoom", true);
    }

    @Override
    public void actionPerformed(ActionEvent arg0) {
        INode selected = MapillaryLayer.getInstance().getData().getSelectedNodes().stream()
            .filter(MapillaryImageUtils::isImage).findFirst().orElse(null);
        if (selected == null) {
            throw new IllegalStateException();
        }
        MainApplication.getMap().mapView.zoomTo(selected.getCoor());
    }

    @Override
    protected boolean listenToSelectionChange() {
        return false;
    }

    @Override
    protected void updateEnabledState() {
        super.updateEnabledState();
        setEnabled(MapillaryLayer.hasInstance() && MapillaryLayer.getInstance().getData().getSelectedNodes().stream()
            .anyMatch(MapillaryImageUtils::isImage));
    }

    @Override
    public void selectionChanged(
        IDataSelectionListener.SelectionChangeEvent<VectorPrimitive, VectorNode, VectorWay, VectorRelation, VectorDataSet> event) {
        updateEnabledState();
    }
}
