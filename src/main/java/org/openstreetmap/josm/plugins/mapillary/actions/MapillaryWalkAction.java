// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.actions;

import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.data.vector.VectorDataSet;
import org.openstreetmap.josm.data.vector.VectorNode;
import org.openstreetmap.josm.data.vector.VectorPrimitive;
import org.openstreetmap.josm.data.vector.VectorRelation;
import org.openstreetmap.josm.data.vector.VectorWay;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.VectorDataSelectionListener;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryMainDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.dialog.MapillaryWalkDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillarySequenceUtils;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.ImageProvider.ImageSizes;
import org.openstreetmap.josm.tools.Shortcut;

import javax.swing.JDialog;
import javax.swing.JOptionPane;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.List;

import static org.openstreetmap.josm.tools.I18n.tr;

/**
 * Walks forward at a given interval.
 *
 * @author nokutu
 */
public class MapillaryWalkAction extends JosmAction implements VectorDataSelectionListener {

    private static final long serialVersionUID = 3454223919402245818L;

    private WalkThread thread;
    private final List<WalkListener> listeners = new ArrayList<>();

    public MapillaryWalkAction() {
        super(tr("Walk mode"), new ImageProvider(MapillaryPlugin.LOGO).setSize(ImageSizes.DEFAULT), tr("Walk mode"),
            Shortcut.registerShortcut("mapillary:mapillaryWalk", tr("Mapillary walk mode"), KeyEvent.CHAR_UNDEFINED,
                Shortcut.NONE),
            false, "mapillary:mapillaryWalk", true);
    }

    @Override
    public void actionPerformed(ActionEvent arg0) {
        MapillaryWalkDialog dialog = new MapillaryWalkDialog();
        JOptionPane pane = new JOptionPane(dialog, JOptionPane.PLAIN_MESSAGE, JOptionPane.OK_CANCEL_OPTION);
        JDialog dlg = pane.createDialog(MainApplication.getMainFrame(), tr("Walk mode"));
        dlg.setMinimumSize(new Dimension(400, 150));
        dlg.setVisible(true);
        if (pane.getValue() != null && (int) pane.getValue() == JOptionPane.OK_OPTION) {
            this.thread = new WalkThread((int) dialog.spin.getValue(), dialog.waitForPicture.isSelected(),
                dialog.followSelection.isSelected(),
                dialog.goForward.isSelected() ? MapillarySequenceUtils.NextOrPrevious.NEXT
                    : MapillarySequenceUtils.NextOrPrevious.PREVIOUS);
            fireWalkStarted();
            this.thread.start();
            MapillaryMainDialog.getInstance().setMode(MapillaryMainDialog.MODE.WALK);
        }
    }

    /**
     * Adds a listener.
     *
     * @param lis
     *        The listener to be added.
     */
    public void addListener(WalkListener lis) {
        this.listeners.add(lis);
    }

    /**
     * Removes a listener.
     *
     * @param lis
     *        The listener to be added.
     */
    public void removeListener(WalkListener lis) {
        this.listeners.remove(lis);
    }

    private void fireWalkStarted() {
        if (this.listeners.isEmpty()) {
            return;
        }
        for (WalkListener lis : this.listeners) {
            lis.walkStarted(this.thread);
        }
    }

    @Override
    protected boolean listenToSelectionChange() {
        return false;
    }

    /**
     * Enabled when a mapillary image is selected.
     */
    @Override
    protected void updateEnabledState() {
        super.updateEnabledState();
        setEnabled(MapillaryLayer.hasInstance() && MapillaryLayer.getInstance().getData().getSelectedNodes().stream()
            .anyMatch(MapillaryImageUtils::isImage));
    }

    @Override
    public void selectionChanged(
        SelectionChangeEvent<VectorPrimitive, VectorNode, VectorWay, VectorRelation, VectorDataSet> event) {
        updateEnabledState();
    }
}
