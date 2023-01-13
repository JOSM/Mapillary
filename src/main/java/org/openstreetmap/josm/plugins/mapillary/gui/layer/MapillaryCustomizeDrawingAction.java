// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.layer;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;

import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.layer.Layer;
import org.openstreetmap.josm.gui.layer.gpx.CustomizeDrawingAction;
import org.openstreetmap.josm.gui.util.GuiHelper;
import org.openstreetmap.josm.plugins.mapillary.gui.preferences.display.MapillaryGPXSettingsPanel;
import org.openstreetmap.josm.tools.ImageProvider;

/**
 * Customize Mapillary drawing
 */
public class MapillaryCustomizeDrawingAction extends AbstractAction
    implements Layer.LayerAction, Layer.MultiLayerAction {
    private transient List<Layer> layers;

    /**
     * Create a new {@link CustomizeDrawingAction}
     *
     * @param l The layers that should be customized
     */
    public MapillaryCustomizeDrawingAction(List<Layer> l) {
        this();
        layers = l;
    }

    /**
     * Create a new {@link CustomizeDrawingAction}
     *
     * @param l The layer that should be customized
     */
    public MapillaryCustomizeDrawingAction(Layer l) {
        this();
        layers = new LinkedList<>();
        layers.add(l);
    }

    private MapillaryCustomizeDrawingAction() {
        super(tr("Customize Mapillary track drawing"));
        new ImageProvider("preference").getResource().attachImageIcon(this, true);
    }

    @Override
    public boolean supportLayers(List<Layer> layers) {
        return layers.stream().allMatch(MapillaryLayer.class::isInstance);
    }

    @Override
    public Component createMenuComponent() {
        return new JMenuItem(this);
    }

    @Override
    public Action getMultiLayerAction(List<Layer> layers) {
        return new CustomizeDrawingAction(layers);
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        MapillaryGPXSettingsPanel panel = new MapillaryGPXSettingsPanel(layers.stream()
            .filter(MapillaryLayer.class::isInstance).map(MapillaryLayer.class::cast).collect(Collectors.toList()));
        JScrollPane scrollpane = GuiHelper.embedInVerticalScrollPane(panel);
        scrollpane.setBorder(BorderFactory.createEmptyBorder(0, 0, 0, 0));
        int answer = JOptionPane.showConfirmDialog(MainApplication.getMainFrame(), scrollpane,
            tr("Customize track drawing"), JOptionPane.OK_CANCEL_OPTION, JOptionPane.QUESTION_MESSAGE);
        if (answer == JOptionPane.CANCEL_OPTION || answer == JOptionPane.CLOSED_OPTION) {
            return;
        }
        panel.savePreferences();
        MainApplication.getMainPanel().repaint();
        layers.forEach(Layer::invalidate);
    }

}
