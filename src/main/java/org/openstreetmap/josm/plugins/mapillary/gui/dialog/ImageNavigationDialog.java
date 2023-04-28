// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.dialog;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.event.MouseInputAdapter;

import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.data.imagery.street_level.IImageEntry;
import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.data.osm.PrimitiveId;
import org.openstreetmap.josm.data.vector.VectorDataSet;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.dialogs.ToggleDialog;
import org.openstreetmap.josm.gui.layer.LayerManager;
import org.openstreetmap.josm.gui.layer.geoimage.IGeoImageLayer;
import org.openstreetmap.josm.gui.util.GuiHelper;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.ImageNavigation;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.MapillaryNode;
import org.openstreetmap.josm.plugins.mapillary.gui.boilerplate.MapillaryButton;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.tools.Shortcut;

/**
 * Create a new image navigation dialog
 */
public class ImageNavigationDialog extends ToggleDialog
    implements LayerManager.LayerChangeListener, IGeoImageLayer.ImageChangeListener {
    /* Shortcut for image navigation: forward */
    private static final Shortcut FORWARD = Shortcut.registerShortcut("mapillary:image_navigation:forward",
        tr("Mapillary: Image Navigation: Forward"), KeyEvent.CHAR_UNDEFINED, Shortcut.NONE);
    /* Shortcut for image navigation: left */
    private static final Shortcut LEFT = Shortcut.registerShortcut("mapillary:image_navigation:left",
        tr("Mapillary: Image Navigation: Left"), KeyEvent.CHAR_UNDEFINED, Shortcut.NONE);
    /* Shortcut for image navigation: right */
    private static final Shortcut RIGHT = Shortcut.registerShortcut("mapillary:image_navigation:right",
        tr("Mapillary: Image Navigation: Right"), KeyEvent.CHAR_UNDEFINED, Shortcut.NONE);
    /* Shortcut for image navigation: turn left */
    private static final Shortcut TURN_LEFT = Shortcut.registerShortcut("mapillary:image_navigation:turn_left",
        tr("Mapillary: Image Navigation: Turn Left"), KeyEvent.CHAR_UNDEFINED, Shortcut.NONE);
    /* Shortcut for image navigation: turn right */
    private static final Shortcut TURN_RIGHT = Shortcut.registerShortcut("mapillary:image_navigation:turn_right",
        tr("Mapillary: Image Navigation: Turn Right"), KeyEvent.CHAR_UNDEFINED, Shortcut.NONE);
    /* Shortcut for image navigation: reverse */
    private static final Shortcut U_TURN = Shortcut.registerShortcut("mapillary:image_navigation:reverse",
        tr("Mapillary: Image Navigation: Reverse"), KeyEvent.CHAR_UNDEFINED, Shortcut.NONE);
    /* Shortcut for image navigation: back */
    private static final Shortcut BACK = Shortcut.registerShortcut("mapillary:image_navigation:back",
        tr("Mapillary: Image Navigation: Back"), KeyEvent.CHAR_UNDEFINED, Shortcut.NONE);
    /* Shortcut for image navigation: 360deg panoramic */
    private static final Shortcut PANO = Shortcut.registerShortcut("mapillary:image_navigation:pano",
        tr("Mapillary: Image Navigation: 360 panoramic"), KeyEvent.CHAR_UNDEFINED, Shortcut.NONE);
    private ImageNavigation imageNavigation;
    private JPanel panel;

    /**
     * Create a new image navigation dialog
     */
    public ImageNavigationDialog() {
        super(tr("Mapillary: Image Navigation"), "mapillary-main", tr("Navigate between different Mapillary images"),
            Shortcut.registerShortcut("mapillary:image_navigation", tr("Mapillary: Image Navigation"), KeyEvent.CHAR_UNDEFINED,
                Shortcut.NONE),
            80);
        MainApplication.getLayerManager().addAndFireLayerChangeListener(this);
    }

    @Override
    public void layerAdded(LayerManager.LayerAddEvent e) {
        if (e.getAddedLayer() instanceof MapillaryLayer) {
            MapillaryLayer layer = (MapillaryLayer) e.getAddedLayer();
            layer.addImageChangeListener(this);
        }
    }

    @Override
    public void layerRemoving(LayerManager.LayerRemoveEvent e) {
        // Don't care
    }

    @Override
    public void layerOrderChanged(LayerManager.LayerOrderChangeEvent e) {
        // Don't care
    }

    @Override
    public void imageChanged(IGeoImageLayer source, List<? extends IImageEntry<?>> oldImages,
        List<? extends IImageEntry<?>> newImages) {
        if (source instanceof MapillaryLayer
            && newImages.stream().filter(MapillaryNode.class::isInstance).count() == 1) {
            MapillaryLayer layer = (MapillaryLayer) source;
            MapillaryNode node = (MapillaryNode) newImages.get(0);
            if (node != null && layer.getData() != null && node.getSequence() != null) {
                this.imageNavigation = new ImageNavigation(layer.getData(), node);
            } else {
                this.imageNavigation = null;
            }
        } else {
            this.imageNavigation = null;
        }
        GuiHelper.runInEDT(this::rebuild);
    }

    @Override
    public void destroy() {
        if (dialogsPanel != null) {
            super.destroy();
        }
    }

    /**
     * This must only be run in the EDT
     */
    private void rebuild() {
        ImageNavigation in = this.imageNavigation;
        JPanel newPanel = new JPanel(new GridLayout(3, 3));
        if (in != null) {
            final INode turnLeft = in.getImage(ImageNavigation.Direction.TURN_LEFT).orElse(null);
            final INode forward = in.getImage(ImageNavigation.Direction.FORWARD).orElse(null);
            final INode turnRight = in.getImage(ImageNavigation.Direction.TURN_RIGHT).orElse(null);
            final INode left = in.getImage(ImageNavigation.Direction.LEFT).orElse(null);
            final INode right = in.getImage(ImageNavigation.Direction.RIGHT).orElse(null);
            final INode uTurn = in.getImage(ImageNavigation.Direction.U_TURN).orElse(null);
            final INode back = in.getImage(ImageNavigation.Direction.BACKWARD).orElse(null);
            final INode pano = in.getImage(ImageNavigation.Direction.THREE_SIXTY).orElse(null);

            newPanel.add(getComponent(tr("Turn left"), TURN_LEFT, in.getDataset(), turnLeft));
            newPanel.add(getComponent(tr("Forward"), FORWARD, in.getDataset(), forward));
            newPanel.add(getComponent(tr("Turn right"), TURN_RIGHT, in.getDataset(), turnRight));
            newPanel.add(getComponent(tr("Left"), LEFT, in.getDataset(), left));
            newPanel.add(new JLabel(""));
            newPanel.add(getComponent(tr("Right"), RIGHT, in.getDataset(), right));
            newPanel.add(getComponent(tr("Reverse"), U_TURN, in.getDataset(), uTurn));
            newPanel.add(getComponent(tr("Back"), BACK, in.getDataset(), back));
            newPanel.add(getComponent(tr("Pano"), PANO, in.getDataset(), pano));
        }

        synchronized (this) {
            if (this.panel != null) {
                this.remove(this.panel);
            }
            this.panel = newPanel;
            this.add(this.panel);
            this.revalidate();
        }
    }

    private static JComponent getComponent(String name, Shortcut shortcut, VectorDataSet dataSet, INode node) {
        MapillaryButton mapillaryButton = new MapillaryButton(new ImageAction(name, shortcut, node));
        if (node != null) {
            mapillaryButton.addMouseListener(new ImageHighlighter(dataSet, node.getPrimitiveId()));
        }
        return mapillaryButton;
    }

    /**
     * Highlight an image on hover
     */
    private static final class ImageHighlighter extends MouseInputAdapter {

        private final VectorDataSet data;
        private final PrimitiveId primitiveId;

        public ImageHighlighter(VectorDataSet data, PrimitiveId primitiveId) {
            Objects.requireNonNull(data);
            Objects.requireNonNull(primitiveId);
            this.data = data;
            this.primitiveId = primitiveId;
        }

        @Override
        public void mouseEntered(MouseEvent e) {
            this.data.setHighlighted(Collections.singleton(this.primitiveId));
        }

        @Override
        public void mouseExited(MouseEvent e) {
            this.data.setHighlighted(Collections.emptyList());
        }
    }

    private static final class ImageAction extends JosmAction {

        private final INode toSelect;

        ImageAction(String name, Shortcut shortcut, INode toSelect) {
            super(name, null, null, shortcut, false);
            this.toSelect = toSelect;
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            MapillaryLayer layer = MapillaryLayer.getInstance();
            if (this.toSelect instanceof MapillaryNode) {
                layer.setCurrentImage((MapillaryNode) this.toSelect);
            } else {
                layer.getData().setSelected(this.toSelect);
            }
        }

        @Override
        public boolean isEnabled() {
            return this.toSelect != null;
        }
    }
}
