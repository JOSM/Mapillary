// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui;

import org.openstreetmap.josm.gui.SideButton;
import org.openstreetmap.josm.gui.dialogs.ToggleDialog;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.actions.MapillarySubmitCurrentChangesetAction;
import org.openstreetmap.josm.plugins.mapillary.history.MapillaryRecord;
import org.openstreetmap.josm.plugins.mapillary.history.commands.MapillaryCommand;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryChangesetListener;
import org.openstreetmap.josm.tools.GBC;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.Shortcut;

import javax.swing.*;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeSelectionModel;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.Arrays;
import java.util.HashSet;
import java.util.concurrent.ConcurrentHashMap;

import static org.openstreetmap.josm.tools.I18n.tr;

/**
 * Toggle dialog that shows you the latest {@link MapillaryCommand} done and
 * allows the user to revert them.
 *
 * @author nokutu
 * @see MapillaryRecord
 * @see MapillaryCommand
 */
public class MapillaryChangesetDialog extends ToggleDialog implements
        MapillaryChangesetListener {

    private static final long serialVersionUID = -3019715241209349372L;

    private static MapillaryChangesetDialog instance;

    private final DefaultTreeModel changesetTreeModel = new DefaultTreeModel(
            new DefaultMutableTreeNode());
    private final JTree changesetTree = new JTree(this.changesetTreeModel);

    private final JSeparator separator = new JSeparator();
    private final Component spacer = Box.createRigidArea(new Dimension(0, 3));

    private final SideButton submitButton;

    private final ConcurrentHashMap<Object, MapillaryAbstractImage> map;

    private MapillaryChangesetDialog() {
        super(tr("Current Mapillary changeset"), "mapillaryhistory.png",
                tr("Open Mapillary changeset dialog"), Shortcut.registerShortcut(
                        tr("Mapillary changeset"), tr("Open Mapillary changeset dialog"),
                        KeyEvent.VK_9, Shortcut.NONE), 200);

        MapillaryLayer.getInstance().getData().addChangesetListener(this);
        this.map = new ConcurrentHashMap<>();

        this.changesetTree.expandRow(0);
        this.changesetTree.setShowsRootHandles(true);
        this.changesetTree.setRootVisible(false);
        this.changesetTree.setCellRenderer(new MapillaryCellRenderer());
        this.changesetTree.getSelectionModel().setSelectionMode(
                TreeSelectionModel.SINGLE_TREE_SELECTION);
        this.changesetTree.addMouseListener(new MouseEventHandler());

        JPanel treesPanel = new JPanel(new GridBagLayout());
        treesPanel.add(this.spacer, GBC.eol());
        this.spacer.setVisible(false);
        treesPanel
                .add(this.changesetTree, GBC.eol().fill(GridBagConstraints.HORIZONTAL));
        this.separator.setVisible(false);
        treesPanel.add(this.separator, GBC.eol()
                .fill(GridBagConstraints.HORIZONTAL));
        treesPanel.add(Box.createRigidArea(new Dimension(0, 0)),
                GBC.std().weight(0, 1));

        this.submitButton = new SideButton(new SubmitAction());

        createLayout(treesPanel, true,
                Arrays.asList(new SideButton[]{this.submitButton}));
        buildTree();
    }

    /**
     * Returns the unique instance of the class.
     *
     * @return The unique instance of the class.
     */
    public static synchronized MapillaryChangesetDialog getInstance() {
        if (instance == null)
            instance = new MapillaryChangesetDialog();
        return instance;
    }

    private void buildTree() {
        this.submitButton.setEnabled(true);
        HashSet<MapillaryImage> changeset = MapillaryLayer.getInstance().getData().getChangeset();
        if (!changeset.isEmpty())
            this.submitButton.setEnabled(true);
        else
            this.submitButton.setEnabled(false);

        DefaultMutableTreeNode changesetRoot = new DefaultMutableTreeNode();

        this.map.clear();
        for (MapillaryImage command : changeset) {
            if (command != null) {
                DefaultMutableTreeNode node = new DefaultMutableTreeNode(command.toString());
                this.map.put(node, command);
                changesetRoot.add(node);
            }
        }

        this.spacer.setVisible(changeset.isEmpty());

        this.changesetTreeModel.setRoot(changesetRoot);
    }

    @Override
    public void changesetChanged(final MapillaryAbstractImage image) {
        if (!SwingUtilities.isEventDispatchThread()) {
            SwingUtilities.invokeLater(new Runnable() {
                @Override
                public void run() {
                    csChanged(image);
                }
            });
        } else {
            buildTree();
        }

    }

    private void csChanged(MapillaryAbstractImage image) {
        buildTree();
    }

    private class SubmitAction extends AbstractAction {

        private static final long serialVersionUID = -2761935780353053512L;

        public SubmitAction() {
            putValue(NAME, tr("Submit"));
            putValue(SMALL_ICON, ImageProvider.get("upload"));
        }

        @Override
        public void actionPerformed(ActionEvent arg0) {
            MapillarySubmitCurrentChangesetAction submitCurrentChangesetAction = new MapillarySubmitCurrentChangesetAction();
            submitCurrentChangesetAction.actionPerformed(null);
        }
    }

    private static class MapillaryCellRenderer extends DefaultTreeCellRenderer {

        private static final long serialVersionUID = -3129520241562296901L;

        @Override
        public Component getTreeCellRendererComponent(
                JTree tree, Object value, boolean sel, boolean expanded, boolean leaf, int row, boolean hasFocus
        ) {
            super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row, hasFocus);
            setIcon(ImageProvider.get("data/node.png"));
            return this;
        }
    }

    /**
     * Destroys the unique instance of the class.
     */
    public static void destroyInstance() {
        MapillaryChangesetDialog.instance = null;
    }

    private class MouseEventHandler implements MouseListener {

        @Override
        public void mouseClicked(MouseEvent e) {
            // Method is enforced by MouseListener, but (currently) not needed
        }

        @Override
        public void mouseEntered(MouseEvent e) {
            // Method is enforced by MouseListener, but (currently) not needed
        }

        @Override
        public void mouseExited(MouseEvent e) {
            // Method is enforced by MouseListener, but (currently) not needed
        }

        @Override
        public void mousePressed(MouseEvent e) {
            if (e.getClickCount() == 2) {
                if (changesetTree.getSelectionPath() == null) {
//                    MapillaryCommand cmd = map.get(changesetTree.getSelectionPath().getLastPathComponent());
//                    if (!(cmd instanceof CommandDelete)) {
//                        MapillaryUtils.showPictures(cmd.images, true);
//                    }
                }
            }
        }

        @Override
        public void mouseReleased(MouseEvent e) {
            // Method is enforced by MouseListener, but (currently) not needed
        }
    }


}