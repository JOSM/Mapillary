package org.openstreetmap.josm.plugins.mapillary.gui;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.Arrays;

import javax.swing.AbstractAction;
import javax.swing.Box;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.JTree;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.DefaultTreeModel;

import org.openstreetmap.josm.gui.SideButton;
import org.openstreetmap.josm.gui.dialogs.ToggleDialog;
import org.openstreetmap.josm.plugins.mapillary.commands.MapillaryCommand;
import org.openstreetmap.josm.plugins.mapillary.commands.MapillaryRecord;
import org.openstreetmap.josm.plugins.mapillary.commands.MapillaryRecordListener;
import org.openstreetmap.josm.tools.GBC;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.Shortcut;

import javax.swing.tree.DefaultMutableTreeNode;

public class MapillaryHistoryDialog extends ToggleDialog implements
        MapillaryRecordListener {

    public static MapillaryHistoryDialog INSTANCE;

    private final DefaultTreeModel undoTreeModel = new DefaultTreeModel(
            new DefaultMutableTreeNode());
    private final DefaultTreeModel redoTreeModel = new DefaultTreeModel(
            new DefaultMutableTreeNode());
    private final JTree undoTree = new JTree(undoTreeModel);
    private final JTree redoTree = new JTree(redoTreeModel);

    private JSeparator separator = new JSeparator();
    private Component spacer = Box.createRigidArea(new Dimension(0, 3));

    private SideButton undoButton;
    private SideButton redoButton;

    public MapillaryHistoryDialog() {
        super(tr("Mapillary history"), "mapillaryhistory.png",
                tr("Open Mapillary history dialog"), Shortcut.registerShortcut(
                        tr("Mapillary history"),
                        tr("Open Mapillary history dialog"), KeyEvent.VK_M,
                        Shortcut.NONE), 200);

        MapillaryRecord.getInstance().addListener(this);

        undoTree.expandRow(0);
        undoTree.setShowsRootHandles(true);
        undoTree.setRootVisible(false);
        undoTree.setCellRenderer(new MapillaryCellRenderer());
        redoTree.expandRow(0);
        redoTree.setCellRenderer(new MapillaryCellRenderer());
        redoTree.setShowsRootHandles(true);
        redoTree.setRootVisible(false);

        JPanel treesPanel = new JPanel(new GridBagLayout());
        treesPanel.add(spacer, GBC.eol());
        spacer.setVisible(false);
        treesPanel.add(undoTree, GBC.eol().fill(GBC.HORIZONTAL));
        separator.setVisible(false);
        treesPanel.add(separator, GBC.eol().fill(GBC.HORIZONTAL));
        treesPanel.add(redoTree, GBC.eol().fill(GBC.HORIZONTAL));
        treesPanel.add(Box.createRigidArea(new Dimension(0, 0)), GBC.std()
                .weight(0, 1));
        treesPanel.setBackground(redoTree.getBackground());

        undoButton = new SideButton(new UndoAction());
        redoButton = new SideButton(new RedoAction());

        createLayout(treesPanel, true,
                Arrays.asList(new SideButton[] { undoButton, redoButton }));
    }

    public static MapillaryHistoryDialog getInstance() {
        if (INSTANCE == null)
            INSTANCE = new MapillaryHistoryDialog();
        return INSTANCE;
    }

    private void buildTree() {
        redoButton.setEnabled(true);
        undoButton.setEnabled(true);
        ArrayList<MapillaryCommand> commands = MapillaryRecord.getInstance().commandList;
        int position = MapillaryRecord.getInstance().position;
        ArrayList<MapillaryCommand> undoCommands = new ArrayList<>();
        if (position >= 0)
            undoCommands = new ArrayList<>(commands.subList(0, position + 1));
        else
            undoButton.setEnabled(false);
        ArrayList<MapillaryCommand> redoCommands = new ArrayList<>();
        if (commands.size() > 0 && position + 1 < commands.size())
            redoCommands = new ArrayList<>(commands.subList(position + 1,
                    commands.size()));
        else
            redoButton.setEnabled(false);

        DefaultMutableTreeNode redoRoot = new DefaultMutableTreeNode();
        DefaultMutableTreeNode undoRoot = new DefaultMutableTreeNode();

        for (MapillaryCommand command : undoCommands) {
            if (command != null)
                undoRoot.add(new DefaultMutableTreeNode(command.toString()));
        }
        for (MapillaryCommand command : redoCommands) {
            if (command != null)
                redoRoot.add(new DefaultMutableTreeNode(command.toString()));
        }

        separator
                .setVisible(!undoCommands.isEmpty() || !redoCommands.isEmpty());
        spacer.setVisible(undoCommands.isEmpty() && !redoCommands.isEmpty());

        undoTreeModel.setRoot(undoRoot);
        redoTreeModel.setRoot(redoRoot);
    }

    @Override
    public void recordChanged() {
        buildTree();
    }

    private class UndoAction extends AbstractAction {

        public UndoAction() {
            putValue(NAME, tr("Undo"));
            putValue(SMALL_ICON, ImageProvider.get("undo"));
        }

        @Override
        public void actionPerformed(ActionEvent arg0) {
            MapillaryRecord.getInstance().undo();
        }

    }

    private class RedoAction extends AbstractAction {
        public RedoAction() {
            putValue(NAME, tr("Redo"));
            putValue(SMALL_ICON, ImageProvider.get("redo"));
        }

        @Override
        public void actionPerformed(ActionEvent arg0) {
            MapillaryRecord.getInstance().redo();
        }

    }

    private static class MapillaryCellRenderer extends DefaultTreeCellRenderer {
        @Override
        public Component getTreeCellRendererComponent(JTree tree, Object value,
                boolean sel, boolean expanded, boolean leaf, int row,
                boolean hasFocus) {
            super.getTreeCellRendererComponent(tree, value, sel, expanded,
                    leaf, row, hasFocus);
            setIcon(ImageProvider.get("data/node.png"));
            return this;
        }
    }
}
