package org.openstreetmap.josm.plugins.mapillary.gui;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
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

/**
 * Toggle dialog that shows you the latest {@link MapillaryCommand} done and
 * allows the user to revert them.
 *
 * @see MapillaryRecord
 * @see MapillaryCommand
 * @author nokutu
 *
 */
public class MapillaryHistoryDialog extends ToggleDialog implements
    MapillaryRecordListener {

  private static final long serialVersionUID = -3019715241209349372L;

  private static MapillaryHistoryDialog INSTANCE;

  private final DefaultTreeModel undoTreeModel = new DefaultTreeModel(
      new DefaultMutableTreeNode());
  private final DefaultTreeModel redoTreeModel = new DefaultTreeModel(
      new DefaultMutableTreeNode());
  private final JTree undoTree = new JTree(this.undoTreeModel);
  private final JTree redoTree = new JTree(this.redoTreeModel);

  private JSeparator separator = new JSeparator();
  private Component spacer = Box.createRigidArea(new Dimension(0, 3));

  private SideButton undoButton;
  private SideButton redoButton;

  private MapillaryHistoryDialog() {
    super(tr("Mapillary history"), "mapillaryhistory.png",
        tr("Open Mapillary history dialog"), Shortcut.registerShortcut(
            tr("Mapillary history"), tr("Open Mapillary history dialog"),
            KeyEvent.VK_M, Shortcut.NONE), 200);

    MapillaryRecord.getInstance().addListener(this);

    this.undoTree.expandRow(0);
    this.undoTree.setShowsRootHandles(true);
    this.undoTree.setRootVisible(false);
    this.undoTree.setCellRenderer(new MapillaryCellRenderer());
    this.redoTree.expandRow(0);
    this.redoTree.setCellRenderer(new MapillaryCellRenderer());
    this.redoTree.setShowsRootHandles(true);
    this.redoTree.setRootVisible(false);

    JPanel treesPanel = new JPanel(new GridBagLayout());
    treesPanel.add(this.spacer, GBC.eol());
    this.spacer.setVisible(false);
    treesPanel.add(this.undoTree, GBC.eol().fill(GridBagConstraints.HORIZONTAL));
    this.separator.setVisible(false);
    treesPanel.add(this.separator, GBC.eol().fill(GridBagConstraints.HORIZONTAL));
    treesPanel.add(this.redoTree, GBC.eol().fill(GridBagConstraints.HORIZONTAL));
    treesPanel.add(Box.createRigidArea(new Dimension(0, 0)),
        GBC.std().weight(0, 1));
    treesPanel.setBackground(this.redoTree.getBackground());

    this.undoButton = new SideButton(new UndoAction());
    this.redoButton = new SideButton(new RedoAction());

    createLayout(treesPanel, true,
        Arrays.asList(new SideButton[] { this.undoButton, this.redoButton }));
  }

  /**
   * Returns the unique instance of the class.
   *
   * @return The unique instance of the class.
   */
  public static MapillaryHistoryDialog getInstance() {
    if (INSTANCE == null)
      INSTANCE = new MapillaryHistoryDialog();
    return INSTANCE;
  }

  private void buildTree() {
    this.redoButton.setEnabled(true);
    this.undoButton.setEnabled(true);
    ArrayList<MapillaryCommand> commands = MapillaryRecord.getInstance().commandList;
    int position = MapillaryRecord.getInstance().position;
    ArrayList<MapillaryCommand> undoCommands = new ArrayList<>();
    if (position >= 0)
      undoCommands = new ArrayList<>(commands.subList(0, position + 1));
    else
      this.undoButton.setEnabled(false);
    ArrayList<MapillaryCommand> redoCommands = new ArrayList<>();
    if (commands.size() > 0 && position + 1 < commands.size())
      redoCommands = new ArrayList<>(commands.subList(position + 1,
          commands.size()));
    else
      this.redoButton.setEnabled(false);

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

    this.separator.setVisible(!undoCommands.isEmpty() || !redoCommands.isEmpty());
    this.spacer.setVisible(undoCommands.isEmpty() && !redoCommands.isEmpty());

    this.undoTreeModel.setRoot(undoRoot);
    this.redoTreeModel.setRoot(redoRoot);
  }

  @Override
  public void recordChanged() {
    buildTree();
  }

  private class UndoAction extends AbstractAction {

    private static final long serialVersionUID = -6435832206342007269L;

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

    private static final long serialVersionUID = -2761935780353053512L;

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

    private static final long serialVersionUID = -3129520241562296901L;

    @Override
    public Component getTreeCellRendererComponent(JTree tree, Object value,
        boolean sel, boolean expanded, boolean leaf, int row, boolean hasFocus) {
      super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row,
          hasFocus);
      setIcon(ImageProvider.get("data/node.png"));
      return this;
    }
  }

  /**
   * Destroys the unique instance of the class.
   */
  public static void destroyInstance() {
    MapillaryHistoryDialog.INSTANCE = null;
  }
}
