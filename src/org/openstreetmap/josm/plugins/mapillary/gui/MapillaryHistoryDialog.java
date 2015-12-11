// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.concurrent.ConcurrentHashMap;

import javax.swing.AbstractAction;
import javax.swing.Box;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.JTree;
import javax.swing.SwingUtilities;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeSelectionModel;

import org.openstreetmap.josm.gui.SideButton;
import org.openstreetmap.josm.gui.dialogs.ToggleDialog;
import org.openstreetmap.josm.plugins.mapillary.history.MapillaryRecord;
import org.openstreetmap.josm.plugins.mapillary.history.MapillaryRecordListener;
import org.openstreetmap.josm.plugins.mapillary.history.commands.CommandDelete;
import org.openstreetmap.josm.plugins.mapillary.history.commands.MapillaryCommand;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;
import org.openstreetmap.josm.tools.GBC;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.Shortcut;

/**
 * Toggle dialog that shows you the latest {@link MapillaryCommand} done and
 * allows the user to revert them.
 *
 * @author nokutu
 * @see MapillaryRecord
 * @see MapillaryCommand
 *
 */
public class MapillaryHistoryDialog extends ToggleDialog implements
    MapillaryRecordListener {

  private static final long serialVersionUID = -3019715241209349372L;

  private static MapillaryHistoryDialog instance;

  private final transient UndoRedoSelectionListener undoSelectionListener;
  private final transient UndoRedoSelectionListener redoSelectionListener;

  private final DefaultTreeModel undoTreeModel = new DefaultTreeModel(
      new DefaultMutableTreeNode());
  private final DefaultTreeModel redoTreeModel = new DefaultTreeModel(
      new DefaultMutableTreeNode());
  private final JTree undoTree = new JTree(this.undoTreeModel);
  private final JTree redoTree = new JTree(this.redoTreeModel);

  private final JSeparator separator = new JSeparator();
  private final Component spacer = Box.createRigidArea(new Dimension(0, 3));

  private final SideButton undoButton;
  private final SideButton redoButton;

  private final ConcurrentHashMap<Object, MapillaryCommand> map;

  private MapillaryHistoryDialog() {
    super(tr("Mapillary history"), "mapillaryhistory.png",
        tr("Open Mapillary history dialog"), Shortcut.registerShortcut(
            tr("Mapillary history"), tr("Open Mapillary history dialog"),
            KeyEvent.VK_M, Shortcut.NONE), 200);

    MapillaryRecord.getInstance().addListener(this);

    this.map = new ConcurrentHashMap<>();

    this.undoTree.expandRow(0);
    this.undoTree.setShowsRootHandles(true);
    this.undoTree.setRootVisible(false);
    this.undoTree.setCellRenderer(new MapillaryCellRenderer());
    this.undoTree.getSelectionModel().setSelectionMode(
        TreeSelectionModel.SINGLE_TREE_SELECTION);
    this.undoTree.addMouseListener(new MouseEventHandler());
    this.undoSelectionListener = new UndoRedoSelectionListener(this.undoTree);
    this.undoTree.getSelectionModel().addTreeSelectionListener(
        this.undoSelectionListener);

    this.redoTree.expandRow(0);
    this.redoTree.setCellRenderer(new MapillaryCellRenderer());
    this.redoTree.setShowsRootHandles(true);
    this.redoTree.setRootVisible(false);
    this.redoTree.getSelectionModel().setSelectionMode(
        TreeSelectionModel.SINGLE_TREE_SELECTION);
    this.redoTree.addMouseListener(new MouseEventHandler());
    this.redoSelectionListener = new UndoRedoSelectionListener(this.redoTree);
    this.redoTree.getSelectionModel().addTreeSelectionListener(
        this.redoSelectionListener);

    JPanel treesPanel = new JPanel(new GridBagLayout());
    treesPanel.add(this.spacer, GBC.eol());
    this.spacer.setVisible(false);
    treesPanel
        .add(this.undoTree, GBC.eol().fill(GridBagConstraints.HORIZONTAL));
    this.separator.setVisible(false);
    treesPanel.add(this.separator, GBC.eol()
        .fill(GridBagConstraints.HORIZONTAL));
    treesPanel
        .add(this.redoTree, GBC.eol().fill(GridBagConstraints.HORIZONTAL));
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
    if (instance == null)
      instance = new MapillaryHistoryDialog();
    return instance;
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
    if (!commands.isEmpty() && position + 1 < commands.size())
      redoCommands = new ArrayList<>(commands.subList(position + 1, commands.size()));
    else
      this.redoButton.setEnabled(false);

    DefaultMutableTreeNode redoRoot = new DefaultMutableTreeNode();
    DefaultMutableTreeNode undoRoot = new DefaultMutableTreeNode();

    this.map.clear();
    for (MapillaryCommand command : undoCommands) {
      if (command != null) {
        DefaultMutableTreeNode node = new DefaultMutableTreeNode(command.toString());
        this.map.put(node, command);
        undoRoot.add(node);
      }
    }
    for (MapillaryCommand command : redoCommands) {
      if (command != null) {
        DefaultMutableTreeNode node = new DefaultMutableTreeNode(command.toString());
        this.map.put(node, command);
        redoRoot.add(node);
      }
    }

    this.separator.setVisible(!undoCommands.isEmpty() || !redoCommands.isEmpty());
    this.spacer.setVisible(undoCommands.isEmpty() && !redoCommands.isEmpty());

    this.undoTreeModel.setRoot(undoRoot);
    this.redoTreeModel.setRoot(redoRoot);
  }

  @Override
  public void recordChanged() {
    if (!SwingUtilities.isEventDispatchThread()) {
      SwingUtilities.invokeLater(new Runnable() {
        @Override
        public void run() {
          recordChanged();
        }
      });
    } else {
      buildTree();
    }
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
    MapillaryHistoryDialog.instance = null;
  }

  private class MouseEventHandler implements MouseListener {

    @Override
    public void mouseClicked(MouseEvent e) {
    }

    @Override
    public void mouseEntered(MouseEvent e) {
    }

    @Override
    public void mouseExited(MouseEvent e) {
    }

    @Override
    public void mousePressed(MouseEvent e) {
      if (e.getClickCount() == 2) {
        if (MapillaryHistoryDialog.this.undoTree.getSelectionPath() != null) {
          MapillaryCommand cmd = MapillaryHistoryDialog.this.map
              .get(MapillaryHistoryDialog.this.undoTree.getSelectionPath()
                  .getLastPathComponent());
          if (!(cmd instanceof CommandDelete))
            MapillaryUtils.showPictures(cmd.images, true);
        } else
          MapillaryUtils.showPictures(MapillaryHistoryDialog.this.map
              .get(MapillaryHistoryDialog.this.redoTree.getSelectionPath()
                  .getLastPathComponent()).images, true);
      }
    }

    @Override
    public void mouseReleased(MouseEvent e) {
    }
  }

  private class UndoRedoSelectionListener implements TreeSelectionListener {

    private JTree source;

    private UndoRedoSelectionListener(JTree source) {
      this.source = source;
    }

    @Override
    public void valueChanged(TreeSelectionEvent e) {
      if (this.source == MapillaryHistoryDialog.this.undoTree) {
        MapillaryHistoryDialog.this.redoTree.getSelectionModel()
            .removeTreeSelectionListener(
                MapillaryHistoryDialog.this.redoSelectionListener);
        MapillaryHistoryDialog.this.redoTree.clearSelection();
        MapillaryHistoryDialog.this.redoTree.getSelectionModel()
            .addTreeSelectionListener(
                MapillaryHistoryDialog.this.redoSelectionListener);
      }
      if (this.source == MapillaryHistoryDialog.this.redoTree) {
        MapillaryHistoryDialog.this.undoTree.getSelectionModel()
            .removeTreeSelectionListener(
                MapillaryHistoryDialog.this.undoSelectionListener);
        MapillaryHistoryDialog.this.undoTree.clearSelection();
        MapillaryHistoryDialog.this.undoTree.getSelectionModel()
            .addTreeSelectionListener(
                MapillaryHistoryDialog.this.undoSelectionListener);
      }
    }
  }
}
