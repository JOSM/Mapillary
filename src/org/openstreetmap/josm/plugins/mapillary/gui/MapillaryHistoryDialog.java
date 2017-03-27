// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
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

/**
 * Toggle dialog that shows you the latest {@link MapillaryCommand} done and
 * allows the user to revert them.
 *
 * @author nokutu
 * @see MapillaryRecord
 * @see MapillaryCommand
 *
 */
public final class MapillaryHistoryDialog extends ToggleDialog implements MapillaryRecordListener {

  private static final long serialVersionUID = -3019715241209349372L;

  private static MapillaryHistoryDialog instance;

  private final transient UndoRedoSelectionListener undoSelectionListener;
  private final transient UndoRedoSelectionListener redoSelectionListener;

  private final DefaultTreeModel undoTreeModel = new DefaultTreeModel(new DefaultMutableTreeNode());
  private final DefaultTreeModel redoTreeModel = new DefaultTreeModel(new DefaultMutableTreeNode());
  private final JTree undoTree = new JTree(this.undoTreeModel);
  private final JTree redoTree = new JTree(this.redoTreeModel);

  private final JSeparator separator = new JSeparator();
  private final Component spacer = Box.createRigidArea(new Dimension(0, 3));

  private final SideButton undoButton;
  private final SideButton redoButton;

  private final ConcurrentHashMap<Object, MapillaryCommand> map;

  private MapillaryHistoryDialog() {
    super(tr("Mapillary history"), "mapillary-history", tr("Open Mapillary history dialog"), null, 200,
        false, MapillaryPreferenceSetting.class);

    MapillaryRecord.getInstance().addListener(this);

    this.map = new ConcurrentHashMap<>();

    this.undoTree.expandRow(0);
    this.undoTree.setShowsRootHandles(true);
    this.undoTree.setRootVisible(false);
    this.undoTree.setCellRenderer(new MapillaryImageTreeCellRenderer());
    this.undoTree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
    this.undoTree.addMouseListener(new MouseEventHandler());
    this.undoSelectionListener = new UndoRedoSelectionListener(this.undoTree);
    this.undoTree.getSelectionModel().addTreeSelectionListener(this.undoSelectionListener);

    this.redoTree.expandRow(0);
    this.redoTree.setCellRenderer(new MapillaryImageTreeCellRenderer());
    this.redoTree.setShowsRootHandles(true);
    this.redoTree.setRootVisible(false);
    this.redoTree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
    this.redoTree.addMouseListener(new MouseEventHandler());
    this.redoSelectionListener = new UndoRedoSelectionListener(this.redoTree);
    this.redoTree.getSelectionModel().addTreeSelectionListener(this.redoSelectionListener);

    JPanel treesPanel = new JPanel(new GridBagLayout());
    treesPanel.add(this.spacer, GBC.eol());
    this.spacer.setVisible(false);
    treesPanel.add(this.undoTree, GBC.eol().fill(GridBagConstraints.HORIZONTAL));
    this.separator.setVisible(false);
    treesPanel.add(this.separator, GBC.eol().fill(GridBagConstraints.HORIZONTAL));
    treesPanel.add(this.redoTree, GBC.eol().fill(GridBagConstraints.HORIZONTAL));
    treesPanel.add(Box.createRigidArea(new Dimension(0, 0)), GBC.std().weight(0, 1));
    treesPanel.setBackground(this.redoTree.getBackground());

    this.undoButton = new SideButton(new UndoAction());
    this.redoButton = new SideButton(new RedoAction());

    createLayout(treesPanel, true, Arrays.asList(this.undoButton, this.redoButton));
  }

  private void buildTree() {
    this.redoButton.setEnabled(true);
    this.undoButton.setEnabled(true);
    List<MapillaryCommand> commands = MapillaryRecord.getInstance().commandList;
    int position = MapillaryRecord.getInstance().position;
    ArrayList<MapillaryCommand> undoCommands = new ArrayList<>();
    if (position >= 0) {
      undoCommands = new ArrayList<>(commands.subList(0, position + 1));
    } else {
      this.undoButton.setEnabled(false);
    }
    ArrayList<MapillaryCommand> redoCommands = new ArrayList<>();
    if (!commands.isEmpty() && position + 1 < commands.size()) {
      redoCommands = new ArrayList<>(commands.subList(position + 1, commands.size()));
    } else {
      this.redoButton.setEnabled(false);
    }

    DefaultMutableTreeNode redoRoot = new DefaultMutableTreeNode();
    DefaultMutableTreeNode undoRoot = new DefaultMutableTreeNode();

    this.map.clear();
    undoCommands.stream().filter(Objects::nonNull).forEach(command -> {
      DefaultMutableTreeNode node = new DefaultMutableTreeNode(command.toString());
      this.map.put(node, command);
      undoRoot.add(node);
    });
    redoCommands.stream().filter(Objects::nonNull).forEach(command -> {
      DefaultMutableTreeNode node = new DefaultMutableTreeNode(command.toString());
      this.map.put(node, command);
      redoRoot.add(node);
    });

    this.separator.setVisible(!undoCommands.isEmpty() || !redoCommands.isEmpty());
    this.spacer.setVisible(undoCommands.isEmpty() && !redoCommands.isEmpty());

    this.undoTreeModel.setRoot(undoRoot);
    this.redoTreeModel.setRoot(redoRoot);
  }

  /**
   * Destroys the unique instance of the class.
   */
  public static void destroyInstance() {
    MapillaryHistoryDialog.instance = null;
  }

  /**
   * Returns the unique instance of the class.
   *
   * @return The unique instance of the class.
   */
  public static synchronized MapillaryHistoryDialog getInstance() {
    if (instance == null)
      instance = new MapillaryHistoryDialog();
    return instance;
  }

  MapillaryCommand getCommandFromMap(Object node) {
    return map.get(node);
  }

  UndoRedoSelectionListener getRedoSelectionListener() {
    return redoSelectionListener;
  }

  JTree getRedoTree() {
    return redoTree;
  }

  UndoRedoSelectionListener getUndoSelectionListener() {
    return undoSelectionListener;
  }

  JTree getUndoTree() {
    return undoTree;
  }

  @Override
  public void recordChanged() {
    if (!SwingUtilities.isEventDispatchThread()) {
      SwingUtilities.invokeLater(this::recordChanged);
    } else {
      buildTree();
    }
  }

  private static class UndoAction extends AbstractAction {

    private static final long serialVersionUID = -6435832206342007269L;

    UndoAction() {
      super(tr("Undo"));
      new ImageProvider("undo").getResource().attachImageIcon(this, true);
    }

    @Override
    public void actionPerformed(ActionEvent arg0) {
      MapillaryRecord.getInstance().undo();
    }
  }

  private static class RedoAction extends AbstractAction {

    private static final long serialVersionUID = -2761935780353053512L;

    RedoAction() {
      super(tr("Redo"));
      new ImageProvider("redo").getResource().attachImageIcon(this, true);
    }

    @Override
    public void actionPerformed(ActionEvent arg0) {
      MapillaryRecord.getInstance().redo();
    }
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
        if (getUndoTree().getSelectionPath() == null) {
          MapillaryUtils.showPictures(getCommandFromMap(getRedoTree().getSelectionPath().getLastPathComponent()).images, true);
        } else {
          MapillaryCommand cmd = getCommandFromMap(getUndoTree().getSelectionPath().getLastPathComponent());
          if (!(cmd instanceof CommandDelete)) {
            MapillaryUtils.showPictures(cmd.images, true);
          }
        }
      }
    }

    @Override
    public void mouseReleased(MouseEvent e) {
      // Method is enforced by MouseListener, but (currently) not needed
    }
  }

  private class UndoRedoSelectionListener implements TreeSelectionListener {

    private final JTree source;

    protected UndoRedoSelectionListener(JTree source) {
      this.source = source;
    }

    @Override
    public void valueChanged(TreeSelectionEvent e) {
      if (this.source == getUndoTree()) {
        getRedoTree().getSelectionModel().removeTreeSelectionListener(getRedoSelectionListener());
        getRedoTree().clearSelection();
        getRedoTree().getSelectionModel().addTreeSelectionListener(getRedoSelectionListener());
      }
      if (this.source == getRedoTree()) {
        getUndoTree().getSelectionModel().removeTreeSelectionListener(getUndoSelectionListener());
        getUndoTree().clearSelection();
        getUndoTree().getSelectionModel().addTreeSelectionListener(getUndoSelectionListener());
      }
    }
  }
}
