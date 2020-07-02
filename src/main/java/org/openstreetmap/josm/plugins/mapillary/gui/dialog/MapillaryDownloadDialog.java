// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.dialog;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import javax.swing.AbstractAction;
import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.SwingUtilities;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.table.TableCellRenderer;
import org.openstreetmap.josm.gui.MainApplication;

import org.openstreetmap.josm.gui.dialogs.ToggleDialog;
import org.openstreetmap.josm.plugins.mapillary.actions.MapillarySubmitChangesetAction;
import org.openstreetmap.josm.plugins.mapillary.gui.DownloadTableModel;
import org.openstreetmap.josm.plugins.mapillary.gui.boilerplate.MapillaryButton;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.io.download.MapillaryDownloader;
import org.openstreetmap.josm.plugins.mapillary.io.download.MapillarySquareDownloadRunnable;
import org.openstreetmap.josm.plugins.mapillary.io.download.MapillarySquareDownloadRunnable.STATE;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryChangesetListener;
import org.openstreetmap.josm.plugins.mapillary.utils.PluginState;
import org.openstreetmap.josm.tools.GBC;
import org.openstreetmap.josm.tools.I18n;
import static org.openstreetmap.josm.tools.I18n.tr;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.Shortcut;

/**
 * Dialog for viewing and managing downloads.
 *
 * @author Kishan
 */
public class MapillaryDownloadDialog extends ToggleDialog implements TableModelListener, MapillaryChangesetListener {

  private static MapillaryDownloadDialog instance;
  private final JTable table;
  private boolean destroyed;
  private final DownloadTableModel model;
  private final JLabel downloadLabel;
  private final JLabel changesetLabel;
  private final JProgressBar uploadProgress = new JProgressBar();
  public boolean isUploadPending;
  private final MapillaryButton submitButton;
  private final MapillaryButton resumeButton;
  private final MapillaryButton pauseButton;
  private final MapillaryButton cancelButton;
  private final MapillaryButton clearButton;
  private final MapillaryButton stopButton;
  private MapillarySquareDownloadRunnable selectedDownload;

  /**
   * Constructs a new {@code MapillaryDownloadDialog}.
   */
  private MapillaryDownloadDialog() {
    super(
      I18n.tr("Mapillary Download Dialog"), "mapillary-logo", I18n.tr("Download manager for Mapillary Images"),
      Shortcut.registerShortcut("mapillary:expertfilter", tr("Toggle: {0}", tr("Filter")), KeyEvent.CHAR_UNDEFINED,
        Shortcut.NONE), 300
    );

    this.model = DownloadTableModel.getInstance();
    DownloadTableModel.getInstance().addTableModelListener(this);
    table = new JTable();
    table.setModel(model);
    table.setDefaultRenderer(JProgressBar.class, new ProgressCellRender());
    table.setDefaultRenderer(JLabel.class, new StringRender());
    table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    table.getSelectionModel().addListSelectionListener((ListSelectionEvent e) -> {
      updateButtons();
    });
    table.getColumnModel().getColumn(0).setMaxWidth(180);
    table.getColumnModel().getColumn(0).setPreferredWidth(180);
    table.getColumnModel().getColumn(1).setMaxWidth(125);
    table.getColumnModel().getColumn(1).setPreferredWidth(125);
    table.getColumnModel().getColumn(2).setMaxWidth(60);
    table.getColumnModel().getColumn(2).setPreferredWidth(60);
    table.getColumnModel().getColumn(2).setResizable(false);
    table.getColumnModel().getColumn(3).setMaxWidth(80);
    table.getColumnModel().getColumn(3).setPreferredWidth(80);
    table.getColumnModel().getColumn(3).setResizable(false);

    downloadLabel = new JLabel();
    downloadLabel.setText(I18n.tr("{0} Downloads in queue", MapillaryDownloader.getQueuedSize()));
    changesetLabel = new JLabel();
    changesetLabel.setText(I18n.tr("{0} Images changed", MapillaryLayer.hasInstance() ? MapillaryLayer.getInstance().getLocationChangeset().size() : 0));
    isUploadPending = Boolean.FALSE;
    uploadProgress.setIndeterminate(true);
    uploadProgress.setString(tr("Submitting changeset to server…"));
    uploadProgress.setStringPainted(true);
    uploadProgress.setVisible(isUploadPending);

    submitButton = new MapillaryButton(new MapillarySubmitChangesetAction(), true);
    clearButton = new MapillaryButton(new ClearAction(), true);
    resumeButton = new MapillaryButton(new ResumeAction(), true);
    pauseButton = new MapillaryButton(new PauseAction(), true);
    cancelButton = new MapillaryButton(new CancelAction(), true);
    stopButton = new MapillaryButton(new CancelAllAction(), true);

    JPanel root = new JPanel(new GridBagLayout());

    JPanel changesetPanel = new JPanel();
    changesetPanel.setLayout(new FlowLayout(FlowLayout.LEFT));
    changesetPanel.add(changesetLabel);
    changesetPanel.add(submitButton);
    changesetPanel.add(uploadProgress);

    JPanel downloadInfoPanel = new JPanel();
    downloadInfoPanel.add(downloadLabel);
    downloadInfoPanel.add(clearButton);
    downloadInfoPanel.add(stopButton);

    JPanel tablePanel = new JPanel(new BorderLayout());
    tablePanel.add(new JScrollPane(table), BorderLayout.CENTER);
    tablePanel.setBorder(BorderFactory.createTitledBorder("Downloads"));

    JPanel downloadButtons = new JPanel();
    downloadButtons.setLayout(new FlowLayout(FlowLayout.LEFT));
    downloadButtons.add(resumeButton);
    downloadButtons.add(pauseButton);
    downloadButtons.add(cancelButton);

    root.add(changesetPanel, GBC.eol().anchor(GridBagConstraints.LINE_START));
    root.add(downloadInfoPanel, GBC.eol().anchor(GridBagConstraints.LINE_START));
    root.add(downloadButtons, GBC.eol().anchor(GridBagConstraints.LINE_START));
    root.add(tablePanel, GBC.eol().fill());
    createLayout(root, false, null);
    updateDownloadInfo();
    updateChangeset();
    updateButtons();
  }

  /**
   * Destroys the unique instance of the class.
   */
  public static synchronized void destroyInstance() {
    instance = null;
  }

  /**
   * Returns the unique instance of the class.
   *
   * @return The unique instance of the class.
   */
  public static synchronized MapillaryDownloadDialog getInstance() {
    if (instance == null) {
      instance = new MapillaryDownloadDialog();
    }
    return instance;
  }

  @Override
  public void tableChanged(TableModelEvent e) {
    if (!SwingUtilities.isEventDispatchThread()) {
      SwingUtilities.invokeLater(() -> {
        table.repaint();
        if (e.getColumn() == model.findColumn("Status") || e.getColumn() == -1) {
          updateButtons();
        }
      });
    } else {
      table.repaint();
      if (e.getColumn() == model.findColumn("Status") || e.getColumn() == -1) {
        updateButtons();
      }
    }
  }

  @Override
  public void changesetChanged() {
    if (!SwingUtilities.isEventDispatchThread()) {
      SwingUtilities.invokeLater(this::updateChangeset);
    } else {
      updateChangeset();
    }
  }

  public void downloadInfoChanged() {
    if (!SwingUtilities.isEventDispatchThread()) {
      SwingUtilities.invokeLater(this::updateDownloadInfo);
    } else {
      this.updateDownloadInfo();
    }
  }

  private void updateChangeset() {
    changesetLabel.setText(I18n.tr("{0} Images changed", MapillaryLayer.hasInstance() ? MapillaryLayer.getInstance().getLocationChangeset().size() : 0));
    submitButton.setEnabled(!isUploadPending && MapillaryLayer.hasInstance() && !MapillaryLayer.getInstance().getLocationChangeset().isEmpty());
    uploadProgress.setVisible(isUploadPending);
  }

  private void updateDownloadInfo() {
    downloadLabel.setText(I18n.tr("{0} Downloads in queue", MapillaryDownloader.getQueuedSize()));
    updateButtons();
  }

  private void updateButtons() {
    selectedDownload = model.getDownload(table.getSelectedRow());
    if (selectedDownload == null) {
      resumeButton.setEnabled(false);
      pauseButton.setEnabled(false);
      cancelButton.setEnabled(false);
    } else {
      STATE state = selectedDownload.state;
      resumeButton.setEnabled(selectedDownload.isResumable());
      pauseButton.setEnabled(selectedDownload.isPausable());
      cancelButton.setEnabled(state != STATE.STOPPED);
    }
    clearButton.setEnabled(MapillaryDownloader.getQueuedSize() != 0);
    stopButton.setEnabled(PluginState.isDownloading());
  }

  public void setUploadPending(final boolean isUploadPending) {
    this.isUploadPending = isUploadPending;
    updateChangeset();
  }

  public static class ProgressCellRender extends JProgressBar implements TableCellRenderer {

    @Override
    public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
      int progress = 0;
      if (value instanceof Float) {
        progress = Math.round(((Float) value) * 100f);
      } else if (value instanceof Integer) {
        progress = (int) value;
      }
      setValue(progress);
      return this;
    }
  }

  public static class StringRender extends JLabel implements TableCellRenderer {

    @Override
    public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
      setText(I18n.tr(value.toString()));
      return this;
    }
  }

  private static class ClearAction extends AbstractAction {

    ClearAction() {
      putValue(NAME, "Clear queue");
      putValue(SHORT_DESCRIPTION, tr("Clear queued downloads"));
    }

    @Override
    public void actionPerformed(ActionEvent e) {
      MapillaryDownloader.removeQueued();
      MapillaryDownloadDialog.getInstance().updateDownloadInfo();
      MapillaryDownloadDialog.getInstance().updateButtons();
    }
  }

  private static class CancelAllAction extends AbstractAction {

    CancelAllAction() {
      super("Stop All", ImageProvider.get("dialogs", "mapillaryStop.png", ImageProvider.ImageSizes.SMALLICON));
      putValue(SHORT_DESCRIPTION, tr("Cancel all downloads"));
    }

    @Override
    public void actionPerformed(ActionEvent e) {
      MapillaryDownloader.stopAll();
      MapillaryDownloadDialog.getInstance().updateDownloadInfo();
      MapillaryDownloadDialog.getInstance().updateButtons();
    }
  }

  private static class ResumeAction extends AbstractAction {

    ResumeAction() {
      super("Resume", ImageProvider.get("dialogs", "mapillaryPlay.png", ImageProvider.ImageSizes.SMALLICON));
      putValue(SHORT_DESCRIPTION, tr("Resume selected download"));
    }

    @Override
    public void actionPerformed(ActionEvent e) {
      MapillaryDownloadDialog.getInstance().selectedDownload.resume();
      MapillaryDownloadDialog.getInstance().updateButtons();
      MapillaryDownloadDialog.getInstance().updateDownloadInfo();
    }
  }

  private static class PauseAction extends AbstractAction {

    PauseAction() {
      super("Pause", ImageProvider.get("dialogs", "mapillaryPause.png", ImageProvider.ImageSizes.SMALLICON));
      putValue(SHORT_DESCRIPTION, tr("Pause selected download"));
    }

    @Override
    public void actionPerformed(ActionEvent e) {
      MapillaryDownloadDialog.getInstance().selectedDownload.pause();
      MapillaryDownloadDialog.getInstance().updateButtons();
      MapillaryDownloadDialog.getInstance().updateDownloadInfo();
    }
  }

  private static class CancelAction extends AbstractAction {

    CancelAction() {
      super("Cancel");
      putValue(SHORT_DESCRIPTION, tr("Cancel selected download"));
    }

    @Override
    public void actionPerformed(ActionEvent e) {
      MapillaryDownloadDialog instance = MapillaryDownloadDialog.getInstance();
      instance.selectedDownload.cancel();
      instance.updateButtons();
      instance.updateDownloadInfo();
    }
  }

  @Override
  public void destroy() {
    if (!destroyed) {
      super.destroy();
      MainApplication.getMap().removeToggleDialog(this);
      destroyed = true;
      this.invalidate();
    }
    destroyInstance();
  }
}
