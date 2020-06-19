// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui;

import java.awt.Component;
import org.openstreetmap.josm.plugins.mapillary.gui.dialog.MapillaryDownloadDialog;
import org.openstreetmap.josm.gui.progress.AbstractProgressMonitor;
import org.openstreetmap.josm.gui.progress.CancelHandler;
import org.openstreetmap.josm.gui.progress.ProgressTaskId;
import org.openstreetmap.josm.plugins.mapillary.io.download.MapillarySquareDownloadRunnable;

/**
 * Progress Monitor to be used with {@link MapillarySquareDownloadRunnable
 * @author Kishan
 */
public class DownloadProgressMonitor extends AbstractProgressMonitor {

  private final MapillarySquareDownloadRunnable download;
  public String title;
  private static final int PROGRESS_BAR_MAX = 100;
  public int currentProgressValue;
  public long size;
  public final DownloadTableModel model;

  public DownloadProgressMonitor(MapillarySquareDownloadRunnable download) {
    super(new CancelHandler());
    this.download = download;
    model = DownloadTableModel.getInstance();
  }

  /**
   * Set size according to currently running {@link BoundsDownloadRunnable}
   *
   * @param newSize Size of running download
   */
  public void setSize(long newSize) {
    size = newSize;
    model.fireTableCellUpdated(model.getDownloadRow(download), model.findColumn("Size"));
  }

  /**
   * Add this monitor's download to TableModel.
   * Should be called exactly once for each download.
   */
  @Override
  public void doBeginTask() {
    DownloadTableModel.getInstance().addDownload(download);
  }

  /**
   * Remove this monitor's download to TableModel.
   * Should be called exactly once for each download
   */
  @Override
  public void doFinishTask() {
    DownloadTableModel.getInstance().clearDownload(download);
  }

  @Override
  protected void doSetIntermediate(boolean bln) {
    //Do Nothing
  }

  @Override
  protected void doSetTitle(String string) {
    title = string;
  }

  @Override
  protected void doSetCustomText(String string) {
    //Do nothing.
  }

  @Override
  protected void updateProgress(double progressValue) {
    final int newValue = (int) (progressValue * PROGRESS_BAR_MAX);
    if (newValue != currentProgressValue) {
      currentProgressValue = newValue;
    }
    model.fireTableCellUpdated(model.getDownloadRow(download), model.findColumn("Progress"));
  }

  @Override
  public void setProgressTaskId(ProgressTaskId pti) {
    //Do nothing.
  }

  @Override
  public ProgressTaskId getProgressTaskId() {
    return null;
  }

  @Override
  public Component getWindowParent() {
    return MapillaryDownloadDialog.getInstance();
  }

  /**
   * Update no. of completed parts.
   */
  public void updateCompleted() {
    model.fireTableCellUpdated(model.getDownloadRow(download), model.findColumn("Completed"));
  }
}
