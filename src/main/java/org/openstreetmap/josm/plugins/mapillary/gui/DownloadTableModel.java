// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui;

import java.util.ArrayList;
import java.util.Locale;

import javax.swing.JProgressBar;
import javax.swing.table.AbstractTableModel;

import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.io.download.MapillaryDownloader;
import org.openstreetmap.josm.plugins.mapillary.io.download.MapillarySquareDownloadRunnable;
import org.openstreetmap.josm.tools.Utils;

/**
 * TableModel to store list of currently active downloads and display them in a table.
 *
 * @author Kishan
 */
public class DownloadTableModel extends AbstractTableModel {

  private final String[] columnNames = { "Info", "Progress", "Size", "Completed", "Status" };
  private final Class[] columnClasses = { String.class, JProgressBar.class, String.class, String.class, String.class };

  /**
   * The table's list of current downloads.
   */
  private final ArrayList<MapillarySquareDownloadRunnable> downloadList = new ArrayList<>();

  /**
   * The unique instance of the class.
   */
  private static DownloadTableModel instance;

  /**
   * Constructs a new {@code DownloadTableModel}.
   *
   * @param listSelectionModel selection model
   */
  private DownloadTableModel() {
    // Private to avoid multiple instances.
  }

  public static synchronized DownloadTableModel getInstance() {
    if (DownloadTableModel.instance == null) {
      DownloadTableModel.instance = new DownloadTableModel();
    }
    return DownloadTableModel.instance;
  }

  /**
   * Method to reset download list after downloader has been stopped.
   */
  public void reset() {
    if (MapillaryLayer.hasInstance()) {
      downloadList.forEach(download -> {
        MapillaryDownloader.removeHash(download);
      });
    }
    downloadList.clear();
  }

  @Override
  public int getColumnCount() {
    return columnNames.length;
  }

  /**
   * Add a new Download to the list
   *
   * @param download
   */
  public void addDownload(MapillarySquareDownloadRunnable download) {
    downloadList.add(download);
    fireTableRowsInserted(getRowCount() - 1, getRowCount() - 1);
  }

  /**
   * Remove existing download from the list.
   */
  public synchronized void clearDownload(MapillarySquareDownloadRunnable download) {
    int index = getDownloadRow(download);
    if (index >= 0 && index < downloadList.size()) {
      downloadList.remove(index);
      this.fireTableRowsDeleted(index, index);
    }
  }

  @Override
  public int getRowCount() {
    return downloadList.size();
  }

  public int getDownloadRow(MapillarySquareDownloadRunnable download) {
    return downloadList.indexOf(download);
  }

  public MapillarySquareDownloadRunnable getDownload(int row) {
    if (row >= 0) {
      return downloadList.get(row);
    }
    return null;
  }

  @Override
  public String getColumnName(int col) {
    return columnNames[col];
  }

  /**
   * Get a column's class.
   */
  @Override
  public Class getColumnClass(int col) {
    return columnClasses[col];
  }

  @Override
  public Object getValueAt(int rowIndex, int columnIndex) {
    MapillarySquareDownloadRunnable download = downloadList.get(rowIndex);
    switch (columnIndex) {
      case 0: //Info
        return download.getMonitor().title;
      case 1: //ProgressBar
        return download.getMonitor().currentProgressValue;
      case 2: //Size
        return Utils.getSizeString(download.getMonitor().size, Locale.getDefault());
      case 3: //Completed
        return download.completeCount() + "/3";
      case 4: //Status
        return download.state.name();
    }
    return "";
  }
}
