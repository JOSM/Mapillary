// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.io.download;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.plugins.mapillary.MapillaryData;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryFilterDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryMainDialog;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.PluginState;

public class MapillarySquareDownloadRunnable implements Runnable {

  private final Bounds bounds;

  /**
   * Main constructor.
   *
   * @param bounds the bounds of the area that should be downloaded
   *
   */
  public MapillarySquareDownloadRunnable(Bounds bounds) {
    this.bounds = bounds;
  }

  @Override
  public void run() {
    PluginState.startDownload();
    MapillaryUtils.updateHelpText();

    // Download basic sequence data synchronously
    new SequenceDownloadRunnable(MapillaryLayer.getInstance().getData(), bounds).run();

    // Asynchronously load the rest of the image details
    Thread imgDetailsThread = new Thread(new ImageDetailsDownloadRunnable(MapillaryLayer.getInstance().getData(), bounds));
    imgDetailsThread.start();

    Thread detectionsThread = new Thread(new DetectionsDownloadRunnable(MapillaryLayer.getInstance().getData(), bounds));
    detectionsThread.start();

    try {
      imgDetailsThread.join();
      detectionsThread.join();
    } catch (InterruptedException e) {
      Main.warn(e, "Mapillary download interrupted (probably because of closing the layer).");
    } finally {
      PluginState.finishDownload();
    }

    MapillaryUtils.updateHelpText();
    MapillaryData.dataUpdated();
    MapillaryFilterDialog.getInstance().refresh();
    MapillaryMainDialog.getInstance().updateImage();
  }
}
