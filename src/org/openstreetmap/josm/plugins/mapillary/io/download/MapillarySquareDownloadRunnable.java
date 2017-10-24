// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.io.download;

import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryFilterDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryMainDialog;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.PluginState;
import org.openstreetmap.josm.tools.Logging;

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

    if (Thread.interrupted()) {
      return;
    }

    // Asynchronously load the rest of the image details
    Thread imgDetailsThread = new Thread(new ImageDetailsDownloadRunnable(MapillaryLayer.getInstance().getData(), bounds));
    imgDetailsThread.start();

    Thread detectionsThread = new Thread(new DetectionsDownloadRunnable(MapillaryLayer.getInstance().getData(), bounds));
    detectionsThread.start();

    try {
      imgDetailsThread.join();
      detectionsThread.join();
    } catch (InterruptedException e) {
      Logging.log(Logging.LEVEL_WARN, "Mapillary download interrupted (probably because of closing the layer).", e);
      Thread.currentThread().interrupt();
    } finally {
      PluginState.finishDownload();
    }

    MapillaryUtils.updateHelpText();
    MapillaryLayer.invalidateInstance();
    MapillaryFilterDialog.getInstance().refresh();
    MapillaryMainDialog.getInstance().updateImage();
  }
}
