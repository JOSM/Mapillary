// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.io.download;

import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.data.DataSource;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.MapView;
import org.openstreetmap.josm.gui.Notification;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.tools.I18n;
import org.openstreetmap.josm.tools.Logging;

/**
 * Class that concentrates all the ways of downloading of the plugin. All the
 * download petitions will be managed one by one.
 *
 * @author nokutu
 */
public final class MapillaryDownloader {
  /** Possible public/private download options */
  public enum PRIVATE_IMAGE_DOWNLOAD_MODE {
    PUBLIC_ONLY("publicOnly", I18n.marktr("Only download public images")),
    PRIVATE_ONLY("privateOnly", I18n.marktr("Only download private images")),
    ALL("all", I18n.marktr("Download all available images"));

    public final static PRIVATE_IMAGE_DOWNLOAD_MODE DEFAULT = ALL;

    private final String prefId;
    private final String label;

    PRIVATE_IMAGE_DOWNLOAD_MODE(String prefId, String label) {
      this.prefId = prefId;
      this.label = label;
    }

    @Override
    public String toString() {
      return I18n.tr(label);
    }

    public String getPrefId() {
      return prefId;
    }

    public static PRIVATE_IMAGE_DOWNLOAD_MODE getFromId(String prefId) {
      for (PRIVATE_IMAGE_DOWNLOAD_MODE val : PRIVATE_IMAGE_DOWNLOAD_MODE.values()) {
        if (val.prefId.equals(prefId))
          return val;
      }
      return ALL;
    }
  }

  /** Possible download modes. */
  public enum DOWNLOAD_MODE {
    // i18n: download mode for Mapillary images
    VISIBLE_AREA("visibleArea", I18n.tr("everything in the visible area")),
    // i18n: download mode for Mapillary images
    OSM_AREA("osmArea", I18n.tr("areas with downloaded OSM-data")),
    // i18n: download mode for Mapillary images
    MANUAL_ONLY("manualOnly", I18n.tr("only when manually requested"));

    public final static DOWNLOAD_MODE DEFAULT = OSM_AREA;

    private final String prefId;
    private final String label;

    DOWNLOAD_MODE(String prefId, String label) {
      this.prefId = prefId;
      this.label = label;
    }

    /**
     * @return the ID that is used to represent this download mode in the JOSM preferences
     */
    public String getPrefId() {
      return prefId;
    }

    /**
     * @return the (internationalized) label describing this download mode
     */
    public String getLabel() {
      return label;
    }

    public static DOWNLOAD_MODE fromPrefId(String prefId) {
      for (DOWNLOAD_MODE mode : DOWNLOAD_MODE.values()) {
        if (mode.getPrefId().equals(prefId)) {
          return mode;
        }
      }
      return DEFAULT;
    }

    public static DOWNLOAD_MODE fromLabel(String label) {
      for (DOWNLOAD_MODE mode : DOWNLOAD_MODE.values()) {
        if (mode.getLabel().equals(label)) {
          return mode;
        }
      }
      return DEFAULT;
    }
  }

  /** Max area to be downloaded */
  private static final double MAX_AREA = MapillaryProperties.MAX_DOWNLOAD_AREA.get();

  /** Executor that will run the petitions. */
  private static ThreadPoolExecutor executor = new ThreadPoolExecutor(
    3, 5, 100, TimeUnit.SECONDS, new ArrayBlockingQueue<>(100), new ThreadPoolExecutor.DiscardPolicy());

  /**
   * Indicates whether the last download request has been rejected because it requested an area that was too big.
   * Iff true, the last download has been rejected, if false, it was executed.
   */
  private static boolean stoppedDownload;

  private MapillaryDownloader() {
    // Private constructor to avoid instantiation
  }

  /**
   * Gets all the images in a square. It downloads all the images of all the
   * sequences that pass through the given rectangle.
   *
   * @param minLatLon The minimum latitude and longitude of the rectangle.
   * @param maxLatLon The maximum latitude and longitude of the rectangle
   */
  public static void getImages(LatLon minLatLon, LatLon maxLatLon) {
    if (minLatLon == null || maxLatLon == null) {
      throw new IllegalArgumentException();
    }
    getImages(new Bounds(minLatLon, maxLatLon));
  }

  /**
   * Gets the images within the given bounds.
   *
   * @param bounds A {@link Bounds} object containing the area to be downloaded.
   */
  public static void getImages(Bounds bounds) {
    run(new MapillarySquareDownloadRunnable(bounds));
  }

  /**
   * Returns the current download mode.
   *
   * @return the currently enabled {@link DOWNLOAD_MODE}
   */
  public static DOWNLOAD_MODE getMode() {
    return DOWNLOAD_MODE.fromPrefId(MapillaryProperties.DOWNLOAD_MODE.get());
  }

  private static void run(Runnable t) {
    executor.execute(t);
  }

  /**
   * If some part of the current view has not been downloaded, it is downloaded.
   */
  public static void downloadVisibleArea() {
    final MapView mv = MapillaryPlugin.getMapView();
    if (mv != null) {
      final Bounds view = mv.getRealBounds();
      if (!isAreaTooBig(view.getArea()) && !isViewDownloaded(view)) {
        MapillaryLayer.getInstance().getData().addDataSource(new DataSource(view, view.toString()));
        getImages(view);
      }
    }
  }

  private static boolean isViewDownloaded(Bounds view) {
    int n = 15;
    for (int i = 0; i < n; i++) {
      for (int j = 0; j < n; j++) {
        if (!isInBounds(new LatLon(view.getMinLat() + (view.getMaxLat() - view.getMinLat()) * ((double) i / n),
          view.getMinLon() + (view.getMaxLon() - view.getMinLon()) * ((double) j / n)))) {
          return false;
        }
      }
    }
    return true;
  }

  /**
   * Checks if the given {@link LatLon} object lies inside the bounds of the
   * image.
   *
   * @param latlon The coordinates to check.
   *
   * @return true if it lies inside the bounds; false otherwise;
   */
  private static boolean isInBounds(LatLon latlon) {
    return MapillaryLayer.getInstance().getData().getBounds().parallelStream().anyMatch(b -> b.contains(latlon));
  }

  /**
   * Downloads all images of the area covered by the OSM data.
   */
  public static void downloadOSMArea() {
    if (MainApplication.getLayerManager().getEditLayer() == null) {
      return;
    }
    if (isAreaTooBig(MainApplication.getLayerManager().getEditLayer().data.getDataSourceBounds().parallelStream().map(Bounds::getArea).reduce(0.0, Double::sum))) {
      return;
    }
    MainApplication.getLayerManager().getEditLayer().data.getDataSourceBounds().stream().
    filter(bounds -> !MapillaryLayer.getInstance().getData().getBounds().contains(bounds)).forEach(bounds -> {
      MapillaryLayer.getInstance().getData().addDataSource(new DataSource(bounds, bounds.toString()));
      MapillaryDownloader.getImages(bounds.getMin(), bounds.getMax());
    });
  }

  /**
   * Checks if the area for which Mapillary images should be downloaded is too big. This means that probably lots of
   * Mapillary images are going to be downloaded, slowing down the program too much. A notification is shown when the
   * download has stopped or continued.
   *
   * @param area The size of the area in LatLon space
   * @return {@code true} if the area is too large
   */
  private static boolean isAreaTooBig(final double area) {
    final boolean tooBig = area > MAX_AREA;
    if (!stoppedDownload && tooBig) {
      new Notification(
        I18n.tr("The Mapillary layer has stopped downloading images, because the requested area is too big!") + (
          getMode() == DOWNLOAD_MODE.VISIBLE_AREA
          ? "\n"+I18n.tr("To solve this problem, you could zoom in and load a smaller area of the map.")
          : (getMode() == DOWNLOAD_MODE.OSM_AREA ? "\n"+I18n.tr("To solve this problem, you could switch to download mode ''{0}'' and load Mapillary images for a smaller portion of the map.", DOWNLOAD_MODE.MANUAL_ONLY): "")
        )
      ).setIcon(MapillaryPlugin.LOGO.get()).setDuration(Notification.TIME_LONG).show();
    }
    if (stoppedDownload && !tooBig) {
      new Notification("The Mapillary layer now continues to download images…").setIcon(MapillaryPlugin.LOGO.get()).show();
    }
    stoppedDownload = tooBig;
    return tooBig;
  }

  /**
   * Stops all running threads.
   */
  public static void stopAll() {
    executor.shutdownNow();
    try {
      executor.awaitTermination(30, TimeUnit.SECONDS);
    } catch (InterruptedException e) {
      Logging.error(e);
    }
    executor = new ThreadPoolExecutor(3, 5, 100, TimeUnit.SECONDS,
      new ArrayBlockingQueue<>(100), new ThreadPoolExecutor.DiscardPolicy());
  }
}
