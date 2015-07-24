package org.openstreetmap.josm.plugins.mapillary.downloads;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import javax.swing.JOptionPane;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.actions.MapillaryDownloadViewAction;

/**
 * Class that concentrates all the ways of downloading of the plugin. All the
 * download petitions will be managed one by one.
 *
 * @author nokutu
 *
 */
public class MapillaryDownloader {

  /** Possible download modes. */
  public static final String[] MODES = new String[] { "Automatic",
      "Semiautomatic", "Manual" };

  /** Base URL of the Mapillary API. */
  public final static String BASE_URL = "https://a.mapillary.com/v2/";
  /** Client ID for the app */
  public final static String CLIENT_ID = "NzNRM2otQkR2SHJzaXJmNmdQWVQ0dzo1YTA2NmNlODhlNWMwOTBm";
  /** Executor that will run the petitions */
  public final static ThreadPoolExecutor EXECUTOR = new ThreadPoolExecutor(1,
      1, 100, TimeUnit.SECONDS, new ArrayBlockingQueue<Runnable>(50));

  /**
   * Gets all the images in a square. It downloads all the images of all the
   * sequences that pass through the given rectangle.
   *
   * @param minLatLon
   *          The minimum latitude and longitude of the rectangle.
   * @param maxLatLon
   *          The maximum latitude and longitude of the rectangle
   */
  public static void getImages(LatLon minLatLon, LatLon maxLatLon) {
    ConcurrentHashMap<String, Double> queryStringParts = new ConcurrentHashMap<>();
    queryStringParts.put("min_lat", minLatLon.lat());
    queryStringParts.put("min_lon", minLatLon.lon());
    queryStringParts.put("max_lat", maxLatLon.lat());
    queryStringParts.put("max_lon", maxLatLon.lon());
    run(new MapillarySquareDownloadManagerThread(queryStringParts,
        MapillaryLayer.getInstance()));
  }

  private static void run(Thread t) {
    EXECUTOR.execute(t);
  }

  /**
   * If some part of the current view has not been downloaded, it is downloaded.
   *
   */
  public static void completeView() {
    Bounds view = Main.map.mapView.getRealBounds();
    if (view.getArea() > MapillaryDownloadViewAction.MAX_AREA)
      return;
    for (Bounds bound : MapillaryLayer.getInstance().bounds) {
      if (!view.intersects(bound))
        continue;
      if (bound.equals(view)) {
        // Already downloaded
        return;
      }
    }
    MapillaryLayer.getInstance().bounds.add(view);
    getImages(view);
  }

  /**
   * Gets the images within the given bounds.
   *
   * @param bounds
   */
  public static void getImages(Bounds bounds) {
    getImages(bounds.getMin(), bounds.getMax());
  }

  /**
   * Downloads all images of the area covered by the OSM data. This is only just
   * for automatic download.
   */
  public static void automaticDownload() {
    MapillaryLayer layer = MapillaryLayer.getInstance();
    checkAreaTooBig();
    if (!Main.pref.get("mapillary.download-mode").equals(
        MapillaryDownloader.MODES[0])
        || layer.TEMP_SEMIAUTOMATIC)
      return;
    for (Bounds bounds : Main.map.mapView.getEditLayer().data
        .getDataSourceBounds()) {
      if (!layer.bounds.contains(bounds)) {
        layer.bounds.add(bounds);
        MapillaryDownloader.getImages(bounds.getMin(), bounds.getMax());
      }
    }
  }

  /**
   * Checks if the area of the OSM data is too big. This means that probably
   * lots of Mapillary images are going to be downloaded, slowing down the
   * program too much. To solve this the automatic is stopped, an alert is shown
   * and you will have to download areas manually.
   */
  private static void checkAreaTooBig() {
    double area = 0;
    for (Bounds bounds : Main.map.mapView.getEditLayer().data
        .getDataSourceBounds()) {
      area += bounds.getArea();
    }
    if (area > MapillaryDownloadViewAction.MAX_AREA) {
      MapillaryLayer.getInstance().TEMP_SEMIAUTOMATIC = true;
      MapillaryPlugin.setMenuEnabled(MapillaryPlugin.DOWNLOAD_VIEW_MENU, true);
      JOptionPane
          .showMessageDialog(
              Main.parent,
              tr("The downloaded OSM area is too big. Download mode has been changed to manual until the layer is restarted."));
    }
  }

  /**
   * Stops all running threads.
   */
  public static void stopAll() {
    EXECUTOR.shutdownNow();
  }
}
