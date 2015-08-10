package org.openstreetmap.josm.plugins.mapillary.downloads;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.util.ArrayList;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;

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
  /** Automatic mode. */
  public static final int AUTOMATIC = 0;
  /** Semiautomatic mode. */
  public static final int SEMIAUTOMATIC = 1;
  /** Manual mode. */
  public static final int MANUAL = 2;

  /** All the Threads that have been run. Used to interrupt them properly. */
  private static ArrayList<Thread> threads = new ArrayList<>();

  /** Max area to be downloaded */
  public static final double MAX_AREA = Main.pref.getDouble(
      "mapillary.max-download-area", 0.015);

  /** Base URL of the Mapillary API. */
  public final static String BASE_URL = "https://a.mapillary.com/v2/";
  /** Client ID for the app */
  public final static String CLIENT_ID = "T1Fzd20xZjdtR0s1VDk5OFNIOXpYdzoxNDYyOGRkYzUyYTFiMzgz";
  /** Executor that will run the petitions. */
  private static ThreadPoolExecutor EXECUTOR = new ThreadPoolExecutor(3, 5, 100, TimeUnit.SECONDS,
      new ArrayBlockingQueue<Runnable>(100));;

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
    run(new MapillarySquareDownloadManagerThread(queryStringParts));
  }

  private static void run(Thread t) {
    threads.add(t);
    EXECUTOR.execute(t);
  }

  /**
   * If some part of the current view has not been downloaded, it is downloaded.
   *
   */
  public static void completeView() {
    if (getMode() != SEMIAUTOMATIC && getMode() != MANUAL)
      throw new IllegalStateException("Must be in semiautomatic or manual mode");
    Bounds view = Main.map.mapView.getRealBounds();
    if (view.getArea() > MAX_AREA)
      return;
    if (isViewDownloaded(view))
      return;
    MapillaryLayer.getInstance().getData().bounds.add(view);
    getImages(view);
  }

  private static boolean isViewDownloaded(Bounds view) {
    int n = 15;
    boolean[][] inside = new boolean[n][n];
    for (int i = 0; i < n; i++) {
      for (int j = 0; j < n; j++) {
        if (isInBounds(new LatLon(view.getMinLat()
            + (view.getMaxLat() - view.getMinLat()) * ((double) i / n),
            view.getMinLon() + (view.getMaxLon() - view.getMinLon())
                * ((double) j / n)))) {
          inside[i][j] = true;
        }
      }
    }
    for (int i = 0; i < n; i++) {
      for (int j = 0; j < n; j++) {
        if (!inside[i][j])
          return false;
      }
    }
    return true;
  }

  /**
   * Checks if the given {@LatLon} object lies inside the bounds of the
   * image.
   *
   * @param latlon
   * @return true if it lies inside the bounds; false otherwise;
   */
  private static boolean isInBounds(LatLon latlon) {
    for (Bounds bounds : MapillaryLayer.getInstance().getData().bounds) {
      if (bounds.contains(latlon))
        return true;
    }
    return false;
  }

  /**
   * Gets the images within the given bounds.
   *
   * @param bounds
   *          A {@link Bounds} object containing the area to be downloaded.
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
    if (isAreaTooBig()) {
      tooBigErrorDialog();
      return;
    }

    if (getMode() != AUTOMATIC)
      throw new IllegalStateException("Must be in automatic mode.");
    for (Bounds bounds : Main.map.mapView.getEditLayer().data
        .getDataSourceBounds()) {
      if (!layer.getData().bounds.contains(bounds)) {
        layer.getData().bounds.add(bounds);
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
  private static boolean isAreaTooBig() {
    double area = 0;
    for (Bounds bounds : Main.map.mapView.getEditLayer().data
        .getDataSourceBounds()) {
      area += bounds.getArea();
    }
    if (area > MAX_AREA)
      return true;
    return false;
  }

  /**
   * Returns the current download mode.
   *
   * @return 0 - automatic; 1 - semiautomatic; 2 - manual.
   */
  public static int getMode() {
    if (Main.pref.get("mapillary.download-mode").equals(MODES[0])
        && (MapillaryLayer.INSTANCE == null || !MapillaryLayer.INSTANCE.TEMP_SEMIAUTOMATIC))
      return 0;
    else if (Main.pref.get("mapillary.download-mode").equals(MODES[1])
        || (MapillaryLayer.INSTANCE != null && MapillaryLayer.getInstance().TEMP_SEMIAUTOMATIC))
      return 1;
    else if (Main.pref.get("mapillary.download-mode").equals(MODES[2]))
      return 2;
    else if (Main.pref.get("mapillary.download-mode").equals(""))
      return 0;
    else
      throw new IllegalStateException();
  }

  private static void tooBigErrorDialog() {
    if (!SwingUtilities.isEventDispatchThread()) {
      SwingUtilities.invokeLater(new Runnable() {
        @Override
        public void run() {
          tooBigErrorDialog();
        }
      });
    } else {
      MapillaryLayer.getInstance().TEMP_SEMIAUTOMATIC = true;
      MapillaryPlugin.setMenuEnabled(MapillaryPlugin.DOWNLOAD_VIEW_MENU, true);
      JOptionPane
          .showMessageDialog(
              Main.parent,
              tr("The downloaded OSM area is too big. Download mode has been changed to semiautomatic until the layer is restarted."));
    }
  }

  /**
   * Stops all running threads.
   */
  public static void stopAll() {
    for (Thread t : threads) {
      if (t.isAlive())
        System.out.println(t);
      t.interrupt();
    }
    threads.clear();
    EXECUTOR.shutdownNow();
    try {
      EXECUTOR.awaitTermination(30, TimeUnit.SECONDS);
    } catch (InterruptedException e) {
      Main.error(e);
    }
    EXECUTOR = new ThreadPoolExecutor(3, 5, 100, TimeUnit.SECONDS,
        new ArrayBlockingQueue<Runnable>(100));
  }
}
