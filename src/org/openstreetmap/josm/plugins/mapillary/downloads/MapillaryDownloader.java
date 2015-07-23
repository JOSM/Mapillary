package org.openstreetmap.josm.plugins.mapillary.downloads;

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.actions.MapillaryDownloadViewAction;

/**
 * Class that concentrates all the ways of downloading of the plugin. All the
 * download petitions will be managed one by one.
 *
 * @author nokutu
 *
 */
public class MapillaryDownloader {

  /** Possible download modes.*/
  public static final String[] MODES = new String[] { "Automatic", "Semiautomatic",
  "Manual" };

  /** Base URL of the Mapillary API. */
  public final static String BASE_URL = "https://a.mapillary.com/v2/";
  /** Client ID for the app */
  public final static String CLIENT_ID = "NzNRM2otQkR2SHJzaXJmNmdQWVQ0dzo1YTA2NmNlODhlNWMwOTBm";
  /** Executor that will run the petitions */
  public final static Executor EXECUTOR = Executors.newSingleThreadExecutor();

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

    try {
      EXECUTOR.execute(new MapillarySquareDownloadManagerThread(
          queryStringParts, MapillaryLayer.getInstance()));
    } catch (Exception e) {
      Main.error(e);
    }
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
}
