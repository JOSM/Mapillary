// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.io.download;

import java.io.IOException;
import java.net.URL;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collector;

import javax.json.Json;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;
import javax.json.JsonReader;
import javax.json.JsonValue;

import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.data.DataSource;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.MapView;
import org.openstreetmap.josm.gui.Notification;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.data.image.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.data.image.MapillarySequence;
import org.openstreetmap.josm.plugins.mapillary.gui.DownloadTableModel;
import org.openstreetmap.josm.plugins.mapillary.gui.dialog.MapillaryDownloadDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.oauth.OAuthUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.api.JsonDecoder;
import org.openstreetmap.josm.plugins.mapillary.utils.api.JsonImageDetailsDecoder;
import org.openstreetmap.josm.plugins.mapillary.utils.api.JsonSequencesDecoder;
import org.openstreetmap.josm.tools.HttpClient;
import org.openstreetmap.josm.tools.HttpClient.Response;
import org.openstreetmap.josm.tools.I18n;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.Logging;

/**
 * Class that concentrates all the ways of downloading of the plugin. All the
 * download petitions will be managed one by one.
 *
 * @author nokutu
 */
public final class MapillaryDownloader {

  /**
   * Possible public/private download options
   */
  public enum PRIVATE_IMAGE_DOWNLOAD_MODE {
    PUBLIC_ONLY("publicOnly", I18n.marktr("Only download public images")),
    PRIVATE_ONLY("privateOnly", I18n.marktr("Only download private images")),
    ALL("all", I18n.marktr("Download all available images"));

    public static final PRIVATE_IMAGE_DOWNLOAD_MODE DEFAULT = ALL;

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
        if (val.prefId.equals(prefId)) {
          return val;
        }
      }
      return ALL;
    }
  }

  /**
   * Possible download modes.
   */
  public enum DOWNLOAD_MODE {
    // i18n: download mode for Mapillary images
    VISIBLE_AREA("visibleArea", I18n.tr("everything in the visible area")),
    // i18n: download mode for Mapillary images
    OSM_AREA("osmArea", I18n.tr("areas with downloaded OSM-data")),
    // i18n: download mode for Mapillary images
    MANUAL_ONLY("manualOnly", I18n.tr("only when manually requested"));

    public static final DOWNLOAD_MODE DEFAULT = OSM_AREA;

    private final String prefId;
    private final String label;

    DOWNLOAD_MODE(String prefId, String label) {
      this.prefId = prefId;
      this.label = label;
    }

    /**
     * @return the ID that is used to represent this download mode in the JOSM
     *         preferences
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

  /**
   * Max area to be downloaded
   */
  private static final double MAX_AREA = MapillaryProperties.MAX_DOWNLOAD_AREA.get();

  /** The key for GeoJSON features */
  private static final String GEOJSON_FEATURES = "features";

  /**
   * Executor that will run the petitions.
   */
  private static ThreadPoolExecutor executor = new ThreadPoolExecutor(3, 5, 100, TimeUnit.SECONDS,
    new ArrayBlockingQueue<>(100), new ThreadPoolExecutor.DiscardPolicy());

  /**
   * Indicates whether the last download request has been rejected because it
   * requested an area that was too big. If true, the last download has been
   * rejected, if false, it was executed.
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
    try {
      run(new MapillarySquareDownloadRunnable(bounds));
    } catch (RejectedExecutionException e) {
      new Notification("Download limit reached")
        .setIcon(MapillaryPlugin.LOGO.setSize(ImageProvider.ImageSizes.LARGEICON).get())
        .setDuration(Notification.TIME_LONG).show();
      MapillaryLayer.getInstance().getData().removeDataSource(new DataSource(bounds, bounds.toString()));
    }
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
    try {
      executor.execute(t);
      MapillaryDownloadDialog.getInstance().downloadInfoChanged();
    } catch (RejectedExecutionException e) {
      throw new RejectedExecutionException(e);
    }
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
   * Checks if the given {@link LatLon} object lies inside the bounds of the image.
   *
   * @param latlon The coordinates to check.
   * @return true if it lies inside the bounds; false otherwise;
   */
  private static boolean isInBounds(LatLon latlon) {
    return MapillaryLayer.getInstance().getData().getBounds().parallelStream().anyMatch(b -> b.contains(latlon));
  }

  /**
   * Download a specific set of images
   *
   * @param images The images to download
   * @return The downloaded images
   */
  public static Map<String, Collection<MapillaryAbstractImage>> downloadImages(String... images) {
    if (images.length == 0) {
      return Collections.emptyMap();
    }
    JsonObject response = getUrlResponse(MapillaryURL.APIv3.getImage(images));
    return Collections
      .unmodifiableMap(JsonDecoder.decodeFeatureCollection(response, JsonImageDetailsDecoder::decodeImageInfos).stream()
        .collect(Collector.of(HashMap<String, Collection<MapillaryAbstractImage>>::new,
          (rMap, oMap) -> rMap.putAll(oMap), (rMap, oMap) -> {
            rMap.putAll(oMap);
            return rMap;
          })));
  }

  /**
   * Download a specific set of sequences
   *
   * @param sequences The sequences to download
   * @return The downloaded sequences
   */
  public static Collection<MapillarySequence> downloadSequences(String... sequences) {
    JsonObject response = getUrlResponse(MapillaryURL.APIv3.getSequence(sequences));
    Collection<MapillarySequence> returnSequences = JsonDecoder.decodeFeatureCollection(response,
      JsonSequencesDecoder::decodeSequence);
    JsonObject imageResponse = getUrlResponse(MapillaryURL.APIv3
      .getImagesBySequences(returnSequences.stream().map(MapillarySequence::getKey).distinct().toArray(String[]::new)));
    JsonDecoder.decodeFeatureCollection(imageResponse, JsonImageDetailsDecoder::decodeImageInfos);
    return returnSequences;
  }

  private static JsonObject getUrlResponse(URL url) {
    HttpClient client = OAuthUtils.addAuthenticationHeader(HttpClient.create(url));
    try (JsonReader reader = Json.createReader(client.connect().getContentReader())) {
      Response response = client.connect();
      URL next = MapillaryURL.APIv3.parseNextFromLinkHeaderValue(response.getHeaderField("Link"));
      JsonObject returnObject = reader.readObject();
      if (next != null && !next.toExternalForm().trim().isEmpty() && !next.equals(url)) {
        JsonObject nextObject = getUrlResponse(next);
        if (checkFeaturesIsArray(returnObject) && checkFeaturesIsArray(nextObject)) {
          JsonArrayBuilder rArray = Json.createArrayBuilder(returnObject.getJsonArray(GEOJSON_FEATURES));
          rArray.addAll(Json.createArrayBuilder(nextObject.getJsonArray(GEOJSON_FEATURES)));
          JsonObjectBuilder rBuilder = Json.createObjectBuilder(returnObject);
          rBuilder.add(GEOJSON_FEATURES, rArray);
          returnObject = rBuilder.build();
        }
      }
      return returnObject;
    } catch (IOException e) {
      Logging.trace(e);
      return null;
    } finally {
      client.disconnect();
    }
  }

  /**
   * Utility method to check that a GeoJSON has a feature array
   *
   * @param object The object to check
   * @return {@code true} if the object has a valid feature array
   */
  private static boolean checkFeaturesIsArray(JsonObject object) {
    return object.containsKey(GEOJSON_FEATURES)
      && object.get(GEOJSON_FEATURES).getValueType() == JsonValue.ValueType.ARRAY;
  }

  /**
   * Downloads all images of the area covered by the OSM data.
   */
  public static void downloadOSMArea() {
    if (MainApplication.getLayerManager().getEditLayer() == null) {
      return;
    }
    if (isAreaTooBig(MainApplication.getLayerManager().getEditLayer().data.getDataSourceBounds().parallelStream()
      .map(Bounds::getArea).reduce(0.0, Double::sum))) {
      return;
    }
    MainApplication.getLayerManager().getEditLayer().data.getDataSourceBounds().stream()
      .filter(bounds -> !MapillaryLayer.getInstance().getData().getBounds().contains(bounds)).forEach(bounds -> {
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
      new Notification(I18n
        .tr("The Mapillary layer has stopped downloading images, because the requested area is too big!")
        + (getMode() == DOWNLOAD_MODE.VISIBLE_AREA
          ? "\n" + I18n.tr("To solve this problem, you could zoom in and load a smaller area of the map.")
          : (getMode() == DOWNLOAD_MODE.OSM_AREA ? "\n" + I18n.tr(
            "To solve this problem, you could switch to download mode ''{0}'' and load Mapillary images for a smaller portion of the map.",
            DOWNLOAD_MODE.MANUAL_ONLY) : ""))).setIcon(MapillaryPlugin.LOGO.get()).setDuration(Notification.TIME_LONG)
              .show();
    }
    if (stoppedDownload && !tooBig) {
      new Notification("The Mapillary layer now continues to download images…").setIcon(MapillaryPlugin.LOGO.get())
        .show();
    }
    stoppedDownload = tooBig;
    return tooBig;
  }

  /**
   * Stops all running threads.
   */
  public static synchronized void stopAll() {
    List<Runnable> shutdownTasks = executor.shutdownNow();
    try {
      executor.awaitTermination(30, TimeUnit.SECONDS);
    } catch (InterruptedException e) {
      Logging.error(e);
      Thread.currentThread().interrupt();
    }
    executor = new ThreadPoolExecutor(3, 5, 100, TimeUnit.SECONDS, new ArrayBlockingQueue<>(100),
      new ThreadPoolExecutor.DiscardPolicy());
    shutdownTasks.forEach((download) -> {
      MapillarySquareDownloadRunnable msdrDownload = (MapillarySquareDownloadRunnable) download;
      msdrDownload.getMonitor().finishTask();
      removeHash(msdrDownload);
    });
    DownloadTableModel.getInstance().reset();
    MapillaryUtils.updateHelpText();
  }

  /**
   * @return No. of downloads which haven't started yet.
   */
  public static int getQueuedSize() {
    return executor.getQueue().size();
  }

  /**
   * Remove all downloads which haven't started yet
   */
  public static void removeQueued() {
    executor.getQueue().forEach((download) -> {
      removeDownload((MapillarySquareDownloadRunnable) download);
    });
  }

  /**
   * Remove a download which has not been started yet
   */
  public static void removeDownload(MapillarySquareDownloadRunnable download) {
    if (executor.remove(download)) {
      removeHash(download);
      download.getMonitor().finishTask();
    }
  }

  /**
   * Remove background hash of download if it was not able to complete
   */
  public static void removeHash(MapillarySquareDownloadRunnable download) {
    MapillaryLayer.getInstance().getData()
      .removeDataSource(new DataSource(download.getBounds(), download.getBounds().toString()));
  }

  /**
   * Rerun a failed download.
   */
  public static void reRunDownload(MapillarySquareDownloadRunnable download) {
    executor.execute(download);
  }
}
