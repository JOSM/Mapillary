// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.io.download;

import java.io.IOException;
import java.net.URL;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import javax.json.Json;
import javax.json.JsonException;
import javax.json.JsonReader;

import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.gui.progress.NullProgressMonitor;
import org.openstreetmap.josm.gui.progress.ProgressMonitor;
import org.openstreetmap.josm.plugins.mapillary.MapillaryData;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.ObjectDetections;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryMainDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.model.ImageDetection;
import org.openstreetmap.josm.plugins.mapillary.oauth.OAuthUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL.APIv3;
import org.openstreetmap.josm.plugins.mapillary.utils.api.JsonDecoder;
import org.openstreetmap.josm.plugins.mapillary.utils.api.JsonImageDetectionDecoder;
import org.openstreetmap.josm.spi.preferences.Config;
import org.openstreetmap.josm.tools.HttpClient;
import org.openstreetmap.josm.tools.Logging;

public class DetectionsDownloadRunnable extends BoundsDownloadRunnable {
  private static final long serialVersionUID = -3086641197630479852L;

  private static final Function<Bounds, Collection<URL>> URL_GEN = APIv3::searchDetections;

  private final MapillaryData data;

  public DetectionsDownloadRunnable(final MapillaryData data, final Bounds bounds, ProgressMonitor monitor) {
    super(bounds, URL_GEN, monitor);
    this.data = data;
  }

  public DetectionsDownloadRunnable(final MapillaryData data, final Bounds bounds, URL url, ProgressMonitor monitor) {
    super(bounds, URL_GEN, Collections.singleton(url), monitor);
    this.data = data;
  }

  @Override
  public BoundsDownloadRunnable getNextUrl(URL nextUrl) {
    // TODO figure out why the monitor is being finished/cancelled/something early (this prevents continued background
    // downloads). This is why NullProgressMonitor is used here.
    return new DetectionsDownloadRunnable(data, bounds, nextUrl, NullProgressMonitor.INSTANCE);
  }

  @Override
  public void compute() {
    super.run();
  }

  @Override
  public void run(final HttpClient client) throws IOException {
    doRun(client, data);
    this.completed = Boolean.TRUE;
  }

  /**
   * Common image get and parsing code
   *
   * @param client The client to get data with
   * @param data The location to put data
   * @return
   * @throws IOException If there is an issue with the received data
   */
  public static Map<String, List<ImageDetection>> doRun(final HttpClient client, final MapillaryData data)
    throws IOException {
    try (JsonReader reader = Json.createReader(client.getResponse().getContent())) {
      final long startTime = System.currentTimeMillis();
      Map<String, List<ImageDetection>> detections = JsonDecoder
        .decodeFeatureCollection(reader.readObject(), JsonImageDetectionDecoder::decodeImageDetection).stream()
        .collect(Collectors.groupingBy(ImageDetection::getImageKey));
      if (Config.getPref().getBoolean("mapillary.ignore_useless_detections", true)) {
        for (Map.Entry<String, List<ImageDetection>> entry : detections.entrySet()) {
          entry.getValue().removeIf(i -> i.isRejected() || ObjectDetections.IGNORE_DETECTIONS.contains(i.getValue()));
        }
      }
      logConnectionInfo(client,
        String.format("%d detections in %.2f s", detections.size(), (System.currentTimeMillis() - startTime) / 1000F));

      for (Map.Entry<String, List<ImageDetection>> entry : detections.entrySet()) {
        data.getImages().parallelStream().filter(MapillaryImage.class::isInstance).map(MapillaryImage.class::cast)
          .filter(img -> img.getKey().equals(entry.getKey()) && !entry.getValue().parallelStream()
            .allMatch(d -> img.getDetections().parallelStream().anyMatch(d::equals)))
          .forEach(img -> img.setAllDetections(entry.getValue()));
      }
      // Repaint if we set the detections for the current selected image
      Object image = MapillaryLayer.getInstance().getData().getSelectedImage();
      if (image instanceof MapillaryImage) {
        MapillaryImage mapillaryImage = (MapillaryImage) image;
        if (detections.containsKey(mapillaryImage.getKey())) {
          MapillaryMainDialog.getInstance().imageViewer.repaint();
        }
      }
      return detections;
    } catch (JsonException | NumberFormatException e) {
      throw new IOException(e);
    } finally {
      MapillaryLayer.invalidateInstance();
    }
  }

  /**
   * @param mapillaryImage
   */
  public static void get(MapillaryImage mapillaryImage) {
    HttpClient client = HttpClient.create(MapillaryURL.APIv3.retrieveDetections(mapillaryImage.getKey()));
    OAuthUtils.addAuthenticationHeader(client);
    try {
      client.connect();
      doRun(client, MapillaryLayer.getInstance().getData());
    } catch (IOException e) {
      Logging.error(e);
    } finally {
      client.disconnect();
    }
  }
}
