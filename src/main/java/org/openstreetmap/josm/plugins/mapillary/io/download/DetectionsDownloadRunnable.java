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
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryMainDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.model.ImageDetection;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL.APIv3;
import org.openstreetmap.josm.plugins.mapillary.utils.api.JsonDecoder;
import org.openstreetmap.josm.plugins.mapillary.utils.api.JsonImageDetectionDecoder;
import org.openstreetmap.josm.tools.HttpClient;

public class DetectionsDownloadRunnable extends BoundsDownloadRunnable {
  private static final long serialVersionUID = -3086641197630479852L;

  private static final Function<Bounds, Collection<URL>> URL_GEN = APIv3::searchDetections;

  private final MapillaryData data;

  public DetectionsDownloadRunnable(final MapillaryData data, final Bounds bounds, ProgressMonitor monitor) {
    super(bounds, monitor);
    this.data = data;
  }

  public DetectionsDownloadRunnable(final MapillaryData data, final Bounds bounds, URL url, ProgressMonitor monitor) {
    super(bounds, Collections.singleton(url), monitor);
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
    try (JsonReader reader = Json.createReader(client.getResponse().getContent())) {
      final long startTime = System.currentTimeMillis();
      Map<String, List<ImageDetection>> detections = JsonDecoder.decodeFeatureCollection(
        reader.readObject(),
        JsonImageDetectionDecoder::decodeImageDetection
      ).stream().collect(Collectors.groupingBy(ImageDetection::getImageKey));
      logConnectionInfo(client, String.format("%d detections in %.2f s", detections.size(), (System.currentTimeMillis() - startTime) / 1000F));

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
          MapillaryMainDialog.getInstance().mapillaryImageDisplay.repaint();
        }
      }
      completed = Boolean.TRUE;
    } catch (JsonException | NumberFormatException e) {
      throw new IOException(e);
    } finally {
      MapillaryLayer.invalidateInstance();
    }
  }

  @Override
  protected Function<Bounds, Collection<URL>> getUrlGenerator() {
    return URL_GEN;
  }
}
