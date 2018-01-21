// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.io.download;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.net.URL;
import java.net.URLConnection;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.function.Function;
import java.util.stream.Collectors;

import javax.json.Json;
import javax.json.JsonException;
import javax.json.JsonReader;

import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.plugins.mapillary.MapillaryData;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.model.ImageDetection;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL.APIv3;
import org.openstreetmap.josm.plugins.mapillary.utils.api.JsonDecoder;
import org.openstreetmap.josm.plugins.mapillary.utils.api.JsonImageDetectionDecoder;

public class DetectionsDownloadRunnable extends BoundsDownloadRunnable {

  private static final Function<Bounds, URL> URL_GEN = APIv3::searchDetections;

  private final MapillaryData data;

  public DetectionsDownloadRunnable(final MapillaryData data, final Bounds bounds) {
    super(bounds);
    this.data = data;
  }

  @Override
  public void run(final URLConnection con) throws IOException {
    try (JsonReader reader = Json.createReader(new BufferedInputStream(con.getInputStream()))) {
      final long startTime = System.currentTimeMillis();
      Map<String, List<ImageDetection>> detections = JsonDecoder.decodeFeatureCollection(
        reader.readObject(),
        JsonImageDetectionDecoder::decodeImageDetection
      ).stream().collect(Collectors.groupingBy(ImageDetection::getImageKey));
      logConnectionInfo(con, String.format("%d detections in %.2f s", detections.size(), (System.currentTimeMillis() - startTime) / 1000F));

      for (Entry<String, List<ImageDetection>> entry : detections.entrySet()) {
        data.getImages().stream()
          .filter(img -> img instanceof MapillaryImage && ((MapillaryImage) img).getKey().equals(entry.getKey()))
          .forEach(img -> ((MapillaryImage) img).setAllDetections(entry.getValue()));
      }
    } catch (JsonException | NumberFormatException e) {
      throw new IOException(e);
    }
    MapillaryLayer.invalidateInstance();
  }

  @Override
  protected Function<Bounds, URL> getUrlGenerator() {
    return URL_GEN;
  }
}
