// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.model;

import java.awt.Color;
import java.awt.Shape;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.ForkJoinTask;
import java.util.function.BiConsumer;

import javax.json.Json;
import javax.json.JsonObject;
import javax.json.JsonReader;

import org.openstreetmap.josm.data.cache.JCSCacheManager;
import org.openstreetmap.josm.data.osm.IPrimitive;
import org.openstreetmap.josm.data.vector.VectorDataSet;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.plugins.mapillary.cache.Caches;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.DetectionType;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.ObjectDetections;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.PointObjectLayer;
import org.openstreetmap.josm.plugins.mapillary.oauth.OAuthUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.DetectionVerification;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryColorScheme;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.api.JsonDecoder;
import org.openstreetmap.josm.plugins.mapillary.utils.api.JsonImageDetectionDecoder;
import org.openstreetmap.josm.tools.Logging;
import org.openstreetmap.josm.tools.Pair;

import org.apache.commons.jcs3.access.CacheAccess;

/**
 * A store for ImageDetection information
 */
public class ImageDetection<T extends Shape> extends SpecialImageArea<Long, T> {
  private static final String DETECTIONS = "detections";

  /**
   * This is used to dynamically get the layer name (so it is CRITICAL that the caches use
   * {@code CACHE_NAME_PREFIX + "layer_name"})
   */
  private static final String CACHE_NAME_PREFIX = "mapillary:image:";
  /** Detections (on a per-image basis) */
  private static final CacheAccess<Long, List<ImageDetection<?>>> DETECTION_CACHE = JCSCacheManager
    .getCache(CACHE_NAME_PREFIX + DETECTIONS);
  private static final String PACKAGE_TRAFFIC_SIGNS = "trafficsign";

  private final ObjectDetections value;
  private final String originalValue;
  private final boolean rejected;

  private DetectionVerification.TYPE approvalType;

  /**
   * Get the detections for an image key
   *
   * @param key The image key
   * @param listener The consumer to notify when the detections are downloaded
   * @return A ForkJoinTask (just in case it needs to be cancelled)
   */
  public static ImageDetectionForkJoinTask getDetections(long key, BiConsumer<Long, List<ImageDetection<?>>> listener) {
    return (ImageDetectionForkJoinTask) MapillaryUtils.getForkJoinPool()
      .submit(new ImageDetectionForkJoinTask(key, listener));
  }

  /**
   * Get the detections for an image key
   *
   * @param key The image key
   * @param wait {@code true} to wait for the detections
   * @return The image detections
   */
  public static List<ImageDetection<?>> getDetections(long key, boolean wait) {
    if (wait) {
      final ImageDetectionForkJoinTask task = new ImageDetectionForkJoinTask(key, null);
      task.fork();
      return task.join();
    } else {
      if (key != 0 && DETECTION_CACHE.get(key) != null) {
        return Collections.unmodifiableList(new ArrayList<>(DETECTION_CACHE.get(key)));
      }
      return Collections.emptyList();
    }
  }

  /**
   * Create a new ImageDetection
   *
   * @param shape The shape of the image detection to be drawn
   * @param imageKey The image key for the detection
   * @param key The detection key
   * @param value The actual detection value (e.g., `object--fire-hydrant`)
   */
  public ImageDetection(final T shape, final long imageKey, final long key, final String value) {
    super(shape, imageKey, key);
    final Pair<Boolean, ObjectDetections> foundDetection = ObjectDetections.findFallbackDetection(value);
    this.value = foundDetection.b;
    this.rejected = foundDetection.a;
    if (this.value == ObjectDetections.UNKNOWN) {
      this.originalValue = value;
    } else {
      this.originalValue = null;
    }
  }

  /**
   * Check if the detection has been rejected
   *
   * @return {@code true} if rejected
   */
  public boolean isRejected() {
    return this.rejected;
  }

  /**
   * Get the ObjectDetection type
   *
   * @return The {@link ObjectDetections} value
   */
  public ObjectDetections getValue() {
    return value;
  }

  /**
   * Get the original value, if this was unknown
   *
   * @return The unknown value for the detection, or the mapillary key from the {@link ObjectDetections}.
   */
  public String getUnknownValue() {
    return this.originalValue == null ? getValue().getKey() : this.originalValue;
  }

  /**
   * Check if the detection was a traffic sign
   *
   * @return {@code true} if the detection is a traffic sign
   */
  public boolean isTrafficSign() {
    return this.value.getDetectionTypes().contains(DetectionType.TRAFFIC_SIGN)
      || (this.value == ObjectDetections.UNKNOWN && this.originalValue.contains(PACKAGE_TRAFFIC_SIGNS));
  }

  /**
   * Get the color to paint this detection with
   *
   * @return The color to paint the detection outline
   */
  public Color getColor() {
    if (MainApplication.getLayerManager().getLayersOfType(PointObjectLayer.class).parallelStream()
      .map(PointObjectLayer::getData).map(VectorDataSet::getSelected).flatMap(Collection::stream)
      .mapToLong(IPrimitive::getId).mapToObj(id -> ImageDetection.getDetections(id, false)).flatMap(Collection::stream)
      .filter(Objects::nonNull).anyMatch(id -> id.getKey().equals(this.getKey()))) {
      return isRejected() || Boolean.TRUE.equals(MapillaryProperties.SMART_EDIT.get()) ? Color.RED : Color.CYAN;
    }
    if (this.isTrafficSign())
      return MapillaryColorScheme.IMAGEDETECTION_TRAFFICSIGN;
    if (ObjectDetections.IGNORE_DETECTIONS.contains(value))
      return Color.LIGHT_GRAY;
    return MapillaryColorScheme.IMAGEDETECTION_UNKNOWN;
  }

  /**
   * Set the approval type
   *
   * @param type The approval type
   */
  public void setApprovalType(DetectionVerification.TYPE type) {
    this.approvalType = type;
  }

  /**
   * Get the approval type
   *
   * @return The approval type
   */
  public DetectionVerification.TYPE getApprovalType() {
    return this.approvalType;
  }

  @Override
  public boolean equals(Object other) {
    if (super.equals(other) && other instanceof ImageDetection) {
      ImageDetection<?> o = (ImageDetection<?>) other;
      return Objects.equals(this.approvalType, o.approvalType) && Objects.equals(this.originalValue, o.originalValue)
        && this.rejected == o.rejected && Objects.equals(this.value, o.value);
    }
    return false;
  }

  @Override
  public int hashCode() {
    return Objects.hash(super.hashCode(), this.approvalType, this.originalValue, this.rejected, this.value);
  }

  /**
   * A ForkJoinTask that can be cancelled
   */
  public static class ImageDetectionForkJoinTask extends ForkJoinTask<List<ImageDetection<?>>> {
    private final BiConsumer<Long, List<ImageDetection<?>>> listener;
    public final long key;
    private List<ImageDetection<?>> results;

    public ImageDetectionForkJoinTask(long key, BiConsumer<Long, List<ImageDetection<?>>> listener) {
      this.key = key;
      this.listener = listener;
    }

    @Override
    public List<ImageDetection<?>> getRawResult() {
      return this.results;
    }

    @Override
    protected void setRawResult(List<ImageDetection<?>> value) {
      this.results = value;
    }

    @Override
    protected boolean exec() {
      if (!this.isCancelled()) {
        List<ImageDetection<?>> detections = new ArrayList<>();
        final List<ImageDetection<?>> layerDetections = DETECTION_CACHE.get(key, () -> getDetections(key));
        detections.addAll(layerDetections);
        if (listener != null) {
          listener.accept(key, detections);
        }
        this.complete(detections);
        return true;
      }
      return false;
    }

    /**
     * Get the detections for a key and layer
     *
     * @param key The image key
     * @return The detections
     */
    private static List<ImageDetection<?>> getDetections(long key) {
      final String urlString = MapillaryURL.APIv4.getDetectionInformation(key);
      final String jsonString = Caches.META_DATA_CACHE.get(urlString, () -> {
        try {
          final JsonObject jsonObject = OAuthUtils.getWithHeader(new URL(urlString));
          return jsonObject != null ? jsonObject.toString() : null;
        } catch (IOException e) {
          Logging.error(e);
        }
        return null;
      });
      if (jsonString != null) {
        try (JsonReader reader = Json
          .createReader(new ByteArrayInputStream(jsonString.getBytes(StandardCharsets.UTF_8)))) {
          final Collection<ImageDetection<?>> detections = JsonDecoder.decodeData(reader.readObject(),
            JsonImageDetectionDecoder::decodeImageDetection);
          if (detections instanceof List) {
            return Collections.unmodifiableList((List<ImageDetection<?>>) detections);
          }
          return Collections.unmodifiableList(new ArrayList<>(detections));
        }
      }
      return Collections.emptyList();
    }
  }
}
