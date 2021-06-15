// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.model;

import java.awt.Color;
import java.awt.Shape;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.Serializable;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ForkJoinTask;
import java.util.function.BiConsumer;

import org.apache.commons.jcs3.access.CacheAccess;
import org.openstreetmap.josm.data.cache.JCSCacheManager;
import org.openstreetmap.josm.data.preferences.BooleanProperty;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.ObjectDetections;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.PointObjectLayer;
import org.openstreetmap.josm.plugins.mapillary.utils.DetectionVerification;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryColorScheme;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.api.JsonDecoder;
import org.openstreetmap.josm.plugins.mapillary.utils.api.JsonImageDetectionDecoder;
import org.openstreetmap.josm.tools.HttpClient;
import org.openstreetmap.josm.tools.JosmRuntimeException;
import org.openstreetmap.josm.tools.Logging;
import org.openstreetmap.josm.tools.Pair;

import javax.json.Json;
import javax.json.JsonObject;
import javax.json.JsonReader;

/**
 * A store for ImageDetection information
 */
public class ImageDetection<T extends Shape & Serializable> extends SpecialImageArea<T> {
  /**
   * A ForkJoinTask that can be cancelled
   */
  public static class ImageDetectionForkJoinTask extends ForkJoinTask<List<ImageDetection<?>>> {
    private final BiConsumer<String, List<ImageDetection<?>>> listener;
    public final String key;
    private List<ImageDetection<?>> results;

    public ImageDetectionForkJoinTask(String key, BiConsumer<String, List<ImageDetection<?>>> listener) {
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
        // Use new ArrayList<>() (despite allocations) since this avoids an expensive synchronization
        for (CacheAccess<String, List<ImageDetection<?>>> cache : new ArrayList<>(searchDetections)) {
          if (this.isCancelled()) {
            break;
          }
          final List<ImageDetection<?>> layerDetections = cache.get(key,
            () -> getDetections(key, cache.getCacheAttributes().getCacheName().replaceFirst(CACHE_NAME_PREFIX, "")));
          detections.addAll(layerDetections);
        }
        if (listener != null) {
          listener.accept(key, detections);
        }
        this.complete(detections);
        return true;
      }
      return false;
    }
  }

  /**
   * This is used to dynamically get the layer name (so it is CRITICAL that the caches use
   * {@code CACHE_NAME_PREFIX + "layer_name"})
   */
  private static final String CACHE_NAME_PREFIX = "mapillary:image:";
  /** Traffic sign detections (on a per-image basis) */
  private static final CacheAccess<String, List<ImageDetection<?>>> TRAFFIC_SIGN = JCSCacheManager
    .getCache(CACHE_NAME_PREFIX + "trafficsigns");
  /** Segmentation detections (on a per-image basis) */
  private static final CacheAccess<String, List<ImageDetection<?>>> SEGMENTATIONS = JCSCacheManager
    .getCache(CACHE_NAME_PREFIX + "segmentations");
  /** Point object detections (on a per-image basis) */
  private static final CacheAccess<String, List<ImageDetection<?>>> INSTANCES = JCSCacheManager
    .getCache(CACHE_NAME_PREFIX + "instances");
  private static final String PACKAGE_TRAFFIC_SIGNS = "trafficsign";

  private static final List<CacheAccess<String, List<ImageDetection<?>>>> searchDetections = new ArrayList<>(3);

  private static void setSearchDetections() {
    // segmentations take awhile to get (10+ seconds on my machine), so default off
    BooleanProperty segmentationProperty = new BooleanProperty("mapillary.image.detections.segmentation", false);
    segmentationProperty.addWeakListener(value -> {
      if (Boolean.TRUE.equals(value.getProperty().get())) {
        searchDetections.add(SEGMENTATIONS);
      } else {
        searchDetections.remove(SEGMENTATIONS);
      }
    });
    MapillaryProperties.SHOW_DETECTED_SIGNS.addWeakListener(value -> {
      if (Boolean.TRUE.equals(value.getProperty().get())) {
        searchDetections.add(TRAFFIC_SIGN);
      } else {
        searchDetections.remove(TRAFFIC_SIGN);
      }
    });
    MapillaryProperties.SHOW_DETECTION_OUTLINES.addWeakListener(value -> {
      if (Boolean.TRUE.equals(value.getProperty().get())) {
        searchDetections.add(INSTANCES);
      } else {
        searchDetections.remove(INSTANCES);
      }
    });
    if (Boolean.TRUE.equals(MapillaryProperties.SHOW_DETECTED_SIGNS.get())) {
      searchDetections.add(TRAFFIC_SIGN);
    }
    if (Boolean.TRUE.equals(MapillaryProperties.SHOW_DETECTION_OUTLINES.get())) {
      searchDetections.add(INSTANCES);
    }
    if (Boolean.TRUE.equals(segmentationProperty.get())) {
      searchDetections.add(SEGMENTATIONS);
    }
  }

  static {
    setSearchDetections();
  }

  /**
   * Get the detections for an image key
   *
   * @param key The image key
   * @param listener The consumer to notify when the detections are downloaded
   * @return A ForkJoinTask (just in case it needs to be cancelled)
   */
  public static ImageDetectionForkJoinTask getDetections(String key,
    BiConsumer<String, List<ImageDetection<?>>> listener) {
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
  public static List<ImageDetection<?>> getDetections(String key, boolean wait) {
    if (wait) {
      try {
        return new ImageDetectionForkJoinTask(key, null).get();
      } catch (InterruptedException e) {
        Logging.error(e);
        Thread.currentThread().interrupt();
        return Collections.emptyList();
      } catch (ExecutionException e) {
        throw new JosmRuntimeException(e.getCause());
      }
    } else {
      final List<ImageDetection<?>> detections = new ArrayList<>();
      for (CacheAccess<String, List<ImageDetection<?>>> cache : new ArrayList<>(searchDetections)) {
        if (key != null && cache.get(key) != null) {
          detections.addAll(cache.get(key));
        }
      }
      return detections;
    }
  }

  /**
   * Get the detections for a key and layer
   *
   * @param key The image key
   * @param layer The layer to get detections for
   * @return The detections
   */
  private static List<ImageDetection<?>> getDetections(String key, String layer) {
    // Object detections currently don't have geometry. TODO fixme
    if (true) {
      return Collections.emptyList();
    }
    URL layerUrl = MapillaryURL.APIv3.getDetections(key, layer);
    HttpClient client = HttpClient.create(layerUrl);
    try {
      HttpClient.Response response = client.connect();
      try (BufferedReader reader = response.getContentReader(); JsonReader parser = Json.createReader(reader)) {
        JsonObject object = parser.readObject();
        Collection<ImageDetection<?>> detections = JsonDecoder.decodeFeatureCollection(object,
          json -> JsonImageDetectionDecoder.decodeImageDetection(json, layer));
        if (detections instanceof List) {
          return (List<ImageDetection<?>>) detections;
        }
        return new ArrayList<>(detections);
      }
    } catch (IOException e) {
      Logging.error(e);
    }
    return Collections.emptyList();
  }

  private final String packag;
  private final double score;
  private final ObjectDetections value;
  private final String originalValue;
  private final boolean rejected;

  private DetectionVerification.TYPE approvalType;

  /**
   * Create a new ImageDetection
   *
   * @param shape The shape of the image detection to be drawn
   * @param imageKey The image key for the detection
   * @param key The detection key
   * @param score The score of the detection
   * @param packag The layer (formerly package) of the detection
   * @param value The actual detection value (e.g., `object--fire-hydrant`)
   */
  public ImageDetection(final T shape, final String imageKey, final String key, final double score, final String packag,
    final String value) {
    super(shape, imageKey, key);
    this.packag = packag;
    this.score = score;
    Pair<Boolean, ObjectDetections> foundDetection = ObjectDetections.findFallbackDetection(value);
    this.value = foundDetection.b;
    this.rejected = foundDetection.a;
    if (this.value == ObjectDetections.UNKNOWN) {
      this.originalValue = value;
    } else {
      this.originalValue = null;
    }
  }

  /**
   * Get the layer (formerly called "package") of the detection
   *
   * @return The detection layer
   */
  public String getPackage() {
    return packag;
  }

  public double getScore() {
    return score;
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
    return (packag != null && packag.contains(PACKAGE_TRAFFIC_SIGNS))
      || (value != null && value.getKey().contains("traffic-sign"));
  }

  /**
   * Get the color to paint this detection with
   *
   * @return The color to paint the detection outline
   */
  public Color getColor() {
    if (MainApplication.getLayerManager().getLayersOfType(PointObjectLayer.class).parallelStream()
      .map(PointObjectLayer::getData).flatMap(ds -> ds.getSelected().parallelStream())
      .filter(prim -> prim.hasKey("detections")).anyMatch(prim -> prim.get("detections").contains(getKey()))) {
      return isRejected() || Boolean.TRUE.equals(MapillaryProperties.SMART_EDIT.get()) ? Color.RED : Color.CYAN;
    }
    if (isTrafficSign())
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
        && Objects.equals(this.packag, o.packag) && this.rejected == o.rejected && this.score == o.score
        && Objects.equals(this.value, o.value);
    }
    return false;
  }

  @Override
  public int hashCode() {
    return Objects.hash(super.hashCode(), this.approvalType, this.originalValue, this.packag, this.rejected, this.score,
      this.value);
  }
}
