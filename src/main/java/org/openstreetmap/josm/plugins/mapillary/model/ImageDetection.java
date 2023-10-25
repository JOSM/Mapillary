// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.model;

import java.awt.Color;
import java.awt.Shape;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumSet;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.ForkJoinTask;
import java.util.function.BiConsumer;
import java.util.function.Supplier;

import jakarta.annotation.Nullable;
import jakarta.json.Json;
import jakarta.json.JsonObject;
import jakarta.json.JsonReader;
import org.apache.commons.jcs3.access.CacheAccess;
import org.openstreetmap.josm.data.cache.JCSCacheManager;
import org.openstreetmap.josm.data.osm.IPrimitive;
import org.openstreetmap.josm.data.vector.VectorDataSet;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.plugins.mapillary.cache.Caches;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.DetectionType;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.ObjectDetections;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.PointObjectLayer;
import org.openstreetmap.josm.plugins.mapillary.oauth.OAuthUtils;
import org.openstreetmap.josm.plugins.mapillary.spi.preferences.MapillaryConfig;
import org.openstreetmap.josm.plugins.mapillary.utils.DetectionVerification;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryColorScheme;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.api.JsonDecoder;
import org.openstreetmap.josm.plugins.mapillary.utils.api.JsonImageDetectionDecoder;
import org.openstreetmap.josm.tools.ListenerList;
import org.openstreetmap.josm.tools.Logging;
import org.openstreetmap.josm.tools.Pair;

/**
 * A store for ImageDetection information
 *
 * @param <T> The shape type
 */
public class ImageDetection<T extends Shape> extends SpecialImageArea<Long, T> {

    /**
     * Options for getting detections
     */
    public enum Options {
        /** Wait for the fetch to complete. Implies {@link #FETCH} */
        WAIT,
        /** Fetch the detections */
        FETCH
    }

    private static final Timer DETECTION_TIMER = new Timer();
    private static ImageDetectionWaitTask currentTask;
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
     * Get detections at some later time
     *
     * @param key The key to use
     * @param listener The listener to notify
     * @param timeout The time to wait prior to getting the detections (milliseconds)
     */
    public static void getDetectionsLaterOptional(final long key,
        final BiConsumer<Long, Collection<ImageDetection<?>>> listener, long timeout) {

        synchronized (DETECTION_TIMER) {
            if (currentTask != null && currentTask.task.key != key) {
                currentTask.cancel();
            }
            if (currentTask != null && currentTask.task.isDone() && currentTask.task.key == key) {
                listener.accept(currentTask.task.key, currentTask.task.results);
            } else if (currentTask != null && currentTask.task.key == key) {
                if (!currentTask.task.listenerList.containsListener(listener)) {
                    currentTask.task.listenerList.addListener(listener);
                }
            } else {
                currentTask = new ImageDetectionWaitTask(new ImageDetectionForkJoinTask(key, listener));
                DETECTION_TIMER.schedule(currentTask, timeout);
            }

        }
    }

    /**
     * Add detections from another source. For example, if someone requests detections with the image.
     * Processing occurs in a separate thread.
     *
     * @param id The id of the image
     * @param supplier The supplier for the detections
     */
    public static void addDetections(long id, Supplier<Collection<ImageDetection<?>>> supplier) {
        MapillaryUtils.getForkJoinPool().execute(() -> realAddDetections(id, supplier));
    }

    /**
     * Add detections from another source. For example, if someone requests detections with the image.
     *
     * @param id The id of the image
     * @param supplier The supplier for the detections
     */
    private static void realAddDetections(long id, Supplier<Collection<ImageDetection<?>>> supplier) {
        final Collection<ImageDetection<?>> detections = supplier.get();
        if (detections != null && !detections.isEmpty()) {
            final List<ImageDetection<?>> detectionList = detections instanceof List
                ? (List<ImageDetection<?>>) detections
                : new ArrayList<>(detections);
            DETECTION_CACHE.put(id, detectionList);
        }
    }

    /**
     * Get the detections for an image key
     *
     * @param key The image key
     * @param listener The consumer to notify when the detections are downloaded
     * @return A ForkJoinTask (just in case it needs to be cancelled)
     */
    public static ImageDetectionForkJoinTask getDetections(long key,
        BiConsumer<Long, Collection<ImageDetection<?>>> listener) {
        return (ImageDetectionForkJoinTask) MapillaryUtils.getForkJoinPool()
            .submit(new ImageDetectionForkJoinTask(key, listener));
    }

    /**
     * Get the detections for an image key
     *
     * @param key The image key
     * @param options The option(s) to use. May be {@code null}
     * @return The image detections
     */
    public static List<ImageDetection<?>> getDetections(final long key, @Nullable final Options... options) {
        final EnumSet<Options> optionSet = EnumSet.noneOf(Options.class);
        Optional.ofNullable(options).map(Arrays::asList).ifPresent(optionSet::addAll);
        if (optionSet.contains(Options.WAIT)) {
            final ImageDetectionForkJoinTask task = new ImageDetectionForkJoinTask(key, null);
            task.fork();
            return task.join();
        } else {
            if (key != 0 && DETECTION_CACHE.get(key) != null) {
                return Collections.unmodifiableList(new ArrayList<>(DETECTION_CACHE.get(key)));
            } else if (key != 0 && optionSet.contains(Options.FETCH)) {
                final ImageDetectionForkJoinTask task = new ImageDetectionForkJoinTask(key, null);
                ForkJoinPool.commonPool().execute(task);
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
            .mapToLong(IPrimitive::getId).mapToObj(ImageDetection::getDetections).flatMap(Collection::stream)
            .filter(Objects::nonNull).anyMatch(id -> id.getKey().equals(this.getKey()))) {
            return isRejected() || Boolean.TRUE.equals(MapillaryProperties.SMART_EDIT.get()) ? Color.RED : Color.CYAN;
        }
        if (this.isTrafficSign())
            return MapillaryColorScheme.IMAGEDETECTION_TRAFFICSIGN;
        if (ObjectDetections.IGNORE_DETECTIONS.contains(value))
            return Color.LIGHT_GRAY;
        return MapillaryColorScheme.IMAGEDETECTION_UNKNOWN;
    }

    @Override
    public boolean equals(Object other) {
        if (super.equals(other) && this.getClass().equals(other.getClass())) {
            ImageDetection<?> o = (ImageDetection<?>) other;
            return Objects.equals(this.approvalType, o.approvalType)
                && Objects.equals(this.originalValue, o.originalValue) && this.rejected == o.rejected
                && Objects.equals(this.value, o.value);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), this.approvalType, this.originalValue, this.rejected, this.value);
    }

    /**
     * Class for delayed get of image detections
     */
    private static class ImageDetectionWaitTask extends TimerTask {
        private final ImageDetectionForkJoinTask task;

        public ImageDetectionWaitTask(final ImageDetectionForkJoinTask task) {
            this.task = task;
        }

        @Override
        public void run() {
            MapillaryUtils.getForkJoinPool().execute(this.task);
        }
    }

    /**
     * A ForkJoinTask that can be cancelled
     */
    public static class ImageDetectionForkJoinTask extends ForkJoinTask<List<ImageDetection<?>>> {
        private static final long serialVersionUID = 1356001237946179L;
        private final transient ListenerList<BiConsumer<Long, Collection<ImageDetection<?>>>> listenerList = ListenerList
            .create();
        /** The image key for the detection we are getting */
        public final long key;
        /** The results from getting the detections for the image key */
        private List<ImageDetection<?>> results;

        public ImageDetectionForkJoinTask(long key, BiConsumer<Long, Collection<ImageDetection<?>>> listener) {
            this.key = key;
            if (listener != null) {
                this.listenerList.addListener(listener);
            }
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
                final List<ImageDetection<?>> layerDetections = DETECTION_CACHE.get(key, () -> getDetections(key));
                final List<ImageDetection<?>> detections = new ArrayList<>(layerDetections);
                this.listenerList.fireEvent(listener -> listener.accept(key, detections));
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
            if (key <= 0) {
                return Collections.emptyList();
            }
            final String urlString = MapillaryConfig.getUrls().getDetectionInformation(key);
            final String jsonString = Caches.META_DATA_CACHE.get(urlString, () -> {
                try {
                    final JsonObject jsonObject = OAuthUtils.getWithHeader(URI.create(urlString));
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
                        j -> JsonImageDetectionDecoder.decodeImageDetection(j, key));
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
