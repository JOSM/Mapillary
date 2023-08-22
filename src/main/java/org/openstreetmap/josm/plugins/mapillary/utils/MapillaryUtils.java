// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.TimeUnit;

import jakarta.json.Json;
import jakarta.json.JsonArray;
import jakarta.json.JsonObject;
import jakarta.json.JsonReader;
import jakarta.json.JsonString;
import jakarta.json.JsonStructure;
import jakarta.json.JsonValue;
import org.openstreetmap.josm.data.osm.IPrimitive;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.PointObjectLayer;
import org.openstreetmap.josm.plugins.mapillary.model.ImageDetection;
import org.openstreetmap.josm.tools.I18n;
import org.openstreetmap.josm.tools.Logging;
import org.openstreetmap.josm.tools.Utils;

/**
 * Set of utilities.
 *
 * @author nokutu
 */
public final class MapillaryUtils {

    private static final Map<String, ForkJoinPool> forkJoinPool = new HashMap<>();
    private static final long[] EMPTY_LONG = new long[0];
    /**
     * Check if we are running webstart -- the method call is actually fairly expensive (~18% of
     * MapillaryImageEntry#read).
     * This is largely due to checking for the classes that are used in webstart every time.
     */
    private static final boolean IS_RUNNING_WEBSTART = Utils.isRunningWebStart();

    private MapillaryUtils() {
        // Private constructor to avoid instantiation
    }

    /**
     * Updates the help text at the bottom of the window.
     */
    public static void updateHelpText() {
        if (MainApplication.getMap() == null || MainApplication.getMap().statusLine == null) {
            return;
        }
        StringBuilder ret = new StringBuilder();
        if (MapillaryLayer.hasInstance()
            && MapillaryLayer.getInstance().getData().getNodes().stream().anyMatch(MapillaryImageUtils::isImage)) {
            ret.append(I18n.tr("Total Mapillary images: {0}", MapillaryLayer.getInstance().getToolTipText()));
        } else {
            ret.append(I18n.tr("No images found"));
        }
        MainApplication.getMap().statusLine.setHelpText(ret.toString());
    }

    /**
     * Check if a detection is filtered based off of {@link MapillaryProperties#SHOW_DETECTED_SIGNS}
     * and {@link MapillaryProperties#SHOW_DETECTION_OUTLINES}.
     *
     * @param detectionLayers The layers to check
     * @param d The image detection to check
     * @return {@code true} if the detection is filtered
     */
    public static boolean checkIfDetectionIsFilteredBasic(List<PointObjectLayer> detectionLayers, ImageDetection<?> d) {
        return (Boolean.FALSE.equals(MapillaryProperties.SHOW_DETECTION_OUTLINES.get()) && !d.isTrafficSign())
            || (Boolean.FALSE.equals(MapillaryProperties.SHOW_DETECTED_SIGNS.get()) && d.isTrafficSign());
    }

    /**
     * Get images from detections
     *
     * @param osmPrimitive The primitive to get the detection from
     * @return A collection of image keys
     */
    public static long[] getImagesFromDetections(IPrimitive osmPrimitive) {
        if (osmPrimitive.hasKey(MapillaryKeys.DETECTIONS)) {
            try (
                ByteArrayInputStream inputStream = new ByteArrayInputStream(
                    osmPrimitive.get(MapillaryKeys.DETECTIONS).getBytes(StandardCharsets.UTF_8));
                JsonReader reader = Json.createReader(inputStream)) {
                JsonStructure value = reader.read();
                if (value.getValueType() == JsonValue.ValueType.ARRAY) {
                    JsonArray array = value.asJsonArray();
                    return array.stream().filter(JsonObject.class::isInstance).map(JsonObject.class::cast)
                        .filter(obj -> obj.containsKey("image_key")).map(obj -> obj.get("image_key"))
                        .filter(JsonString.class::isInstance).map(JsonString.class::cast).map(JsonString::getString)
                        .mapToLong(Long::parseLong).distinct().toArray();
                }
            } catch (IOException e) {
                Logging.error(e);
            }
        }
        return EMPTY_LONG;
    }

    /**
     * Get keys from detections
     *
     * @param osmPrimitive The primitive to get the detection from
     * @param <T> The primitive type
     * @return A collection of detection keys
     */
    public static <T extends IPrimitive> long[] getDetections(T osmPrimitive) {
        if (osmPrimitive.hasKey(MapillaryKeys.DETECTIONS)) {
            try (
                ByteArrayInputStream inputStream = new ByteArrayInputStream(
                    osmPrimitive.get(MapillaryKeys.DETECTIONS).getBytes(StandardCharsets.UTF_8));
                JsonReader reader = Json.createReader(inputStream)) {
                JsonStructure value = reader.read();
                if (value.getValueType() == JsonValue.ValueType.ARRAY) {
                    JsonArray array = value.asJsonArray();
                    return array.stream().filter(JsonObject.class::isInstance).map(JsonObject.class::cast)
                        .filter(obj -> obj.containsKey("detection_key")).map(obj -> obj.get("detection_key"))
                        .filter(JsonString.class::isInstance).map(JsonString.class::cast).map(JsonString::getString)
                        .mapToLong(Long::parseLong).toArray();
                }
            } catch (IOException e) {
                Logging.error(e);
            }
        }
        return EMPTY_LONG;
    }

    /**
     * Get the Mapillary ForkJoin pools.
     *
     * @param clazz The class to use for naming and creating the ForkJoin pool
     * @return The fork join pool
     */
    public static synchronized ForkJoinPool getForkJoinPool(Class<?> clazz) {
        ForkJoinPool pool = forkJoinPool.get(clazz.getSimpleName());
        if (pool == null || pool.isShutdown()) {
            pool = Utils.newForkJoinPool("mapillary.forkjoinpool", "mapillary-" + clazz.getSimpleName() + "-%d", 2);
            forkJoinPool.put(clazz.getSimpleName(), pool);
        }
        return pool;
    }

    /**
     * This is a "known good" fork join pool that can be used with WebStart or non-webstart installs.
     *
     * @return The default ForkJoin pool
     */
    public static ForkJoinPool getForkJoinPool() {
        if (IS_RUNNING_WEBSTART || System.getSecurityManager() != null) {
            return getForkJoinPool(MapillaryUtils.class);
        }
        return ForkJoinPool.commonPool();
    }

    /**
     * Await quiescence on all ForkJoinPools. This excludes the common pool.
     *
     * @param timeout The amount of time units to wait
     * @param timeUnit The unit of time to wait
     */
    public static void forkJoinPoolsAwaitQuiescence(long timeout, TimeUnit timeUnit) {
        forkJoinPool.values().parallelStream().forEach(pool -> pool.awaitQuiescence(timeout, timeUnit));
    }
}
