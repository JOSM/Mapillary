// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.Instant;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.JsonReader;
import javax.json.JsonString;
import javax.json.JsonStructure;
import javax.json.JsonValue;
import javax.swing.SwingUtilities;

import org.apache.commons.imaging.common.RationalNumber;
import org.apache.commons.imaging.formats.tiff.constants.GpsTagConstants;

import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.data.osm.INode;
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

  private static final double MIN_ZOOM_SQUARE_SIDE = 0.002;
  private static final Map<String, ForkJoinPool> forkJoinPool = new HashMap<>();

  private MapillaryUtils() {
    // Private constructor to avoid instantiation
  }

  /**
   * Returns the current date formatted as EXIF timestamp.
   * As timezone the default timezone of the JVM is used ({@link java.util.TimeZone#getDefault()}).
   *
   * @return A {@code String} object containing the current date.
   */
  public static String currentDate() {
    return new SimpleDateFormat("yyyy:MM:dd HH:mm:ss", Locale.UK).format(Calendar.getInstance().getTime());
  }

  /**
   * Parses a string with a given format and returns the Epoch time.
   * If no timezone information is given, the default timezone of the JVM is used
   * ({@link java.util.TimeZone#getDefault()}).
   *
   * @param date The string containing the date.
   * @param format The format of the date.
   * @return The date in Epoch format.
   * @throws ParseException if the date cannot be parsed with the given format
   */
  public static Instant getEpoch(String date, String format) throws ParseException {
    return new SimpleDateFormat(format, Locale.UK).parse(date).toInstant();
  }

  /**
   * Calculates the decimal degree-value from a degree value given in
   * degrees-minutes-seconds-format
   *
   * @param degMinSec an array of length 3, the values in there are (in this order)
   *        degrees, minutes and seconds
   * @param ref the latitude or longitude reference determining if the given value
   *        is:
   *        <ul>
   *        <li>north (
   *        {@link GpsTagConstants#GPS_TAG_GPS_LATITUDE_REF_VALUE_NORTH}) or
   *        south (
   *        {@link GpsTagConstants#GPS_TAG_GPS_LATITUDE_REF_VALUE_SOUTH}) of
   *        the equator</li>
   *        <li>east (
   *        {@link GpsTagConstants#GPS_TAG_GPS_LONGITUDE_REF_VALUE_EAST}) or
   *        west ({@link GpsTagConstants#GPS_TAG_GPS_LONGITUDE_REF_VALUE_WEST}
   *        ) of the equator</li>
   *        </ul>
   * @return the decimal degree-value for the given input, negative when west of
   *         0-meridian or south of equator, positive otherwise
   * @throws IllegalArgumentException if {@code degMinSec} doesn't have length 3 or if {@code ref} is
   *         not one of the values mentioned above
   */
  public static double degMinSecToDouble(RationalNumber[] degMinSec, String ref) {
    if (degMinSec == null || degMinSec.length != 3) {
      throw new IllegalArgumentException("Array's length must be 3.");
    }
    for (int i = 0; i < 3; i++) {
      if (degMinSec[i] == null)
        throw new IllegalArgumentException("Null value in array.");
    }

    switch (ref) {
    case GpsTagConstants.GPS_TAG_GPS_LATITUDE_REF_VALUE_NORTH:
    case GpsTagConstants.GPS_TAG_GPS_LATITUDE_REF_VALUE_SOUTH:
    case GpsTagConstants.GPS_TAG_GPS_LONGITUDE_REF_VALUE_EAST:
    case GpsTagConstants.GPS_TAG_GPS_LONGITUDE_REF_VALUE_WEST:
      break;
    default:
      throw new IllegalArgumentException("Invalid ref.");
    }

    double result = degMinSec[0].doubleValue(); // degrees
    result += degMinSec[1].doubleValue() / 60; // minutes
    result += degMinSec[2].doubleValue() / 3600; // seconds

    if (GpsTagConstants.GPS_TAG_GPS_LATITUDE_REF_VALUE_SOUTH.equals(ref)
      || GpsTagConstants.GPS_TAG_GPS_LONGITUDE_REF_VALUE_WEST.equals(ref)) {
      result *= -1;
    }

    result = 360 * ((result + 180) / 360 - Math.floor((result + 180) / 360)) - 180;
    return result;
  }

  /**
   * Zooms to fit all the {@link INode} objects stored in the
   * database.
   */
  public static void showAllPictures() {
    showPictures(MapillaryLayer.getInstance().getData().getNodes().stream().filter(MapillaryImageUtils.IS_IMAGE)
      .collect(Collectors.toSet()), false);
  }

  /**
   * Zooms to fit all the given {@link INode} objects.
   *
   * @param images The images your are zooming to.
   * @param select Whether the added images must be selected or not.
   */
  public static void showPictures(final Set<INode> images, final boolean select) {
    if (!SwingUtilities.isEventDispatchThread()) {
      SwingUtilities.invokeLater(() -> showPictures(images, select));
    } else {
      Bounds zoomBounds;
      if (images.isEmpty()) {
        zoomBounds = new Bounds(new LatLon(0, 0));
      } else {
        zoomBounds = new Bounds(images.iterator().next().getCoor());
        for (INode img : images) {
          if (img != null && img.getCoor() != null) {
            zoomBounds.extend(img.getCoor());
          }
        }
      }

      // The zoom rectangle must have a minimum size.
      double latExtent = Math.max(zoomBounds.getMaxLat() - zoomBounds.getMinLat(), MIN_ZOOM_SQUARE_SIDE);
      double lonExtent = Math.max(zoomBounds.getMaxLon() - zoomBounds.getMinLon(), MIN_ZOOM_SQUARE_SIDE);
      zoomBounds = new Bounds(zoomBounds.getCenter(), latExtent, lonExtent);

      MainApplication.getMap().mapView.zoomTo(zoomBounds);
      MapillaryLayer.getInstance().setSelected(Collections.emptyList());
      MapillaryLayer.getInstance().getData().clearSelection();
      if (select) {
        MapillaryLayer.getInstance().setSelected(images);
      }
      MapillaryLayer.invalidateInstance();
    }

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
      && MapillaryLayer.getInstance().getData().getNodes().stream().anyMatch(MapillaryImageUtils.IS_IMAGE)) {
      ret.append(I18n.tr("Total Mapillary images: {0}", MapillaryLayer.getInstance().getToolTipText()));
    } else {
      ret.append(I18n.tr("No images found"));
    }
    MainApplication.getMap().statusLine.setHelpText(ret.toString());
  }

  /**
   * Check if the given ImageDetection is filtered out.
   * This checks if smart edit is enabled, and if so, if any smart edit layers are available.
   *
   * @param detectionLayers The layers to check
   * @param d The image detection to check
   * @return {@code true} if the detection is filtered
   */
  public static boolean checkIfDetectionIsFiltered(final List<PointObjectLayer> detectionLayers,
    final ImageDetection<?> d) {
    if (Boolean.TRUE.equals(MapillaryProperties.SMART_EDIT.get())
      && !MainApplication.getLayerManager().getLayersOfType(PointObjectLayer.class).isEmpty()) {
      return MainApplication.getLayerManager().getLayersOfType(PointObjectLayer.class).stream()
        .map(PointObjectLayer::getData).flatMap(ds -> ds.getAllSelected().stream()).map(MapillaryUtils::getDetections)
        .flatMap(Collection::stream).noneMatch(key -> d.getKey().equals(key));
    }
    return checkIfDetectionIsFilteredBasic(detectionLayers, d);
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
    if ((Boolean.FALSE.equals(MapillaryProperties.SHOW_DETECTION_OUTLINES.get()) && !d.isTrafficSign())
      || (Boolean.FALSE.equals(MapillaryProperties.SHOW_DETECTED_SIGNS.get()) && d.isTrafficSign()))
      return true;
    IPrimitive prim = detectionLayers.parallelStream().map(PointObjectLayer::getData)
      .flatMap(data -> data.allPrimitives().parallelStream())
      .filter(p -> p.hasKey(MapillaryKeys.DETECTIONS) && p.get(MapillaryKeys.DETECTIONS).contains(d.getKey())).findAny()
      .orElse(null);
    return prim != null && prim.isDisabled();
  }

  /**
   * Get images from detections
   *
   * @param osmPrimitive The primitive to get the detection from
   * @return A collection of image keys
   */
  public static Collection<String> getImagesFromDetections(IPrimitive osmPrimitive) {
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
            .collect(Collectors.toSet());
        }
      } catch (IOException e) {
        Logging.error(e);
      }
    }
    return Collections.emptyList();
  }

  /**
   * Get keys from detections
   *
   * @param osmPrimitive The primitive to get the detection from
   * @param <T> The primitive type
   * @return A collection of detection keys
   */
  public static <T extends IPrimitive> Collection<String> getDetections(T osmPrimitive) {
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
            .collect(Collectors.toSet());
        }
      } catch (IOException e) {
        Logging.error(e);
      }
    }
    return Collections.emptyList();
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
    if (Utils.isRunningJavaWebStart() || System.getSecurityManager() != null) {
      return getForkJoinPool(MapillaryUtils.class);
    }
    return ForkJoinPool.commonPool();
  }

  /**
   * Await quiesience on all ForkJoinPools. This excludes the common pool.
   *
   * @param timeout The amount of time units to wait
   * @param timeUnit The unit of time to wait
   */
  public static void forkJoinPoolsAwaitQuiescence(long timeout, TimeUnit timeUnit) {
    forkJoinPool.values().parallelStream().forEach(pool -> pool.awaitQuiescence(timeout, timeUnit));
  }
}
