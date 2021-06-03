// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import org.openstreetmap.gui.jmapviewer.Tile;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.data.imagery.vectortile.mapbox.MVTTile;
import org.openstreetmap.josm.data.osm.BBox;
import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.data.osm.IWay;
import org.openstreetmap.josm.data.vector.VectorNode;
import org.openstreetmap.josm.data.vector.VectorWay;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryKeys;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL.APIv3;
import org.openstreetmap.josm.tools.Logging;
import org.openstreetmap.josm.tools.Pair;

import javax.json.JsonArray;
import javax.json.JsonNumber;
import javax.json.JsonObject;
import javax.json.JsonString;
import javax.json.JsonValue;
import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Decodes the JSON returned by {@link APIv3} into Java objects.
 * Takes a {@link JsonObject} and {@link #decodeSequence(JsonObject)} tries to convert it to a
 * {@link IWay}.
 */
public final class JsonSequencesDecoder {
  private JsonSequencesDecoder() {
    // Private constructor to avoid instantiation
  }

  /**
   * Parses a given {@link JsonObject} as a GeoJSON Feature into a {@link IWay}.
   *
   * @param json the {@link JsonObject} to be parsed
   * @return a {@link IWay} that is parsed from the given {@link JsonObject}. If mandatory information is
   *         missing from the JSON or it's not meeting the expecting format in another way, <code>null</code> will be
   *         returned.
   */
  public static VectorWay decodeSequence(final JsonObject json) {
    if (json == null || !"Feature".equals(json.getString("type", null))) {
      return null;
    }
    VectorWay result = null;
    final JsonObject properties = json.getJsonObject("properties");
    final Long capturedAt = properties == null ? null
      : JsonDecoder.decodeTimestamp(properties.getString("captured_at", null));
    if (properties != null && properties.getString("key", null) != null
      && properties.getString("user_key", null) != null && capturedAt != null) {
      result = new VectorWay(MapillaryKeys.SEQUENCE_LAYER);
      result.put(MapillaryKeys.KEY, properties.getString(MapillaryKeys.KEY, null));
      result.put(MapillaryKeys.USER_KEY, properties.getString(MapillaryKeys.USER_KEY, null));
      result.put(MapillaryKeys.ORGANIZATION_KEY, properties.getString(MapillaryKeys.ORGANIZATION_KEY, null));
      result.put(MapillaryKeys.CAPTURED_AT, capturedAt.toString());
      createImages(json, properties, result);
      if (result.getNodes().isEmpty()) {
        return null;
      }
      result.setKeys(JsonTagMapDecoder.getTagMap(properties));
    }
    return result;
  }

  private static void createImages(JsonObject json, JsonObject properties, VectorWay result) {
    final Double[] cas = decodeCoordinateProperty(properties, "cas",
      val -> val instanceof JsonNumber ? ((JsonNumber) val).doubleValue() : null, Double.class);
    final String[] imageKeys = decodeCoordinateProperty(properties, "image_keys",
      val -> val instanceof JsonString ? ((JsonString) val).getString() : null, String.class);
    final LatLon[] geometry = decodeLatLons(json.getJsonObject("geometry"));
    final int sequenceLength = Math.min(Math.min(cas.length, imageKeys.length), geometry.length);
    boolean pano = properties.getBoolean("pano", false);
    List<Pair<VectorNode, Boolean>> nodesIfNew = new ArrayList<>(imageKeys.length);
    for (int i = 0; i < sequenceLength; i++) {
      if (cas[i] != null && imageKeys[i] != null && geometry[i] != null) {
        nodesIfNew.add(getImage(imageKeys[i], geometry[i], cas[i], pano));
      }
    }
    nodesIfNew.stream().map(pair -> pair.a).flatMap(node -> node.getReferrers().stream()).distinct()
      .filter(IWay.class::isInstance).map(IWay.class::cast).forEach(way -> {
        way.setNodes(Collections.emptyList());
        way.setDeleted(true);
      });
    List<VectorNode> newNodes = new ArrayList<>(result.getNodes().size() + nodesIfNew.size());
    newNodes.addAll(result.getNodes());
    nodesIfNew.stream().map(pair -> pair.a).forEach(newNodes::add);
    result.setNodes(newNodes);

    MapillaryImageUtils.downloadImageDetails(
      nodesIfNew.stream().filter(pair -> Boolean.TRUE.equals(pair.b)).map(pair -> pair.a).collect(Collectors.toSet()));
    result.setDeleted(false);
  }

  /**
   * @param key The image key
   * @param location The location of the image
   * @param cameraAngle The camera angle
   * @param pano If the image is a panoramic image
   * @return The node for the image
   */
  private static Pair<VectorNode, Boolean> getImage(String key, LatLon location, Double cameraAngle, boolean pano) {
    VectorNode image = null;
    if (MapillaryLayer.hasInstance()) {
      BBox searchBBox = new BBox();
      searchBBox.addLatLon(location, 0.001);
      image = findImage(key, MapillaryLayer.getInstance().getData().searchNodes(searchBBox).stream());
      // Attempt to load the missing tile(s)
      if (image == null) {
        Future<Tile> future = MapillaryLayer.getInstance().loadTileFor(location);
        try {
          Tile tile = future.get(5, TimeUnit.SECONDS);
          if (tile instanceof MVTTile) {
            image = findImage(key, ((MVTTile) tile).getData().getStore().searchNodes(searchBBox).stream());
          }
        } catch (InterruptedException e) {
          Logging.error(e);
          Thread.currentThread().interrupt();
        } catch (ExecutionException | TimeoutException e) {
          Logging.error(e);
        }
      }
    }
    final boolean newNode;
    if (image == null) {
      image = new VectorNode(MapillaryKeys.IMAGE_LAYER);
      newNode = true;
    } else {
      newNode = false;
    }
    if (image.isDeleted()) {
      image.setDeleted(false);
    }
    // Update the location to the actual location, instead of the "near" location from MVT
    image.put(MapillaryImageUtils.KEY, key);
    image.setCoor(location);
    image.put(MapillaryImageUtils.CAMERA_ANGLE, cameraAngle.toString());
    image.put(MapillaryKeys.PANORAMIC, pano ? MapillaryKeys.PANORAMIC_TRUE : MapillaryKeys.PANORAMIC_FALSE);
    return new Pair<>(image, newNode);
  }

  /**
   * Find an image from a primitive stream
   *
   * @param key The image key
   * @param primitiveStream The stream of primitives
   * @param <N> The generic node type
   * @return The image, or {@code null}
   */
  private static <N extends INode> N findImage(String key, Stream<N> primitiveStream) {
    return primitiveStream.filter(node -> key.equals(MapillaryImageUtils.getKey(node))).findAny().orElse(null);
  }

  /**
   * Converts a {@link JsonArray} to a java array. The conversion from
   * {@link JsonValue} to a java type is done by the supplied function.
   *
   * @param <T> the type that the elements of the resulting array will be
   * @param array the array to be converted
   * @param decodeValueFunction the function used for conversion from {@link JsonValue} to the desired type.
   * @param clazz the desired type that the elements of the resulting array should have
   * @return the supplied array converted from {@link JsonArray} to a java array of the supplied type, converted using
   *         the supplied function. Never <code>null</code>, in case of array==null, an array of length 0 is returned.
   */
  @SuppressWarnings("unchecked")
  private static <T> T[] decodeJsonArray(final JsonArray array, final Function<JsonValue, T> decodeValueFunction,
    final Class<T> clazz) {
    final T[] result;
    if (array == null) {
      result = (T[]) Array.newInstance(clazz, 0);
    } else {
      result = (T[]) Array.newInstance(clazz, array.size());
      for (int i = 0; i < result.length; i++) {
        result[i] = decodeValueFunction.apply(array.get(i));
      }
    }
    return result;
  }

  /**
   * Given the JSON object representing the `properties` of a sequence, this method converts one attribute from the
   * `coordinateProperties` object to an array of appropriate type.
   * For example this is used to convert the `image_keys` JSON array to a String[] array or the `cas` JSON array to a
   * Double[] array.
   *
   * @param json the JSON object representing the `properties` of a sequence
   * @param key the key, which identifies the desired array inside the `coordinateProperties` object to
   *        be converted
   * @param decodeValueFunction a function that converts the {@link JsonValue}s in the JSON array to java objects of the
   *        desired type
   * @param clazz the {@link Class} object of the desired type, that the entries of the resulting array
   *        should have
   * @param <T> The object type that will be returned in an array
   * @return the resulting array, when converting the desired entry of the `coordinateProperties`.
   *         Never <code>null</code>. If no `coordinateProperties` are set, or if the desired key is not set or is not
   *         an array, then an empty array of the desired type is returned.
   */
  @SuppressWarnings("unchecked")
  private static <T> T[] decodeCoordinateProperty(final JsonObject json, final String key,
    final Function<JsonValue, T> decodeValueFunction, final Class<T> clazz) {
    final JsonValue coordinateProperties = json == null ? null : json.get("coordinateProperties");
    if (coordinateProperties instanceof JsonObject) {
      JsonValue valueArray = ((JsonObject) coordinateProperties).get(key);
      if (valueArray instanceof JsonArray) {
        return decodeJsonArray((JsonArray) valueArray, decodeValueFunction, clazz);
      }
    }
    return (T[]) Array.newInstance(clazz, 0);
  }

  private static LatLon[] decodeLatLons(final JsonObject json) {
    final JsonValue coords = json == null ? null : json.get("coordinates");
    if (coords instanceof JsonArray && "LineString".equals(json.getString("type", null))) {
      final LatLon[] result = new LatLon[((JsonArray) coords).size()];
      for (int i = 0; i < ((JsonArray) coords).size(); i++) {
        final JsonValue coord = ((JsonArray) coords).get(i);
        if (coord instanceof JsonArray) {
          result[i] = JsonDecoder.decodeLatLon((JsonArray) coord);
        }
      }
      return result;
    }
    return new LatLon[0];
  }
}
