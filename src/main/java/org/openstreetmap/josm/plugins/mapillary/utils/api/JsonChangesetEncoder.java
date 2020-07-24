// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import java.util.Comparator;
import java.util.Objects;

import javax.json.Json;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObjectBuilder;

import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.gui.changeset.MapillaryChangeset;

/**
 * Encodes in JSON a location/deletion changeset.
 * Former location and compass angle (CA) are systematically provided,
 * even if not changed.
 */
public final class JsonChangesetEncoder {

  private JsonChangesetEncoder() {
    // Private constructor to avoid instantiation
  }

  public static JsonObjectBuilder encodeLocationChangeset(MapillaryChangeset changeset) {
    Objects.requireNonNull(changeset);
    final JsonArrayBuilder imgChanges = Json.createArrayBuilder();

    changeset.stream()
      .sorted(Comparator.comparing(MapillaryImage::getKey)) // sort for easier testing
      .forEach(it -> imgChanges.add(encodeImageLocationChanges(it)));
    return Json.createObjectBuilder()
      .add("type", "location")
      .add("changes", imgChanges)
      .add("request_comment", "JOSM-created");
  }

  public static JsonObjectBuilder encodeDeletionChangeset (MapillaryChangeset changeset) {
    Objects.requireNonNull(changeset);
    final JsonArrayBuilder imgChanges = Json.createArrayBuilder();

    changeset.stream()
      .sorted(Comparator.comparing(MapillaryImage::getKey)) // sort for easier testing
      .forEach(it -> imgChanges.add(encodeImageDeletionChanges(it)));
    return Json.createObjectBuilder()
      .add("type", "deletion")
      .add("changes", imgChanges)
      .add("request_comment", "JOSM-created");

  }

  protected static JsonObjectBuilder encodeImageLocationChanges(MapillaryImage img) {
    Objects.requireNonNull(img);

    final JsonObjectBuilder from = getChangeJsonBuilder(img.getLatLon(), img.getCa());
    final JsonObjectBuilder to = getChangeJsonBuilder(img.getTempLatLon(), img.getTempCa());

    return Json.createObjectBuilder()
      .add("image_key", img.getKey())
      .add("from", from)
      .add("to", to);
  }

  protected static JsonObjectBuilder encodeImageDeletionChanges(MapillaryImage img) {
    Objects.requireNonNull(img);

    final JsonObjectBuilder from = getChangeJsonBuilder(img.getLatLon(), img.getCa());

    return Json.createObjectBuilder()
      .add("image_key", img.getKey())
      .add("from", from);
  }

  private static JsonObjectBuilder getChangeJsonBuilder(LatLon tempLatLon, double tempCa) {
    final JsonObjectBuilder to = Json.createObjectBuilder();
    to.add("type", "Feature");
    to.add("geometry", Json.createObjectBuilder()
      .add("coordinates", Json.createArrayBuilder()
        .add(tempLatLon.getX())
        .add(tempLatLon.getY())
      ).add("type", "Point")
    );
    to.add("properties", Json.createObjectBuilder().add("ca", tempCa));
    return to;
  }
}
