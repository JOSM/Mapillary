// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import java.util.Objects;

import javax.json.Json;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObjectBuilder;

import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLocationChangeset;

/**
 * Encodes in JSON a location changeset.
 * Former location and compass angle (CA) are systematically provided,
 * even if not changed.
 */
public final class JsonLocationChangesetEncoder {

  private JsonLocationChangesetEncoder() {
    // Private constructor to avoid instantiation
  }

  public static JsonObjectBuilder encodeLocationChangeset(MapillaryLocationChangeset changeset) {
    Objects.requireNonNull(changeset);
    final JsonArrayBuilder imgChanges = Json.createArrayBuilder();
    for (MapillaryImage img : changeset) {
      imgChanges.add(encodeImageChanges(img));
    }
    return Json.createObjectBuilder()
      .add("type", "location")
      .add("changes", imgChanges)
      .add("request_comment", "JOSM-created");
  }

  protected static JsonObjectBuilder encodeImageChanges(MapillaryImage img) {
    Objects.requireNonNull(img);

    final JsonObjectBuilder from = getChangeJsonBuilder(img.getLatLon(), img.getCa());
    final JsonObjectBuilder to = getChangeJsonBuilder(img.getTempLatLon(), img.getTempCa());

    return Json.createObjectBuilder()
      .add("image_key", img.getKey())
      .add("from", from)
      .add("to", to);
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
