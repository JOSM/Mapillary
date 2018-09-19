// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import java.util.Objects;

import javax.json.Json;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObjectBuilder;

import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLocationChangeset;

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

  private static JsonObjectBuilder encodeImageChanges(MapillaryImage img) {
    Objects.requireNonNull(img);

    final JsonObjectBuilder to = Json.createObjectBuilder();
    if (!img.getTempLatLon().equalsEpsilon(img.getLatLon())) {
      to.add("geometry", Json.createObjectBuilder()
        .add("coordinates", Json.createArrayBuilder()
          .add(img.getTempLatLon().getX())
          .add(img.getTempLatLon().getY())
        ).add("type", "Point")
      );
    }
    if (Math.abs(img.getCa() - img.getTempCa()) > 1e-9) {
      to.add("properties", Json.createObjectBuilder().add("ca", img.getTempCa()));
    } else {
      to.add("properties", Json.createObjectBuilder());
    }
    if (!img.getTempLatLon().equalsEpsilon(img.getLatLon())) {
      to.add("type", "Feature");
    }

    return Json.createObjectBuilder()
      .add("image_key", img.getKey())
      .add("to", to);
  }
}
