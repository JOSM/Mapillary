// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import javax.json.JsonObject;
import javax.json.JsonValue;

import org.openstreetmap.josm.plugins.mapillary.data.mapillary.GroupRecord;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL.APIv3;
import org.openstreetmap.josm.tools.Logging;

/**
 * Decodes the JSON returned by {@link APIv3} into Java objects. Takes a {@link JsonObject} tore create groups
 *
 * @author Taylor Smock
 */
public final class JsonGroupDecoder {
  private JsonGroupDecoder() {
    // Avoid instantiation
  }

  public static List<GroupRecord> decodeGroups(JsonValue groups) {
    if (groups.getValueType().equals(JsonValue.ValueType.ARRAY)) {
      return groups.asJsonArray().stream().filter(val -> val.getValueType().equals(JsonValue.ValueType.OBJECT))
        .map(JsonValue::asJsonObject).map(JsonGroupDecoder::decodeGroup).collect(Collectors.toList());
    }
    Logging.error("Mapillary: Cannot decode groups from a {0}", groups.getValueType());
    return Collections.emptyList();
  }

  public static GroupRecord decodeGroup(JsonObject json) {
    String avatar = json.getString("avatar", "");
    String description = json.getString("description", "");
    String key = json.getString("key", "");
    String name = json.getString("name", "");
    String niceName = json.getString("nice_name", "");
    boolean privateRepository = json.getBoolean("private_repository", false);
    boolean publicRepository = json.getBoolean("public_repository", false);
    return GroupRecord.getGroup(avatar, description, key, name, niceName, privateRepository, publicRepository);
  }
}
