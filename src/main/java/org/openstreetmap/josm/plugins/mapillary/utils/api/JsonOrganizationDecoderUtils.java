// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import javax.json.JsonObject;
import javax.json.JsonValue;

import org.openstreetmap.josm.plugins.mapillary.data.mapillary.OrganizationRecord;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL.APIv3;
import org.openstreetmap.josm.tools.Logging;

/**
 * Decodes the JSON returned by {@link APIv3} into Java objects. Takes a {@link JsonObject} tore create organizations
 *
 * @author Taylor Smock
 */
public final class JsonOrganizationDecoderUtils {
  private JsonOrganizationDecoderUtils() {
    // Avoid instantiation
  }

  public static List<OrganizationRecord> decodeOrganizations(JsonValue organizations) {
    if (organizations.getValueType().equals(JsonValue.ValueType.ARRAY)) {
      return organizations.asJsonArray().stream().filter(
        val -> val.getValueType().equals(JsonValue.ValueType.OBJECT)
      ).map(JsonValue::asJsonObject).map(JsonOrganizationDecoderUtils::decodeOrganization).collect(Collectors.toList());
    }
    Logging.error("Mapillary: Cannot decode organizations from a {0}", organizations.getValueType());
    return Collections.emptyList();
  }

  public static OrganizationRecord decodeOrganization(JsonObject organization) {
    String avatar = organization.getString("avatar", "");
    String description = organization.getString("description", "");
    String key = organization.getString("key", "");
    String name = organization.getString("name", "");
    String niceName = organization.getString("nice_name", "");
    boolean privateRepository = organization.getBoolean("private_repository", false);
    boolean publicRepository = organization.getBoolean("public_repository", false);
    return OrganizationRecord
      .getOrganization(avatar, description, key, name, niceName, privateRepository, publicRepository);
  }
}
