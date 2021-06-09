// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import javax.json.JsonObject;

import org.openstreetmap.josm.plugins.mapillary.data.mapillary.OrganizationRecord;

/**
 * Decodes the JSON returned by {@link APIv4} into Java objects. Takes a {@link JsonObject} to create organizations
 *
 * @author Taylor Smock
 */
public final class JsonOrganizationDecoderUtils {
  private JsonOrganizationDecoderUtils() {
    // Avoid instantiation
  }

  /**
   * Decode an organization object
   *
   * @param organization The organization object
   * @return The decoded organization record
   */
  public static OrganizationRecord decodeOrganization(JsonObject organization) {
    // v4 API has slug (short name used in URLs), name (pretty name), and description
    String description = organization.getString("description", "");
    String slug = organization.getString("slug", "");
    String name = organization.getString("name", "");
    String id = organization.getString("id", "");
    return OrganizationRecord.getOrganization(id, slug, name, description);
  }
}
