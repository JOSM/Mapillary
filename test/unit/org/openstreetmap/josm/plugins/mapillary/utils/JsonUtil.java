// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import java.io.ByteArrayInputStream;
import java.nio.charset.StandardCharsets;

import javax.json.Json;
import javax.json.JsonObject;

public final class JsonUtil {
  private JsonUtil() {
    // Private constructor to avoid instantiation
  }

  public static JsonObject string2jsonObject(String s) {
    return Json.createReader(new ByteArrayInputStream(s.getBytes(StandardCharsets.UTF_8))).readObject();
  }
}
