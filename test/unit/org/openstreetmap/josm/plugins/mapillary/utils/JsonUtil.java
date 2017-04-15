// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import java.io.ByteArrayInputStream;

import javax.json.Json;
import javax.json.JsonObject;

public class JsonUtil {
  public static JsonObject string2jsonObject(String s) {
    return Json.createReader(new ByteArrayInputStream(s.getBytes())).readObject();
  }
}
