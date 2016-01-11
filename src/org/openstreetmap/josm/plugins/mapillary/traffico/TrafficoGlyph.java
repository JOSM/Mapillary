// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.traffico;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import javax.json.Json;
import javax.json.JsonObject;
import javax.json.JsonReader;

public final class TrafficoGlyph {
  private static Map<String, Character> glyphs;

  private TrafficoGlyph() {
    // private constructor to avoid instantiation
  }

  private static Map<String, Character> readGlyphsFromResources() {
    JsonReader reader = Json.createReader(TrafficoSignElement.class
        .getResourceAsStream("/data/fonts/traffico/glyphs.json"));
    JsonObject glyphObject = reader.readObject().getJsonObject("glyphs");
    Set<String> glyphNames = glyphObject.keySet();
    glyphs = new HashMap<>();
    for (String name : glyphNames) {
      glyphs.put(name, glyphObject.getString(name).charAt(0));
    }
    reader.close();
    return glyphs;
  }

  public static synchronized Character getGlyph(String key) {
    if (glyphs == null) {
      glyphs = readGlyphsFromResources();
    }
    return glyphs.get(key);
  }
}
