// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.traffico;

import java.awt.Color;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonObject;

import org.openstreetmap.josm.Main;

public final class TrafficoSign {
  private static Map<String, Map<String, TrafficoSignElement[]>> signs = new HashMap<>();

  private TrafficoSign() {
    // private constructor to avoid instantiation
  }

  public static TrafficoSignElement[] getSign(String country, String signName) {
    if (signs.get(country) == null) {
      Main.info("Reading signs for country ''{0}''.", country);
      try (
        InputStream countryStream = TrafficoSign.class
            .getResourceAsStream("/data/fonts/traffico/signs/" + country + ".json")) {
        if (countryStream == null) {
          return null;
        }
        JsonObject countrySigns = Json.createReader(countryStream).readObject();
        Set<String> countrySignNames = countrySigns.keySet();
        Main.info(
            countrySignNames.size() + " different signs are supported for ''{0}'' by the Mapillary-plugin .",
            country
        );
        Map<String, TrafficoSignElement[]> countryMap = new HashMap<>();
        for (String name : countrySignNames) {
          JsonArray elements = countrySigns.getJsonObject(name).getJsonArray("elements");

          // TODO: Replace by an array when all types of sign elements (text!) are
          // supported
          List<TrafficoSignElement> layers = new ArrayList<>();

          for (int i = 0; i < elements.size(); i++) {
            Character glyph = TrafficoGlyph.getGlyph(elements.getJsonObject(i)
                .getString("type"));
            if (glyph != null) {
              Color c;
              switch (elements.getJsonObject(i).getString("color").toLowerCase()) {
                case "black":
                  c = Color.BLACK;
                  break;
                case "red":
                  c = Color.RED;
                  break;
                case "blue":
                  c = Color.BLUE;
                  break;
                case "white":
                  c = Color.WHITE;
                  break;
                case "green":
                  c = Color.GREEN;
                  break;
                case "yellow":
                  c = Color.YELLOW;
                  break;
                default:
                  c = Color.MAGENTA;
              }
              layers.add(new TrafficoSignElement(glyph, c));
            }
          }
          countryMap.put(name, layers.toArray(new TrafficoSignElement[layers.size()]));
        }
        signs.put(country, countryMap);
      } catch (IOException e) {
        Main.error(e);
      }
    }
    if (signs.get(country).get(signName) != null) {
      return signs.get(country).get(signName);
    }
    return null;
  }
}
