// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.traffico;

import java.awt.Color;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonObject;

import org.openstreetmap.josm.Main;

/**
 * <p>
 * Representation of a traffico sign, which consists of a country code, a unique name and the individual
 * layers of the sign.
 * </p><p>
 * You can obtain a {@link TrafficoSign} via {@link #getSign(String, String)}. The first call to that method reads
 * all sign definition for the queried country from disk and holds them in memory for future access.
 * </p><p>
 * You can rely on the fact, that two instances of {@link TrafficoSign} with same {@link #getName()} and same
 * {@link #getCountry()} that were obtained via {@link #getSign(String, String)} will return <code>true</code> when
 * compared with <code>==</code>
 * </p>
 */
public final class TrafficoSign {
  private static final String SIGN_DEFINITION_PATH = "/data/fonts/traffico/signs/%s.json";
  /**
   * The signs that are already created from the JSON source file
   */
  private static final Map<String, Map<String, TrafficoSign>> signs = new TreeMap<>();

  private final String country;
  private final String name;
  private final TrafficoSignElement[] elements;

  private TrafficoSign(TrafficoSignElement[] elements, String country, String name) {
    this.country = country;
    this.name = name;
    if (elements == null) {
      this.elements = new TrafficoSignElement[0];
    } else {
      this.elements = elements.clone();
    }
  }

  /**
   * @return the country of this sign
   */
  public String getCountry() {
    return country;
  }

  /**
   * @return the name of the sign
   */
  public String getName() {
    return name;
  }

  /**
   * @return the number of sign elements (=layers) of which this sign consists
   */
  public int getNumElements() {
    return elements.length;
  }

  /**
   *
   * @param index the index which is assigned to the sign element in question
   * @return the sign element (=layer) with the given index
   * @throws ArrayIndexOutOfBoundsException if index >= {@link #getNumElements()} or index < 0
   */
  public TrafficoSignElement getElement(int index) {
    return elements[index];
  }

  /**
   * <p>
   * Returns the sign associated with the given country and name or <code>null</code> if no such sign is available.
   * </p><p>
   * If you obtain two instances of {@link TrafficoSign} with this method that have the same country and name, you can
   * safely assume that a comparison of both objects with <code>==</code> will result in <code>true</code>.
   * </p>
   * @param country the country to which the sign belongs
   * @param signName the name of the sign (unique in a country)
   * @return the requested sign or <code>null</code> if this sign is unavailable
   */
  public static TrafficoSign getSign(String country, String signName) {
    synchronized (signs) {
      if (!signs.containsKey(country)) {
        buildSignsFromJsonDefinition(country);
      }
      if (signs.containsKey(country) && signs.get(country).containsKey(signName)) {
        return signs.get(country).get(signName);
      }
      return null;
    }
  }

  private static void addSign(TrafficoSign sign) {
    synchronized (signs) {
      // Create Map for country if not already exists
      if (!signs.containsKey(sign.getCountry())) {
        signs.put(sign.getCountry(), new TreeMap<String, TrafficoSign>());
      }
      // Don't overwrite existing sign with same country-name-combination
      if (signs.get(sign.getCountry()).containsKey(sign.getName())) {
        Main.warn(
            "The sign {0}--{1} was found multiple times in the traffico sign-definitions.",
            sign.getName(), sign.getCountry()
        );
      } else {
        signs.get(sign.getCountry()).put(sign.getName(), sign);
      }
    }
  }

  private static void buildSignsFromJsonDefinition(String country) {
    Main.info("Reading signs for country ''{0}''.", country);
    try (
      InputStream countryStream = TrafficoSign.class.getResourceAsStream(String.format(SIGN_DEFINITION_PATH, country))
    ) {
      if (countryStream == null) {
        return;
      }
      JsonObject countrySigns = Json.createReader(countryStream).readObject();
      Set<String> countrySignNames = countrySigns.keySet();
      Main.info(
          countrySignNames.size() + " different signs are supported for ''{0}'' by the Mapillary-plugin .",
          country
      );

      // TODO: Replace by an array when all types of sign elements (text!) are supported
      List<TrafficoSignElement> signLayers = new ArrayList<>();
      for (String name : countrySignNames) {
        signLayers.clear();
        JsonArray elements = countrySigns.getJsonObject(name).getJsonArray("elements");

        for (int i = 0; i < elements.size(); i++) {
          Character glyph = TrafficoGlyph.getGlyph(elements.getJsonObject(i).getString("type"));
          if (glyph != null) {
            Color c;
            switch (elements.getJsonObject(i).getString("color").toLowerCase()) {
              case "black":
                c = Color.BLACK;
                break;
              case "red":
                c = new Color(0xc1121c);
                break;
              case "blue":
                c = new Color(0x154889);
                break;
              case "white":
                c = Color.WHITE;
                break;
              case "green":
                c = new Color(0x008754);
                break;
              case "yellow":
                c = new Color(0xfecf33);
                break;
              default:
                c = Color.MAGENTA;
                break;
            }
            signLayers.add(new TrafficoSignElement(glyph, c));
          }
        }
        addSign(new TrafficoSign(signLayers.toArray(new TrafficoSignElement[signLayers.size()]), country, name));
      }
    } catch (IOException e) {
      Main.error(e);
    }
  }
}
