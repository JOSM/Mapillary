// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.data.image;

import java.util.Collection;
import java.util.Map;

import org.openstreetmap.josm.data.osm.Tagged;

/**
 *
 */
public interface MapillaryTagged extends Tagged {

  /**
   * Get the tag map for manual editing. Should only be used by the {@link MapillaryTagged} interface methods.
   *
   * @return The tagmap.
   */
  Map<String, String> getTagMap();

  /* Begin Tagged implementation */
  @Override
  default void put(String key, String value) {
    if (value != null && !value.trim().isEmpty())
      getTagMap().put(key, value);
    else
      getTagMap().remove(key);
  }

  @Override
  default String get(String key) {
    return getTagMap().get(key);
  }

  @Override
  default boolean hasKey(String key) {
    return getTagMap().containsKey(key);
  }

  @Override
  default void setKeys(Map<String, String> keys) {
    getTagMap().clear();
    getTagMap().putAll(keys);
  }

  @Override
  default Map<String, String> getKeys() {
    return getTagMap();
  }

  @Override
  default void remove(String key) {
    getTagMap().remove(key);
  }

  @Override
  default boolean hasKeys() {
    return !getTagMap().isEmpty();
  }

  @Override
  default Collection<String> keySet() {
    return getTagMap().keySet();
  }

  @Override
  default int getNumKeys() {
    return getTagMap().size();
  }

  @Override
  default void removeAll() {
    getTagMap().clear();
  }
  /* End Tagged implementation */
}
