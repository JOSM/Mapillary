// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.objects;

import java.io.IOException;
import java.net.URL;
import java.util.function.Function;

import javax.swing.Icon;
import javax.swing.ImageIcon;

import org.apache.commons.jcs.access.CacheAccess;
import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.data.cache.JCSCacheManager;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL.MainWebsite;

public class MapObject {
  private static final CacheAccess<String, Icon> MAP_OBJECT_ICON_CACHE;
  private static Function<String, URL> iconUrlGen = MainWebsite::mapObjectIcon;

  static {
    CacheAccess<String, Icon> o;
    try {
      o = JCSCacheManager.<String, Icon>getCache(
        "mapillaryObjectIcons", 100, 1000, MapillaryPlugin.getCacheDirectory().getPath()
      );
    } catch (IOException e) {
      Main.warn(e, "Could not initialize cache for map objects");
      o = null;
    }
    MAP_OBJECT_ICON_CACHE = o;
  }

  private final LatLon coordinate;
  private final String key;
  private final String objPackage;
  private final String value;
  private final long firstSeenTime;
  private final long lastSeenTime;
  private final long updatedTime;

  public MapObject(final LatLon coordinate, final String key, final String objPackage, final String value, long firstSeenTime, long lastSeenTime, long updatedTime) {
    if (key == null || objPackage == null || value == null || coordinate == null) {
      throw new IllegalArgumentException("The fields of a MapObject must not be null!");
    }
    this.coordinate = coordinate;
    this.key = key;
    this.objPackage = objPackage;
    this.value = value;
    this.firstSeenTime = firstSeenTime;
    this.lastSeenTime = lastSeenTime;
    this.updatedTime = updatedTime;
  }

  public LatLon getCoordinate() {
    return coordinate;
  }

  public Icon getIcon() {
    final Icon cachedIcon = MAP_OBJECT_ICON_CACHE != null ? MAP_OBJECT_ICON_CACHE.get(value) : null;
    if (cachedIcon == null) {
      final Icon downloadedIcon = new ImageIcon(iconUrlGen.apply(value));
      if (MAP_OBJECT_ICON_CACHE != null) {
        MAP_OBJECT_ICON_CACHE.put(value, downloadedIcon);
      }
      return downloadedIcon;
    }
    return cachedIcon;
  }

  public String getKey() {
    return key;
  }

  public String getPackage() {
    return objPackage;
  }

  public long getFirstSeenTime() {
    return firstSeenTime;
  }

  public long getLastSeenTime() {
    return lastSeenTime;
  }

  public long getUpdatedTime() {
    return updatedTime;
  }

  public String getValue() {
    return value;
  }

  /* (non-Javadoc)
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((key == null) ? 0 : key.hashCode());
    return result;
  }

  /* (non-Javadoc)
   * @see java.lang.Object#equals(java.lang.Object)
   */
  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (!(obj instanceof MapObject)) {
      return false;
    }
    MapObject other = (MapObject) obj;
    if (key == null) {
      if (other.key != null) {
        return false;
      }
    } else if (!key.equals(other.key)) {
      return false;
    }
    return true;
  }

}
