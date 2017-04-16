// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.model;

import java.io.IOException;
import java.net.URL;
import java.util.function.Function;

import javax.imageio.ImageIO;
import javax.swing.ImageIcon;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.plugins.mapillary.cache.Caches.MapObjectIconCache;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL.MainWebsite;
import org.openstreetmap.josm.tools.ImageProvider;

public class MapObject {
  private static final ImageIcon ICON_UNKNOWN_TYPE = ImageProvider.get("unknown-mapobject-type");
  private static Function<String, URL> iconUrlGen = MainWebsite::mapObjectIcon;

  private final LatLon coordinate;
  private final String key;
  private final String objPackage;
  private final String value;
  private final long firstSeenTime;
  private final long lastSeenTime;
  private final long updatedTime;

  public MapObject(
    final LatLon coordinate, final String key, final String objPackage, final String value,
    long firstSeenTime, long lastSeenTime, long updatedTime
  ) {
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

  /**
   * @param objectTypeID the {@link String} representing the type of map object. This ID can be retrieved via
   *   {@link #getValue()} for any given {@link MapObject}.
   * @return the icon, which represents the given objectTypeID
   */
  public static ImageIcon getIcon(final String objectTypeID) {
    final ImageIcon cachedIcon = MapObjectIconCache.getInstance().get(objectTypeID);
    if ("not-in-set".equals(objectTypeID)) {
      return ICON_UNKNOWN_TYPE;
    } else if (cachedIcon == null) {
      try {
        final ImageIcon downloadedIcon = new ImageIcon(ImageIO.read(iconUrlGen.apply(objectTypeID)));
        MapObjectIconCache.getInstance().put(objectTypeID, downloadedIcon);
        return downloadedIcon;
      } catch (IOException e) {
        Main.warn(e, "Failed to download icon " + objectTypeID);
        return ICON_UNKNOWN_TYPE;
      }
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
