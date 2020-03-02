// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.model;

import java.io.IOException;
import java.net.URL;
import java.util.Arrays;
import java.util.function.Function;

import javax.imageio.ImageIO;
import javax.swing.ImageIcon;

import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.io.CachedFile;
import org.openstreetmap.josm.plugins.mapillary.cache.Caches.MapObjectIconCache;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL.MainWebsite;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.ImageProvider.ImageSizes;
import org.openstreetmap.josm.tools.Logging;

public class MapObject extends KeyIndexedObject {
  private static final ImageIcon ICON_UNKNOWN_TYPE = ImageProvider.get("unknown-mapobject-type");
  /** The icon for unknown unknown objects */
  public static final ImageIcon ICON_NULL_TYPE = ImageProvider.createBlankIcon(ImageSizes.LARGEICON);
  private static final Function<String, URL> iconUrlGen = MainWebsite::mapObjectIcon;

  private final LatLon coordinate;
  private final String layer;
  private final String value;
  private final long firstSeenTime;
  private final long lastSeenTime;

  public MapObject(
    final LatLon coordinate, final String key, final String layer, final String value, long firstSeenTime,
    long lastSeenTime
  ) {
    super(key);
    if (layer == null || value == null || coordinate == null) {
      throw new IllegalArgumentException("The fields of a MapObject must not be null!");
    }
    this.coordinate = coordinate;
    this.layer = layer;
    this.value = value;
    this.firstSeenTime = firstSeenTime;
    this.lastSeenTime = lastSeenTime;
  }

  public LatLon getCoordinate() {
    return coordinate;
  }

  /**
   * @param objectTypeID
   *          the {@link String} representing the type of map object. This ID can be retrieved via {@link #getValue()}
   *          for any given {@link MapObject}.
   * @return the icon, which represents the given objectTypeID
   */
  public static ImageIcon getIcon(final String objectTypeID) {
    final ImageIcon cachedIcon = MapObjectIconCache.getInstance().get(objectTypeID);
    if ("not-in-set".equals(objectTypeID)) {
      return ICON_UNKNOWN_TYPE;
    } else if (cachedIcon == null) {
      ImageIcon downloadedIcon = null;
      for (String directory : Arrays.asList("package_objects", "package_signs")) {
        try {
          downloadedIcon = ImageProvider.get("mapillary_sprite_source/" + directory, objectTypeID);
        } catch (RuntimeException e) {
          Logging.trace(e);
        }
        if (downloadedIcon != null)
          break;
      }
      if (downloadedIcon == null) {
        try (CachedFile image = new CachedFile(iconUrlGen.apply(objectTypeID).toExternalForm())) {
          Logging.error("Trying to get " + image.getName() + " " + objectTypeID);
          downloadedIcon = new ImageIcon(ImageIO.read(image.getInputStream()));
          Logging.warn("Downloaded icon. ID known to the icon list: " + objectTypeID);
        } catch (IOException e) {
          Logging.log(Logging.LEVEL_WARN, "Failed to download icon. ID unknown to the icon list: " + objectTypeID, e);
        }
      }
      if (downloadedIcon == null) {
        downloadedIcon = ICON_NULL_TYPE;
      }
      MapObjectIconCache.getInstance().put(objectTypeID, downloadedIcon);
      return downloadedIcon;
    }
    return cachedIcon;
  }

  public String getLayer() {
    return layer;
  }

  public long getFirstSeenTime() {
    return firstSeenTime;
  }

  public long getLastSeenTime() {
    return lastSeenTime;
  }

  public String getValue() {
    return value;
  }
}
