// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.io.download;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.net.URL;
import java.net.URLConnection;
import java.util.Collection;
import java.util.HashSet;
import java.util.function.Function;

import javax.json.Json;
import javax.json.JsonReader;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.gui.Notification;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapObjectLayer;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapObjectLayer.STATUS;
import org.openstreetmap.josm.plugins.mapillary.objects.MapObject;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL.APIv3;
import org.openstreetmap.josm.plugins.mapillary.utils.api.JsonDecoder;
import org.openstreetmap.josm.plugins.mapillary.utils.api.JsonMapObjectDecoder;
import org.openstreetmap.josm.tools.I18n;
import org.openstreetmap.josm.tools.ImageProvider.ImageSizes;

public class MapObjectDownloadRunnable implements Runnable {
  private final Bounds bounds;
  private final MapObjectLayer layer;
  private static final Function<Bounds, URL> urlGen = APIv3::searchMapObjects;

  private final Collection<MapObject> result = new HashSet<>();

  public MapObjectDownloadRunnable(final MapObjectLayer layer, final Bounds bounds) {
    this.bounds = bounds;
    this.layer = layer;
  }

  public Collection<MapObject> getMapObjects() {
    return result;
  }

  @Override
  public void run() {
    layer.setStatus(MapObjectLayer.STATUS.DOWNLOADING);
    URL nextURL = urlGen.apply(bounds);
    try {
      while (nextURL != null && result.size() < MapillaryProperties.MAX_MAPOBJECTS.get() && !layer.isDownloadRunnableScheduled()) {
        Main.debug("Download map objects from " + nextURL);
        final URLConnection con = nextURL.openConnection();
        try (JsonReader reader = Json.createReader(new BufferedInputStream(con.getInputStream()))) {
          result.addAll(JsonDecoder.decodeFeatureCollection(
            reader.readObject(),
            JsonMapObjectDecoder::decodeMapObject
          ));
        }
        layer.invalidate();
        nextURL = APIv3.parseNextFromLinkHeaderValue(con.getHeaderField("Link"));
      }
    } catch (IOException e) {
      String message = I18n.tr("Could not read map objects from URL {0}!", nextURL.toString());
      Main.warn(e, message);
      new Notification(message)
        .setIcon(MapillaryPlugin.LOGO.setSize(ImageSizes.LARGEICON).get())
        .setDuration(Notification.TIME_LONG)
        .show();
      layer.setStatus(STATUS.FAILED);
      layer.finishDownload(false);
      return;
    }
    layer.setStatus(
      nextURL != null
        ? (result.size() >= MapillaryProperties.MAX_MAPOBJECTS.get() ? STATUS.INCOMPLETE : STATUS.DOWNLOADING)
        : STATUS.COMPLETE
    );
    layer.finishDownload(nextURL == null);
  }

}
