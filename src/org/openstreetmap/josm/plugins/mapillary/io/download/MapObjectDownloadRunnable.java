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
import javax.json.JsonException;
import javax.json.JsonReader;

import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.gui.Notification;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapObjectLayer;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapObjectLayer.STATUS;
import org.openstreetmap.josm.plugins.mapillary.model.MapObject;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL.APIv3;
import org.openstreetmap.josm.plugins.mapillary.utils.api.JsonDecoder;
import org.openstreetmap.josm.plugins.mapillary.utils.api.JsonMapObjectDecoder;
import org.openstreetmap.josm.tools.I18n;
import org.openstreetmap.josm.tools.ImageProvider.ImageSizes;
import org.openstreetmap.josm.tools.Logging;

public class MapObjectDownloadRunnable implements Runnable {
  private final Bounds bounds;
  private final MapObjectLayer layer;
  private static final Function<Bounds, URL> URL_GEN = APIv3::searchMapObjects;

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
    URL nextURL = URL_GEN.apply(bounds);
    try {
      while (nextURL != null && result.size() < MapillaryProperties.MAX_MAPOBJECTS.get() && !layer.isDownloadRunnableScheduled()) {
        final int prevResultSize = result.size();
        final long startTime = System.currentTimeMillis();
        final URLConnection con = nextURL.openConnection();
        try (JsonReader reader = Json.createReader(new BufferedInputStream(con.getInputStream()))) {
          result.addAll(JsonDecoder.decodeFeatureCollection(
            reader.readObject(),
            JsonMapObjectDecoder::decodeMapObject
          ));
        }
        layer.invalidate();
        BoundsDownloadRunnable.logConnectionInfo(con, String.format(
          "%d map objects in %.2f s",
          result.size() - prevResultSize,
          (System.currentTimeMillis() - startTime) / 1000f
        ));
        nextURL = APIv3.parseNextFromLinkHeaderValue(con.getHeaderField("Link"));
      }
    } catch (IOException | JsonException e) {
      String message = I18n.tr("{0}\nCould not read map objects from URL\n{1}!", e.getLocalizedMessage(), nextURL.toString());
      Logging.log(Logging.LEVEL_WARN, message, e);
      new Notification(message)
        .setIcon(MapillaryPlugin.LOGO.setSize(ImageSizes.LARGEICON).get())
        .setDuration(Notification.TIME_LONG)
        .show();
      layer.setStatus(STATUS.FAILED);
      layer.finishDownload(false);
      return;
    }
    if (nextURL == null) {
      layer.setStatus(STATUS.COMPLETE);
    } else {
      layer.setStatus(result.size() >= MapillaryProperties.MAX_MAPOBJECTS.get() ? STATUS.INCOMPLETE : STATUS.DOWNLOADING);
    }
    layer.finishDownload(nextURL == null || result.size() >= MapillaryProperties.MAX_MAPOBJECTS.get());
    try {
      Thread.sleep(1000); // Buffer between downloads to avoid too many downloads when e.g. panning around
    } catch (InterruptedException e) {
      Logging.debug(e);
      Thread.currentThread().interrupt();
    }
  }

}
