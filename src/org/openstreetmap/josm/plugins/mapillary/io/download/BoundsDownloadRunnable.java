// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.io.download;

import java.io.IOException;
import java.net.URL;
import java.net.URLConnection;
import java.util.function.Function;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.gui.Notification;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL.APIv3;
import org.openstreetmap.josm.tools.I18n;
import org.openstreetmap.josm.tools.ImageProvider.ImageSizes;

public abstract class BoundsDownloadRunnable implements Runnable {

  protected Bounds bounds;

  public BoundsDownloadRunnable(final Bounds bounds) {
    this.bounds = bounds;
  }

  protected abstract Function<Bounds, URL> getUrlGenerator();

  @Override
  public void run() {
    URL nextURL = getUrlGenerator().apply(bounds);
    try {
      while (nextURL != null) {
        final URLConnection con = nextURL.openConnection();
        run(con);
        nextURL = APIv3.parseNextFromLinkHeaderValue(con.getHeaderField("Link"));
      }
    } catch (IOException e) {
      String message = I18n.tr("Could not read from URL {0}!", nextURL.toString());
      Main.warn(e, message);
      new Notification(message)
        .setIcon(MapillaryPlugin.LOGO.setSize(ImageSizes.LARGEICON).get())
        .setDuration(Notification.TIME_LONG)
        .show();
    }
  }

  public abstract void run(final URLConnection connection) throws IOException;

}
