// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.io.download;

import java.awt.GraphicsEnvironment;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLConnection;
import java.util.Collection;
import java.util.function.Function;

import javax.swing.SwingUtilities;

import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.gui.Notification;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.oauth.MapillaryUser;
import org.openstreetmap.josm.plugins.mapillary.oauth.OAuthUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL.APIv3;
import org.openstreetmap.josm.tools.I18n;
import org.openstreetmap.josm.tools.ImageProvider.ImageSizes;
import org.openstreetmap.josm.tools.Logging;

public abstract class BoundsDownloadRunnable implements Runnable {

  protected final Bounds bounds;

  protected abstract Function<Bounds, Collection<URL>> getUrlGenerator();

  public BoundsDownloadRunnable(final Bounds bounds) {
    this.bounds = bounds;
  }

  @Override
  public void run() {
    Collection<URL> urls = getUrlGenerator().apply(bounds);
    for (URL url : urls) {
      realRun(url);
    }
  }

  private void realRun(URL nextURL) {
    try {
      while (nextURL != null) {
        if (Thread.interrupted()) {
          Logging.debug("{} for {} interrupted!", getClass().getSimpleName(), bounds.toString());
          return;
        }
        final URLConnection con = nextURL.openConnection();
        if (MapillaryUser.getUsername() != null)
          OAuthUtils.addAuthenticationHeader(con);
        run(con);
        nextURL = APIv3.parseNextFromLinkHeaderValue(con.getHeaderField("Link"));
      }
    } catch (IOException e) {
      String message = I18n.tr("Could not read from URL {0}!", nextURL.toString());
      Logging.log(Logging.LEVEL_WARN, message, e);
      if (!GraphicsEnvironment.isHeadless()) {
        if (SwingUtilities.isEventDispatchThread()) {
          showNotification(message);
        } else {
          SwingUtilities.invokeLater(() -> showNotification(message));
        }
      }
    }
  }

  private static void showNotification(String message) {
    new Notification(message).setIcon(MapillaryPlugin.LOGO.setSize(ImageSizes.LARGEICON).get())
        .setDuration(Notification.TIME_LONG).show();
  }

  /**
   * Logs information about the given connection via {@link Logging#info(String)}.
   * If it's a {@link HttpURLConnection}, the request method, the response code and the URL itself are logged.
   * Otherwise only the URL is logged.
   * @param con the {@link URLConnection} for which information is logged
   * @param info an additional info text, which is appended to the output in braces
   * @throws IOException if {@link HttpURLConnection#getResponseCode()} throws an {@link IOException}
   */
  public static void logConnectionInfo(final URLConnection con, final String info) throws IOException {
    final StringBuilder message;
    if (con instanceof HttpURLConnection) {
      message = new StringBuilder(((HttpURLConnection) con).getRequestMethod())
        .append(' ').append(con.getURL())
        .append(" → ").append(((HttpURLConnection) con).getResponseCode());
    } else {
      message = new StringBuilder("Download from ").append(con.getURL());
    }
    if (info != null && info.length() >= 1) {
      message.append(" (").append(info).append(')');
    }
    Logging.info(message.toString());
  }

  public abstract void run(final URLConnection connection) throws IOException;
}
