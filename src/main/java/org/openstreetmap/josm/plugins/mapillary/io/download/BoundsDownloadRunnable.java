// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.io.download;

import java.awt.GraphicsEnvironment;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLConnection;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.ForkJoinTask;
import java.util.concurrent.RecursiveAction;
import java.util.function.Function;

import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.gui.Notification;
import org.openstreetmap.josm.gui.progress.AbstractProgressMonitor;
import org.openstreetmap.josm.gui.progress.ChildProgress;
import org.openstreetmap.josm.gui.progress.NullProgressMonitor;
import org.openstreetmap.josm.gui.progress.ProgressMonitor;
import org.openstreetmap.josm.gui.util.GuiHelper;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.gui.DownloadProgressMonitor;
import org.openstreetmap.josm.plugins.mapillary.oauth.MapillaryUser;
import org.openstreetmap.josm.plugins.mapillary.oauth.OAuthUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL.APIv3;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;
import org.openstreetmap.josm.tools.HttpClient;
import org.openstreetmap.josm.tools.I18n;
import org.openstreetmap.josm.tools.ImageProvider.ImageSizes;
import org.openstreetmap.josm.tools.Logging;

public abstract class BoundsDownloadRunnable extends RecursiveAction {

  private static final long serialVersionUID = -3097850570397160069L;
  private static final short MAX_DOWNLOAD_ATTEMPTS = 2;
  protected final Bounds bounds;
  protected final Collection<URL> urls;
  protected static final int MAXIMUM_URLS = 50;
  private ProgressMonitor monitor;
  private final Function<Bounds, Collection<URL>> urlGen;
  private BoundsDownloadRunnable next;
  /**
   * Checks if this download has been completed before
   */
  protected boolean completed = Boolean.FALSE;

  protected BoundsDownloadRunnable(final Bounds bounds, final Function<Bounds, Collection<URL>> urlGen,
    ProgressMonitor monitor) {
    this(bounds, urlGen, null, monitor);
  }

  protected BoundsDownloadRunnable(final Bounds bounds, final Function<Bounds, Collection<URL>> urlGen,
    Collection<URL> urls, ProgressMonitor monitor) {
    this.bounds = bounds;
    this.urlGen = urlGen;
    this.urls = urls == null ? getUrlGenerator().apply(bounds) : urls;
    this.monitor = monitor;
  }

  public void run() {
    ForkJoinPool pool = ForkJoinTask.getPool();
    if (pool == null) {
      pool = MapillaryUtils.getForkJoinPool(this.getClass());
    }
    if (!completed) {
      for (URL url : urls) {
        pool.execute(() -> realRun(url));
      }
    }
  }

  protected Function<Bounds, Collection<URL>> getUrlGenerator() {
    return this.urlGen;
  }

  private void realRun(URL currentUrl) {
    realRun(currentUrl, 0);
  }

  private void realRun(URL currentUrl, int attempt) {
    HttpClient client = null;
    ForkJoinPool pool = ForkJoinTask.getPool();
    try {
      if (Thread.interrupted()) {
        Logging.debug("{} for {} interrupted!", getClass().getSimpleName(), bounds.toString());
        return;
      }
      client = HttpClient.create(currentUrl);
      client.setReadTimeout(MapillaryProperties.TIMEOUT.get());
      client.setHeader("Accept-Encoding", null); // compression is broken as of 2020-03-03
      if (MapillaryUser.getUsername() != null) {
        OAuthUtils.addAuthenticationHeader(client);
      }
      if (this.isCancelled()) {
        return;
      }
      // HttpClient sets the state to FINISHED, and the subsequent calls to getResponse().getBufferedReader() error out.
      HttpClient.Response response = client.connect(NullProgressMonitor.INSTANCE);
      monitor.beginTask("Download data");
      if (monitor instanceof ChildProgress) {
        AbstractProgressMonitor parentMonitor = ((ChildProgress) monitor).getParent();
        if (parentMonitor instanceof DownloadProgressMonitor) {
          DownloadProgressMonitor parentDownloadMonitor = (DownloadProgressMonitor) parentMonitor;
          parentDownloadMonitor.setSize(response.getContentLength());
        }
      }
      URL nextURL = APIv3.parseNextFromLinkHeaderValue(response.getHeaderField("Link"));
      if (nextURL != null && pool != null && !this.isCancelled()) {
        next = getNextUrl(nextURL);
        if (monitor.isCanceled() || next == null || pool.isShutdown()) {
          pool.shutdown();
        } else {
          pool.execute(next);
        }
      }
      if (!this.isCancelled()) {
        run(client);
      }
      if (monitor instanceof ChildProgress) {
        AbstractProgressMonitor parentMonitor = ((ChildProgress) monitor).getParent();
        if (parentMonitor instanceof DownloadProgressMonitor) {
          DownloadProgressMonitor parentDownloadMonitor = (DownloadProgressMonitor) parentMonitor;
          parentDownloadMonitor.updateCompleted();
        }
      }
      monitor.finishTask();
    } catch (IOException e) {
      // Finish the task and use the nullprogressmonitor instance for future attempts.
      monitor.finishTask();
      this.monitor = NullProgressMonitor.INSTANCE;
      final String message;
      // Limit retries to 504 server timeouts and no more than {@link MAX_DOWNLOAD_ATTEMPTS} (2 attempts).
      if (client.getResponse() != null && client.getResponse().getResponseCode() == 504
        && attempt < MAX_DOWNLOAD_ATTEMPTS && pool != null) {
        message = I18n.tr("Server timeout, trying {0} again (attempt {1} of {2})", currentUrl.toString(), attempt + 1,
          MAX_DOWNLOAD_ATTEMPTS);
        if (next != null) {
          next.cancel(true);
        }
        if (!this.isCancelled()) {
          pool.execute(() -> this.realRun(currentUrl, attempt + 1));
        }
      } else {
        message = I18n.tr("Could not read from URL {0}!", currentUrl.toString());
      }
      client.disconnect();
      Logging.warn(message);
      Logging.warn(e);
      if (!GraphicsEnvironment.isHeadless()) {
        GuiHelper.runInEDT(() -> showNotification(message));
      }
    }
  }

  @Override
  public boolean cancel(boolean mayInterruptIfRunning) {
    boolean canceled = super.cancel(mayInterruptIfRunning);
    if (mayInterruptIfRunning && this.next != null) {
      canceled = canceled && this.next.cancel(mayInterruptIfRunning);
    }
    return canceled;
  }

  protected static void showNotification(String message) {
    new Notification(message).setIcon(MapillaryPlugin.LOGO.setSize(ImageSizes.LARGEICON).get())
      .setDuration(Notification.TIME_SHORT).show();
  }

  /**
   * Logs information about the given connection via {@link Logging#info(String)}. If it's a {@link HttpURLConnection},
   * the request method, the response code and the URL itself are logged. Otherwise only the URL is logged.
   *
   * @param client the {@link URLConnection} for which information is logged
   * @param info an additional info text, which is appended to the output in braces
   */
  public static void logConnectionInfo(final HttpClient client, final String info) {
    final StringBuilder message = new StringBuilder(client.getRequestMethod()).append(' ').append(client.getURL())
      .append(" → ").append(client.getResponse().getResponseCode());

    if (info != null && info.length() >= 1) {
      message.append(" (").append(info).append(')');
    }
    Logging.info(message.toString());
  }

  public abstract void run(final HttpClient client) throws IOException;

  public abstract BoundsDownloadRunnable getNextUrl(final URL nextUrl);

  /**
   * Split the collection of URLs into reasonably sized chunks
   *
   * @return The URLs split into chunks for fork/join tasks.
   */
  protected List<Collection<URL>> splitUrls() {
    List<Collection<URL>> toExecute = new ArrayList<>();
    Collection<URL> collection = new ArrayList<>();
    toExecute.add(collection);
    for (URL url : urls) {
      if (!collection.isEmpty() && collection.size() % MAXIMUM_URLS == 0) {
        collection = new ArrayList<>();
        toExecute.add(collection);
      }
      collection.add(url);
    }
    return toExecute;
  }
}
