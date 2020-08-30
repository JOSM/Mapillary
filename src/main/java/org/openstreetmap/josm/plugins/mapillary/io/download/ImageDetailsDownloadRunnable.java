// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.io.download;

import java.io.IOException;
import java.net.URL;
import java.util.Collection;
import java.util.Collections;
import java.util.function.Function;

import javax.json.Json;
import javax.json.JsonException;
import javax.json.JsonReader;

import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.gui.progress.NullProgressMonitor;
import org.openstreetmap.josm.gui.progress.ProgressMonitor;
import org.openstreetmap.josm.plugins.mapillary.MapillaryData;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryMainDialog;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL.APIv3;
import org.openstreetmap.josm.plugins.mapillary.utils.api.JsonImageDetailsDecoder;
import org.openstreetmap.josm.tools.HttpClient;

public class ImageDetailsDownloadRunnable extends BoundsDownloadRunnable {
  private static final long serialVersionUID = -4669402516726758428L;

  private static final Function<Bounds, Collection<URL>> URL_GEN = APIv3::searchImages;

  private final MapillaryData data;

  public ImageDetailsDownloadRunnable(final MapillaryData data, final Bounds bounds, ProgressMonitor monitor) {
    super(bounds, monitor);
    this.data = data;
  }

  public ImageDetailsDownloadRunnable(final MapillaryData data, final Bounds bounds, URL url, ProgressMonitor monitor) {
    super(bounds, Collections.singleton(url), monitor);
    this.data = data;
  }

  @Override
  public BoundsDownloadRunnable getNextUrl(URL nextUrl) {
    // Use NullProgressMonitor to avoid cancelled downloads...
    return new ImageDetailsDownloadRunnable(data, bounds, nextUrl, NullProgressMonitor.INSTANCE);
  }

  @Override
  public void compute() {
    super.run();
  }

  @Override
  public void run(final HttpClient client) throws IOException {
    try (JsonReader reader = Json.createReader(client.getResponse().getContent())) {
      JsonImageDetailsDecoder.decodeImageInfos(reader.readObject(), data);
      logConnectionInfo(client, null);
      MapillaryMainDialog.getInstance().updateTitle();
      completed = Boolean.TRUE;
    } catch (JsonException | NumberFormatException e) {
      throw new IOException(e);
    }
  }

  @Override
  protected Function<Bounds, Collection<URL>> getUrlGenerator() {
    return URL_GEN;
  }

}
