// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.io.download;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.Collection;
import java.util.Collections;
import java.util.function.Function;

import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.data.osm.DataSet;
import org.openstreetmap.josm.gui.progress.NullProgressMonitor;
import org.openstreetmap.josm.gui.progress.ProgressMonitor;
import org.openstreetmap.josm.io.GeoJSONReader;
import org.openstreetmap.josm.io.IllegalDataException;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.PointObjectLayer;
import org.openstreetmap.josm.plugins.mapillary.oauth.MapillaryUser;
import org.openstreetmap.josm.plugins.mapillary.oauth.OAuthUtils;
import org.openstreetmap.josm.tools.HttpClient;
import org.openstreetmap.josm.tools.Logging;

/**
 *
 */
public class ObjectDetectionsDownloadRunnable extends BoundsDownloadRunnable {
  private static final String DETECTIONS = "detections";

  private final DataSet data;

  public ObjectDetectionsDownloadRunnable(DataSet data, Bounds bounds, Function<Bounds, Collection<URL>> urlGen,
    ProgressMonitor monitor) {
    super(bounds, urlGen, monitor);
    this.data = data;
  }

  public ObjectDetectionsDownloadRunnable(DataSet data, Bounds bounds, Function<Bounds, Collection<URL>> urlGen,
    URL url, ProgressMonitor monitor) {
    super(bounds, urlGen, Collections.singleton(url), monitor);
    this.data = data;
  }

  @Override
  public void run(HttpClient client) throws IOException {
    if (MapillaryUser.getUsername() != null)
      OAuthUtils.addAuthenticationHeader(client);
    client.connect();
    try (InputStream stream = client.getResponse().getContent()) {
      DataSet ds = GeoJSONReader.parseDataSet(stream, NullProgressMonitor.INSTANCE);
      ds.allPrimitives().parallelStream().filter(p -> p.hasKey(DETECTIONS))
        .forEach(p -> p.put("detections_num", Integer.toString(p.get(DETECTIONS).split("detection_key").length)));
      ds.allPrimitives().forEach(p -> p.setModified(false));
      synchronized (PointObjectLayer.class) {
        data.mergeFrom(ds);
      }
    } catch (IllegalDataException e) {
      Logging.error(e);
      showNotification(tr("Bad data for URL: {0}", client.getURL().toExternalForm()));
    }
  }

  @Override
  public BoundsDownloadRunnable getNextUrl(URL nextUrl) {
    return new ObjectDetectionsDownloadRunnable(data, bounds, this.getUrlGenerator(), nextUrl,
      NullProgressMonitor.INSTANCE);
  }

  @Override
  protected void compute() {
    super.run();
  }

}
