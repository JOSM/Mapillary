// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.io.download;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.net.URL;
import java.net.URLConnection;
import java.util.function.Function;

import javax.json.Json;
import javax.json.JsonReader;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.plugins.mapillary.MapillaryData;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryMainDialog;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL.APIv3;
import org.openstreetmap.josm.plugins.mapillary.utils.api.JsonImageDetailsDecoder;

public class ImageDetailsDownloadRunnable extends BoundsDownloadRunnable {
  private static final Function<Bounds, URL> URL_GEN = APIv3::searchImages;

  private final MapillaryData data;

  public ImageDetailsDownloadRunnable(final MapillaryData data, final Bounds bounds) {
    super(bounds);
    this.data = data;
  }

  @Override
  public void run(final URLConnection con) throws IOException {
    Main.info("Download image details from " + con.getURL());
    try (JsonReader reader = Json.createReader(new BufferedInputStream(con.getInputStream()))) {
      JsonImageDetailsDecoder.decodeImageInfos(reader.readObject(), data);
      MapillaryMainDialog.getInstance().updateTitle();
    }
    Main.info("Finished downloading image details");
  }

  @Override
  protected Function<Bounds, URL> getUrlGenerator() {
    return URL_GEN;
  }

}
