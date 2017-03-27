// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.io.download;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.net.URL;
import java.net.URLConnection;
import java.util.Collection;
import java.util.function.Function;

import javax.json.Json;
import javax.json.JsonReader;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.gui.Notification;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryData;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.MapillarySequence;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL.APIv3;
import org.openstreetmap.josm.plugins.mapillary.utils.api.JsonDecoder;
import org.openstreetmap.josm.plugins.mapillary.utils.api.JsonSequencesDecoder;
import org.openstreetmap.josm.tools.I18n;
import org.openstreetmap.josm.tools.ImageProvider.ImageSizes;

public final class SequenceDownloadRunnable implements Runnable {
  private final Bounds bounds;
  private final MapillaryData dataStorage;
  private static final Function<Bounds, URL> urlGen = APIv3::searchSequences;

  public SequenceDownloadRunnable(final MapillaryData data, final Bounds bounds) {
    this.bounds = bounds;
    this.dataStorage = data;
  }

  @Override
  public void run() {
    URL nextURL = urlGen.apply(bounds);
    try {
      while (nextURL != null) {
        Main.debug("Download sequences from " + nextURL);
        final URLConnection con = nextURL.openConnection();
        try (JsonReader reader = Json.createReader(new BufferedInputStream(con.getInputStream()))) {
          Collection<MapillarySequence> sequences = JsonDecoder.decodeFeatureCollection(
            reader.readObject(),
            JsonSequencesDecoder::decodeSequence
          );
          for (MapillarySequence seq : sequences) {
            if (MapillaryProperties.CUT_OFF_SEQUENCES_AT_BOUNDS.get()) {
              for (MapillaryAbstractImage img : seq.getImages()) {
                if (bounds.contains(img.getLatLon())) {
                  dataStorage.add(img);
                } else {
                  seq.remove(img);
                }
              }
            } else {
              boolean sequenceCrossesThroughBounds = false;
              for (int i = 0; i < seq.getImages().size() && !sequenceCrossesThroughBounds; i++) {
                sequenceCrossesThroughBounds |= bounds.contains(seq.getImages().get(i).getLatLon());
              }
              if (sequenceCrossesThroughBounds) {
                dataStorage.addAll(seq.getImages(), true);
              }
            }
          }
        }
        nextURL = APIv3.parseNextFromLinkHeaderValue(con.getHeaderField("Link"));
      }
    } catch (IOException e) {
      String message = I18n.tr("Could not read sequences from URL {0}!", nextURL.toString());
      Main.warn(e, message);
      new Notification(message)
        .setIcon(MapillaryPlugin.LOGO.setSize(ImageSizes.LARGEICON).get())
        .setDuration(Notification.TIME_LONG)
        .show();
    }
  }
}
