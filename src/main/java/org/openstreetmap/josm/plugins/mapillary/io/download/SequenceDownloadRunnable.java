// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.io.download;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.net.URL;
import java.net.URLConnection;
import java.util.Collection;
import java.util.function.Function;

import javax.json.Json;
import javax.json.JsonException;
import javax.json.JsonReader;

import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryData;
import org.openstreetmap.josm.plugins.mapillary.MapillarySequence;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL.APIv3;
import org.openstreetmap.josm.plugins.mapillary.utils.api.JsonDecoder;
import org.openstreetmap.josm.plugins.mapillary.utils.api.JsonSequencesDecoder;

public final class SequenceDownloadRunnable extends BoundsDownloadRunnable {
  private final MapillaryData data;
  private static final Function<Bounds, URL> URL_GEN = APIv3::searchSequences;

  public SequenceDownloadRunnable(final MapillaryData data, final Bounds bounds) {
    super(bounds);
    this.data = data;
  }

  @Override
  public void run(final URLConnection con) throws IOException {
    if (Thread.interrupted()) {
      return;
    }
    try (JsonReader reader = Json.createReader(new BufferedInputStream(con.getInputStream()))) {
      final long startTime = System.currentTimeMillis();
      final Collection<MapillarySequence> sequences = JsonDecoder.decodeFeatureCollection(
        reader.readObject(),
        JsonSequencesDecoder::decodeSequence
      );
      logConnectionInfo(con, String.format("%d sequences in %.2f s", sequences.size(), (System.currentTimeMillis() - startTime) / 1000F));
      if (Thread.interrupted()) {
        return;
      }
      for (MapillarySequence seq : sequences) {
        if (MapillaryProperties.CUT_OFF_SEQUENCES_AT_BOUNDS.get()) {
          for (MapillaryAbstractImage img : seq.getImages()) {
            if (bounds.contains(img.getLatLon())) {
              data.add(img);
            } else {
              seq.remove(img);
            }
          }
        } else {
          boolean sequenceCrossesThroughBounds = false;
          for (int i = 0; i < seq.getImages().size() && !sequenceCrossesThroughBounds; i++) {
            sequenceCrossesThroughBounds = bounds.contains(seq.getImages().get(i).getLatLon());
          }
          if (sequenceCrossesThroughBounds) {
            data.addAll(seq.getImages(), true);
          }
        }
      }
    } catch (JsonException | NumberFormatException e) {
      throw new IOException(e);
    }

  }

  @Override
  protected Function<Bounds, URL> getUrlGenerator() {
    return URL_GEN;
  }
}
