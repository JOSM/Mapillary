// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.io.download;

import java.io.BufferedReader;
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
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryData;
import org.openstreetmap.josm.plugins.mapillary.MapillarySequence;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL.APIv3;
import org.openstreetmap.josm.plugins.mapillary.utils.api.JsonDecoder;
import org.openstreetmap.josm.plugins.mapillary.utils.api.JsonSequencesDecoder;
import org.openstreetmap.josm.tools.HttpClient;

public final class SequenceDownloadRunnable extends BoundsDownloadRunnable {
  private static final long serialVersionUID = -2975246177315340212L;
  private final MapillaryData data;
  private static final Function<Bounds, Collection<URL>> URL_GEN = APIv3::searchSequences;

  public SequenceDownloadRunnable(final MapillaryData data, final Bounds bounds, ProgressMonitor monitor) {
    super(bounds, monitor);
    this.data = data;
  }

  public SequenceDownloadRunnable(MapillaryData data, Bounds bounds, URL url, ProgressMonitor monitor) {
    super(bounds, Collections.singleton(url), monitor);
    this.data = data;
  }

  @Override
  public BoundsDownloadRunnable getNextUrl(URL nextUrl) {
    // Use NullProgressMonitor to avoid cancelled downloads...
    return new SequenceDownloadRunnable(data, bounds, nextUrl, NullProgressMonitor.INSTANCE);
  }

  @Override
  public void compute() {
    super.run();
  }

  @Override
  public void run(final HttpClient client) throws IOException {
    if (Thread.interrupted()) {
      return;
    }
    try (BufferedReader content = client.getResponse().getContentReader();
      JsonReader reader = Json.createReader(content)) {
      final long startTime = System.currentTimeMillis();
      final Collection<MapillarySequence> sequences = JsonDecoder
        .decodeFeatureCollection(reader.readObject(), JsonSequencesDecoder::decodeSequence);
      logConnectionInfo(
        client,
        String.format("%d sequences in %.2f s", sequences.size(), (System.currentTimeMillis() - startTime) / 1000F));
      if (Thread.interrupted()) {
        return;
      }
      for (MapillarySequence seq : sequences) {
        if (Boolean.TRUE.equals(MapillaryProperties.CUT_OFF_SEQUENCES_AT_BOUNDS.get())) {
          for (MapillaryAbstractImage img : seq.getImages()) {
            if (bounds.isCollapsed() || bounds.isOutOfTheWorld() || bounds.contains(img.getLatLon())) {
              data.add(img);
            } else {
              seq.remove(img);
            }
          }
        } else {
          if (seq.getImages().stream().anyMatch(image -> bounds.contains(image.getLatLon()))) {
            data.addAll(seq.getImages(), true);
          }
        }
      }
      completed = Boolean.TRUE;
    } catch (JsonException | NumberFormatException e) {
      throw new IOException(e);
    } finally {
      MapillaryLayer.getInstance().invalidate();
    }

  }

  @Override
  protected Function<Bounds, Collection<URL>> getUrlGenerator() {
    return URL_GEN;
  }
}
