// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.io.download;

import java.io.BufferedReader;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.function.Function;
import java.util.stream.Collectors;

import javax.json.Json;
import javax.json.JsonException;
import javax.json.JsonReader;

import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.data.DataSource;
import org.openstreetmap.josm.data.coor.LatLon;
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
    super(bounds, URL_GEN, monitor);
    this.data = data;
  }

  public SequenceDownloadRunnable(MapillaryData data, Bounds bounds, URL url, ProgressMonitor monitor) {
    super(bounds, URL_GEN, Collections.singleton(url), monitor);
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
      final Collection<MapillarySequence> sequences = JsonDecoder.decodeFeatureCollection(reader.readObject(),
        JsonSequencesDecoder::decodeSequence);
      logConnectionInfo(client,
        String.format("%d sequences in %.2f s", sequences.size(), (System.currentTimeMillis() - startTime) / 1000F));
      if (Thread.interrupted()) {
        return;
      }
      for (MapillarySequence seq : sequences) {
        if (Boolean.TRUE.equals(MapillaryProperties.CUT_OFF_SEQUENCES_AT_BOUNDS.get())) {
          Collection<Bounds> boundsCollection = new ArrayList<>();
          boundsCollection.add(bounds);
          if (MapillaryLayer.hasInstance()) {
            boundsCollection.addAll(MapillaryLayer.getInstance().getData().getBounds());
          }
          boundsCollection = boundsCollection.stream().filter(b -> !b.isCollapsed() && !b.isOutOfTheWorld())
            .collect(Collectors.toSet());
          for (MapillaryAbstractImage img : seq.getImages()) {
            LatLon ll = img.getLatLon();
            if (boundsCollection.isEmpty() || boundsCollection.stream().anyMatch(b -> b.contains(ll))) {
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
      data.addDataSource(new DataSource(bounds, "Mapillary"));
    } catch (JsonException | NumberFormatException e) {
      throw new IOException(e);
    } finally {
      MapillaryLayer.getInstance().invalidate();
    }
  }
}
