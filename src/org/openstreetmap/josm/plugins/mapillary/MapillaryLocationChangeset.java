// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary;

import java.util.HashSet;
import java.util.Set;

import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryChangesetListener;

public class MapillaryLocationChangeset extends HashSet<MapillaryImage> {
  private static final long serialVersionUID = 2461033584553885626L;
  private final Set<MapillaryChangesetListener> listeners = new HashSet<>();

  public void addChangesetListener(MapillaryChangesetListener listener) {
    this.listeners.add(listener);
  }

  public void cleanChangeset() {
    this.clear();
    fireListeners();
  }

  private void fireListeners() {
    for (MapillaryChangesetListener listener : listeners) {
      listener.changesetChanged();
    }
  }

  @Override
  public boolean add(MapillaryImage image) {
    boolean add = super.add(image);
    fireListeners();
    return add;
  }

  @Override
  public boolean remove(Object image) {
    boolean remove = super.remove(image);
    fireListeners();
    return remove;
  }

}
