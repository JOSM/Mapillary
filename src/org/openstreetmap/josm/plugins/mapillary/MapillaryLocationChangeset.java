// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary;

import java.util.AbstractSet;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryChangesetListener;

public class MapillaryLocationChangeset extends AbstractSet<MapillaryImage> {
  private final Set<MapillaryChangesetListener> listeners = new HashSet<>();
  private final Set<MapillaryImage> changeset = Collections.newSetFromMap(new ConcurrentHashMap<>());

  public void addChangesetListener(MapillaryChangesetListener listener) {
    this.listeners.add(listener);
  }

  public void cleanChangeset() {
    changeset.clear();
    fireListeners();
  }

  private void fireListeners() {
    listeners.forEach(MapillaryChangesetListener::changesetChanged);
  }

  @Override
  public boolean add(MapillaryImage image) {
    boolean add = changeset.add(image);
    fireListeners();
    return add;
  }

  @Override
  public Iterator<MapillaryImage> iterator() {
    return changeset.iterator();
  }

  @Override
  public int size() {
    return changeset.size();
  }

  @Override
  public boolean remove(Object image) {
    boolean remove = changeset.remove(image);
    fireListeners();
    return remove;
  }
}
