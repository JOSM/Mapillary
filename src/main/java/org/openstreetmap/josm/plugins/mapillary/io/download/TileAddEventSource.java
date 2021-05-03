package org.openstreetmap.josm.plugins.mapillary.io.download;

import org.openstreetmap.gui.jmapviewer.Tile;

/**
 * The source for {@link TileAddListener} events
 *
 * @param <T> The tile type
 */
public interface TileAddEventSource<T extends Tile> {
  /**
   * Add a listener
   *
   * @param listener The listener to add
   * @param <L> The listener type
   * @return {@code true} if the listener was added
   */
  <L extends TileAddListener<T>> boolean addListener(L listener);

  /**
   * Remove a listener
   *
   * @param listener The listener to remove
   * @param <L> The listener type
   * @return {@code true} if the listener was removed
   */
  <L extends TileAddListener<T>> boolean removeListener(L listener);
}
