// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.io.download;

import org.openstreetmap.gui.jmapviewer.Tile;

/**
 * Listen for tile add events
 * @param <T> The tile type that we are listening for
 */
public interface TileAddListener<T extends Tile> {
    /**
     * Called when a tile is added
     *
     * @param tile The tile added
     */
    void tileAdded(T tile);
}
