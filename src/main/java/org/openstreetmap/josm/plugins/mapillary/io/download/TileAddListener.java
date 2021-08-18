package org.openstreetmap.josm.plugins.mapillary.io.download;

import org.openstreetmap.gui.jmapviewer.Tile;

/**
 * Listen for tile add events
 */
public interface TileAddListener<T extends Tile> {
    /**
     * Called when a tile is added
     *
     * @param tile The tile added
     */
    void tileAdded(T tile);
}
